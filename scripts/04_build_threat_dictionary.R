source("scripts/00_setup.R")

# load data
species_profiles <- read.csv("data_processed/species_profiles.csv")
species_master <- read.csv("data_processed/freshwater_species_master.csv")

# create a list of relevant threat
threat_dictionary <- list(
  "Habitat Loss" = c("habitat loss", "habitat destruction", "habitat degradation"),
  "Urbanization" = c("urban", "urbanization", "impervious", "stormwater"),
  "Flow Alteration" = c("flow", "flow alteration", "water level", "hydrolog",
                        "discharge", "water regulation"),
  "Barriers" = c("dam", "dams", "barrier", "culvert", "fish passage"),
  "Habitat Fragmentation" = c("fragmentation", "fragmented", "habitat isolation",
                              "isolated populations", "connectivity"),
  "Sedimentation" = c("sediment", "sedimentation", "silt", "turbidity"),
  "Nutrient Enrichment" = c("nutrient","phosphorus","nitrogen","eutrophication"),
  "Contaminants" = c("contaminant","contamination","toxic","pollution","effluent"),
  "Runoff" = c("runoff","stormwater"),
  "Water Quality" = c("water quality","dissolved oxygen","oxygen depletion",
                      "poor water quality"),
  "Agriculture" = c("agriculture", "agricultural", "livestock"),
  "Invasive Species" = c("invasive", "non-native", "exotic species", "zebra mussel",
                         "quagga mussel", "round goby", "common carp", "sea lamprey"),
  "Climate Change" = c("climate", "warming", "temperature", "extreme weather",
                       "drought","flooding","changing precipitation"),
  "Harvest" = c("harvest", "overharvest", "overfishing", "fishing"),
  "Disease" = c("disease", "parasite", "pathogen"),
  "Wetland Loss" = c("wetland", "marsh", "swamp", "drainage", "wetland loss"),
  "Water Withdrawal" = c("water taking", "water withdrawal", "groundwater",
                         "irrigation", "water extraction"),
  "Shoreline Development" = c("shoreline","waterfront development",
                              "cottage development","bank stabilization"),
  "Channel Modification" = c("dredging","channelization","channel modification",
                             "straightening","realignment"),
  "Forestry" = c("forestry","logging","timber harvest","clearcut","clear-cut"),
  "Mining" = c("mining","mine","tailings","aggregate extraction","quarry"),
  "Transportation" = c("road","highway","crossing","bridge","transport corridor"),
  "Hybridization" = c("hybridization","hybrid","genetic introgression"),
  "Predation" = c("predation","predator","predators"),
  "Competition" = c("competition", "competitive exclusion","competitor"),
  "Recreation" = c("recreational","boating","off-road vehicle","atv",
                   "human disturbance"),
  "Aquaculture" = c("aquaculture", "fish farming", "fish farm"),
  "Small Population Size" = c("small population","low abundance",
                              "demographic stochasticity","genetic drift",
                              "inbreeding"),
  "Knowledge Gaps" = c("unknown","poorly understood","data deficient","lack of information"),
  "Residential and Commercial Development" = c("development","residential",
                                               "commercial","housing","subdivision"),
  "Riparian Degradation" = c("riparian","stream bank","bank erosion",
                             "vegetation removal","loss of vegetation"),
  "Incidental Capture" = c("bycatch","incidental capture"),
  "Aquatic Vegetation Loss" = c("aquatic vegetation", "submerged vegetation",
                                "macrophyte","vegetation removal"),
  "Unclassified Threat" = c())

# create a threat matching function
find_threats <- function(text){
  if(is.na(text)){
    return(NULL)
  }
  text <- stringr::str_to_lower(text)
  matches <- tibble()
  for(category in names(threat_dictionary)){
    terms <- threat_dictionary[[category]]
    for(term in terms){
      if(stringr::str_detect(text, term)){
        matches <- bind_rows(matches,
                             tibble(threat_category = category,
                                    matched_term = term))
      }
    }
  }
  matches %>%
    distinct()
}

# apply threat matching function across species
species_threats <- purrr::map_dfr(
  seq_len(nrow(species_profiles)),
  function(i){
    hits <- find_threats(species_profiles$threats[i])
    if(is.null(hits)){
      return(NULL)
    }
    bind_cols(tibble(id = species_profiles$id[i]), hits)
  }
)

# add species information
colnames(species_master)
species_threats <- species_threats %>%
  left_join(species_master %>%
              select(id, record_name, common_name, scientific_name, sara_status), by = "id")
head(species_threats)

# export
write_csv(species_threats,"data_processed/species_threats.csv")

# checks
species_threats %>%
  count(threat_category, sort = TRUE)

species_threats %>%
  filter(threat_category == "Flow Alteration") %>%
  distinct(common_name)

species_threats %>%
  filter(threat_category == "Barriers")

species_threats %>%
  filter(threat_category == "Climate Change")

# more checks
species_threats %>%
  count(threat_category, sort = TRUE)

species_threats %>%
  count(record_name, sort = TRUE)

# summary file
species_threat_summary <- species_threats %>%
  distinct(id, record_name, threat_category)
species_threat_summary %>%
  count(record_name, sort = TRUE)

species_threats %>%
  count(threat_category, matched_term, sort = TRUE)

write_csv(species_threat_summary,"data_processed/species_threats_summary.csv")

# look for species with unusually high threat counts
species_threats %>%
  filter(record_name == "Nooksack Dace") %>%
  distinct(threat_category) %>%
  arrange(threat_category)

# which threat categories are driving a given record
species_threats %>%
  distinct(record_name, threat_category) %>%
  count(threat_category, sort = TRUE)

species_threats %>%
  distinct(record_name, threat_category) %>%
  count(threat_category, sort = TRUE)

# threat category frequency
threat_category_frequency <- species_threats %>%
  distinct(id, record_name, threat_category) %>%
  count( threat_category, sort = TRUE)

write_csv(threat_category_frequency,"data_processed/threat_category_frequency.csv")
