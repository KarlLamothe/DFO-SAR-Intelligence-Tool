source("scripts/00_setup.R")

# load data
species_profiles <- read.csv("data_processed/all_species_profiles.csv")
species_master <- read.csv("data_processed/all_species_master.csv")

# create a list of relevant threat
threat_dictionary <- list(
  "Residential and Commercial Development" = c("development","residential","housing",
                                               "commercial","housing","subdivision",
                                               "urban", "urbanization","tourism","industrial",
                                               "impervious","stormwater","habitat loss",
                                               "habitat destruction"),
  "Agriculture and Aquaculture" = c("agriculture", "agricultural", "livestock","aquaculture",
                                    "farming","farm","plantation","grazing","habitat degredation"),
  "Energy Production and Gas" = c("oil","gas","drilling","mining","renewable","quarrying"),
  "Transportation and Service Corridors" = c("road","railroad","utility","service lines",
                                             "shipping","road","highway","crossing",
                                             "bridge","transport corridor"),
  "Biological Resource Use" = c("hunting","fishing","gathering","logging","harvest",
                                "subsistence","overharvest","overfishing","bycatch",
                                "incidental capture"),
  "Human Intrusions and Disturbance" = c("recreation", "war","work",
                                         "recreational","boating","off-road vehicle","atv",
                                         "human disturbance", "habitat loss", "habitat destruction", "habitat degradation",
                                         "degradation of habitat"),
  "Natural Systems Modification" =c("fire","fire suppression","dams","water management",
                                    "water abstraction", "small dams", "large dams",
                                    "barrier","culvert","fish passage","flow","flow alteration",
                                    "water level","hydrolog","discharge","water regulation",
                                    "shoreline","waterfront development",
                                    "cottage development","bank stabilization",
                                    "dredging","channelization","channel modification",
                                    "straightening","realignment","fragmentation",
                                    "habitat fragmentation","habitat isolation","isolated population",
                                    "connectivity"),
  "Invasive Species and other Problematic Species, Genes, and Diseases" = c(
    "invasive", "non-native", "exotic species", "zebra mussel",
     "quagga mussel", "round goby", "common carp", "sea lamprey",
     "alien","problematic species","introduced genetic material",
     "disease", "parasite", "pathogen","virus","prion"),
  "Pollution"=c("waste water", "waste", "sewage", "run-off", "runoff", "oil spill",
                "manure", "seepage", "effluent", "nutrient", "loading", "load",
                "erosion", "sedimentation", "herbicide", "pesticide", "garbage",
                "acid rain", "smog", "ozone", "light pollution", "thermal pollution",
                "noise pollution", "sediment", "silt", "turbidity","contaminent",
                "toxic substance", "contamination","pollution","phosphorus","nitrogen",
                "eutrophication","dissolved oxygen","oxygen depletion","poor water quality"),
  "Geological Events" =c("volcano","eartquake","avalanche","tsunami","landslide"),
  "Climate Change" = c("climate", "warming", "temperature", "extreme weather",
                       "drought","flooding","changing precipitation", "storm"),
  "Other Limiting Factors" = c("hybridization","hybrid","genetic introgression",
                               "predation","predator","predators",
                               "competition", "competitive exclusion","competitor",
                               "small population","low abundance",
                              "demographic stochasticity","genetic drift",
                              "inbreeding","poorly understood","data deficient",
                              "lack of information","habitat loss"))

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
write_csv(species_threats,"data_processed/all_species_threats.csv")

# checks
species_threats %>%
  count(threat_category, sort = TRUE)

species_threats %>%
  filter(threat_category == "Climate Change")

# more checks
species_threats %>%
  count(threat_category, sort = TRUE)

species_threats %>%
  count(record_name, sort = TRUE)

# summary file
species_threat_summary <- species_threats %>%
  distinct(id,record_name,common_name,threat_category) %>%
  group_by(id,record_name,common_name) %>%
  summarise(n_threat_categories = n(),
            threat_categories = paste(sort(threat_category),collapse = "; "),
            .groups = "drop")
write_csv(species_threat_summary,"data_processed/all_species_threats_summary.csv")

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

write_csv(threat_category_frequency,"data_processed/all_threat_category_frequency.csv")
