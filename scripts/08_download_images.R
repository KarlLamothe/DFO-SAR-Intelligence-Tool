# download images script
source("scripts/00_setup.R")

dir.create("app/www/images", recursive = TRUE, showWarnings = FALSE)

images <- species_master %>%
  filter(!is.na(imageFileName)) %>%
  mutate(
    image_url = paste0(
      "https://ecprccsarstacct.z9.web.core.windows.net/files/photo/",
      gsub(" ", "%20", imageFileName)
    ),
    local_file = paste0(
      common_name %>%
        tolower() %>%
        gsub("/", "-", .) %>%   
        gsub(" ", "_", .), ".jpg"))

for(i in seq_len(nrow(images))) {
  tryCatch({
    download.file(url = images$image_url[i],
                  destfile = file.path("app/www/images",images$local_file[i]),
                  mode = "wb", quiet = TRUE)
  }, error = function(e) {
    message("Failed: ", images$common_name[i])
  })
}

species_master %>%
  filter(grepl("/", common_name) |
           grepl("\\\\", common_name) |
           grepl(":", common_name)) %>%
  select(common_name)
