# download images script
source("scripts/00_setup.R")

dir.create("app/www/images", recursive = TRUE, showWarnings = FALSE)

images <- species_master %>%
  filter(!is.na(imageFileName)) %>%
  mutate(image_url = paste0(
    "https://ecprccsarstacct.z9.web.core.windows.net/files/photo/",
    imageFileName),
    local_file = paste0(gsub(" ", "_", tolower(common_name)),".jpg"))

for(i in seq_len(nrow(images))) {
  download.file(url = images$image_url[i],
                destfile = file.path("app/www/images",
                                     images$local_file[i]),
                mode = "wb",quiet = TRUE)}

