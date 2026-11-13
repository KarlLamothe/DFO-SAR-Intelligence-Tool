# download images script
dir.create("app/www/images", recursive = TRUE, showWarnings = FALSE)

images <- species_master %>%
  filter(!is.na(imageFileName)) %>%
  mutate(image_url = paste0(
    "https://ecprccsarstacct.z9.web.core.windows.net/files/photo/",
    gsub(" ", "%20", imageFileName)),
    local_file = paste0(common_name %>%
                          tolower() %>%
                          gsub("/", "-", .) %>%   
                          gsub(" ", "_", .), ".jpg"))

failed <- character()

for(i in seq_len(nrow(images))) {
  if(i %% 25 == 0 || i == nrow(images)) {
    message(sprintf("Downloading images: %s/%s (%.0f%%)",i,nrow(images),
        100 * i / nrow(images)))}
  
  tryCatch({
    download.file(url = images$image_url[i], destfile = file.path(
        "app/www/images", images$local_file[i]), mode = "wb", quiet = TRUE)
  }, error = function(e) {
    failed <<- c(failed, images$common_name[i])
  })
}

species_master %>%
  filter(grepl("/", common_name) |
           grepl("\\\\", common_name) |
           grepl(":", common_name)) %>%
  select(common_name)

message(sprintf(
    "Image download complete: %s images processed, %s failures.",nrow(images),
    length(failed)))

if(length(failed) > 0) {message(paste("Failed species:",
                                      paste(failed, collapse = ", ")))}
