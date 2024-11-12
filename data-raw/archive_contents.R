## code to prepare `archive_contents` dataset

page <- xml2::read_html("https://data.police.uk/data/archive/")

zip_list <- page %>%
  rvest::html_nodes("a") %>%       # find all links
  rvest::html_attr("href") %>%     # get the url
  stringr::str_subset("/[0-9]{4}\\-[0-9]{2}\\.zip$") # find those that end in 'year-month.zip'

zip_list <- paste("https://data.police.uk", zip_list, sep = "")


archive_contents <- crossing(year = 2010:2024, month = c("01", "02", "03", "04",
                                                   "05", "06", "07", "08",
                                                   "09", "10", "11", "12"),
                       police = force_list$id, series = series_list) %>%
  filter(year != 2010 | year == 2010 & month == "12") %>%
  filter(year != 2024 | year == 2024 & as.numeric(month) <= 8) %>%
  arrange(desc(year), desc(month)) %>%
  mutate(file_name = paste(year, "-", month, "/", year, "-", month, "-", police, "-", series, ".csv", sep = ""))


temp <- tempfile()

for (i in zip_list) {

  col_nm <- paste("zip", str_sub(i, 37, 43), sep = "")

  download.file(i, temp)
  files_list <- unzip(temp, list = TRUE)$Name

  archive_contents <- archive_contents %>%
    mutate(!!col_nm := ifelse(file_name %in% files_list, T, F))

}


archive_contents <- archive_contents %>%
  pivot_longer(c(starts_with("zip")),
               names_to = "folder",
               values_to = "contains")


usethis::use_data(archive_contents, overwrite = TRUE)

