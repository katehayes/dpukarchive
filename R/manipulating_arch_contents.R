#' Filters by force, data series, time frame
#'
#' @param df A tibble.
#' @param police_force A string.
#' @param data_series A string.
#' @param month_min A string.
#' @param month_max A string.
#' @param option Either contains or should_contain
#'
#' @import dplyr
#' @import tidyr
#'
#' @return A tibble.
#'
#' @examples
#' arch_filter(police_force = "wiltshire", data_series = "stop-and-search", month_min = "2019-01", month_max = "2024-06")
arch_filter <- function(df = archive_contents,
                        police_force = ".", data_series = ".", month_min = ".", month_max = ".",
                        option) {

  df %>%
    mutate(year_month = paste(year, month, sep="-")) %>%
    filter(police == police_force,
           series == data_series,
           year_month >= month_min,
           year_month <= month_max) %>%
    select(-year_month) %>%
   filter({{option}} == T)

}


#' Adds column detailing whether folder should contain file
#'
#' @param df A tibble.
#'
#' @return A tibble.
should_contain <- function(df = archive_contents) {

  df %>%
    mutate(year_month = paste(year, month, sep="-")) %>%
    group_by(folder) %>%
    mutate(folder_min = min(year_month[contains == T]),
           folder_max = max(year_month[contains == T])) %>%
    ungroup() %>%
    mutate(should_contain = ifelse(year_month >= folder_min & year_month <= folder_max,
                                   T, F)) %>%
    select(-c(folder_min, folder_max, year_month))
  # drop year_month again? drop folder min and max?

}

#' Adds column detailing file status
#'
#' @param df A tibble.
#'
#' @return A tibble.
file_status <- function(df) {

  df %>%
    filter(should_contain == T) %>%
    group_by(file_name) %>%
    mutate(file_status = if_else(all(contains == T), "always present", if_else(all(contains == F), "never present", "sometimes present"))) %>%
    ungroup()

}
