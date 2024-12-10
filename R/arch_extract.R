#' Gets list of police forces
#'
#' @return A tibble.
#' @export
#'
#' @examples
get_force_list <- function() {

  httr::GET("https://data.police.uk/api/forces") %>%
    httr::content("text") %>% # extract the JSON
    jsonlite::fromJSON() # convert the JSON string to a list

}





# function that can go in any of the other functions, checks conditions
# ie only name one police for, one data series, that they are part of the lists
# that the months are in range, month min smaller than month max and so on.



#' Filters by force, data series, time frame
#'
#' @param df A tibble.
#' @param police_force A string.
#' @param data_series A string.
#' @param month_min A string.
#' @param month_max A string.
#'
#' @import dplyr
#' @import tidyr
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' arch_filter(police_force = "wiltshire", data_series = "stop-and-search", month_min = "2019-01", month_max = "2024-06")
arch_filter <- function(df = archive_contents, police_force = ".", data_series = ".", month_min = ".", month_max = ".") {

  df %>%
    mutate(year_month = paste(year, month, sep="-")) %>%
    filter(police == police_force,
           series == data_series,
           year_month >= month_min,
           year_month <= month_max) %>%
    select(-year_month)

}




# need to write an option for a report
# essentially, which folders were used for which files
# maybe also if anything is missing, if any of the files are missing in places

#' Extract data from archive efficiently
#'
#' @param police_force A string.
#' @param data_series A string.
#' @param month_min A string.
#' @param month_max A string.
#'
#' @import dplyr
#' @import tidyr
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' arch_extract(police_force = "west-midlands", data_series = "stop-and-search", month_min = "2015-01", month_max = "2020-08")
#' arch_extract(police_force = "greater-manchester", data_series = "outcomes", month_min = "2012-12", month_max = "2013-01")
arch_extract <- function(police_force = ".", data_series = ".", month_min = ".", month_max = ".") {

  # op <- archive_contents %>%
  #   filter(police == police_force, series == data_series) %>%
  #   mutate(year_month = paste(year, month, sep="-")) %>%
  #   filter(year_month >= month_min,
  #          year_month <= month_max) %>%
  #   filter(contains == T) %>%
  #   select(file_name, folder)

  op <- archive_contents %>%
    arch_filter(police_force, data_series, month_min, month_max) %>%
    filter(contains == T) %>%
    select(file_name, folder)

  nm <- op %>%
    distinct(folder)

  op <- op %>%
    mutate(count = 1) %>%
    pivot_wider(names_from = folder,
                values_from = count,
                values_fill = 0)  %>%
    tibble::column_to_rownames(var="file_name")

  # # https://math.mit.edu/~goemans/18434S06/setcover-tamara.pdf
  # # https://stackoverflow.com/questions/39184257/set-cover-approximation-in-r
  # # incidence matrix  (each row corresponds to an element and each column corresponds to a set)

  solution <- lpSolve::lp("min", rep(1, ncol(op)), op, rep(">=", nrow(op)), rep(1, nrow(op)))$solution

  zip_list <- op %>%
    tibble::rownames_to_column("file_name") %>%
    pivot_longer(!file_name,
                 names_to = "folder",
                 values_to = "present") %>%
    left_join(nm %>%
                cbind(solution)) %>%
    filter(solution == 1) %>%
    select(!solution) %>%
    group_by(file_name) %>%
    mutate(selected = if_else(folder == max(folder[present == 1]), 1, 0)) %>%  # select most recent
    ungroup() %>%
    filter(selected == 1)  %>%
    select(!c(present, selected)) %>%
    mutate(folder = substr(folder, 4, nchar(folder)),
           folder = paste("https://data.police.uk/data/archive/", folder, ".zip", sep = "")
           # ,
           # file_name = paste(substr(file_name, 1, 7), file_name, sep = "/")
           ) %>%
    # unstack(file_name ~ folder)
    with(split(file_name,
               factor(folder, levels = unique(folder))))


  data_list <- list()


  for (i in 1:length(zip_list)) {

    temp <- tempfile()

    utils::download.file(names(zip_list)[i], temp)

    data_list[[i]] <- bind_rows(lapply(zip_list[[i]], function(fn) readr::read_csv(unz(temp, fn), col_types = readr::cols(Latitude = readr::col_double(), Longitude = readr::col_double()))))

  }

  df <- bind_rows(data_list)

  return(df)

}

# check <- arch_extract(police_force = "west-midlands", data_series = "stop-and-search", month_min = "2015-01", month_max = "2020-08")
