

# write function that can go in any of the other functions, checks conditions
# ie only name one police for, one data series, that they are part of the lists
# that the months are in range, month min smaller than month max and so on.


#' For given force, data series, time range, chooses list of files to download
#'
#' @param police_force
#' @param data_series
#' @param month_min
#' @param month_max
#'
#' @return List of urls to data files
#'
#' @examples
#' choose_files(police_force = "dorset", data_series = "street", month_min = "2017-03", month_max = "2017-06")
choose_files <- function(police_force = ".", data_series = ".", month_min = ".", month_max = ".", option = "list") {

  df <- archive_contents %>%
    arch_filter(police_force, data_series, month_min, month_max, option = contains) %>%
    select(file_name, folder) %>%
    mutate(present = 1) %>%
    pivot_wider(names_from = folder,
                values_from = present,
                values_fill = 0)  %>%
    tibble::column_to_rownames(var="file_name")


  # # https://math.mit.edu/~goemans/18434S06/setcover-tamara.pdf
  # # https://stackoverflow.com/questions/39184257/set-cover-approximation-in-r
  # # incidence matrix  (each row corresponds to an element and each column corresponds to a set)
  solution <- lpSolve::lp("min", rep(1, ncol(df)), df, rep(">=", nrow(df)), rep(1, nrow(df)))$solution


  url_list <- df %>%
    tibble::rownames_to_column("file_name") %>%
    pivot_longer(!file_name,
                 names_to = "folder",
                 values_to = "present") %>%
    left_join(archive_contents %>%
                arch_filter(police_force, data_series, month_min, month_max, option = contains) %>%
                distinct(folder) %>%
                cbind(solution)) %>%
    filter(solution == 1) %>%
    select(!solution) %>%
    group_by(file_name) %>%
    mutate(selected = if_else(folder == max(folder[present == 1]), 1, 0)) %>%  # select most recent
    ungroup() %>%
    filter(selected == 1) %>%
    select(!c(present, selected))

  if(option != "table") {

    url_list <- url_list %>%
      mutate(folder = substr(folder, 4, nchar(folder)),
             folder = paste("https://data.police.uk/data/archive/", folder, ".zip", sep = "")) %>%
      with(split(file_name,
                 factor(folder, levels = unique(folder))))

  }

  return(url_list)

}




# check <- choose_files(police_force = "wiltshire", data_series = "stop-and-search", month_min = "2019-01", month_max = "2024-06", option = "table")
#


#' Takes a list of files, extracts them from archive
#'
#' @param url_list A list containing URLs of data files
#'
#' @return A list of data tables
extract_list <- function(url_list) {

  data_list <- list()

  for (i in 1:length(url_list)) {

    temp <- tempfile()

    utils::download.file(names(url_list)[i], temp)

    data_list[[i]] <- bind_rows(lapply(url_list[[i]], function(fn) readr::read_csv(unz(temp, fn), col_types = readr::cols(Latitude = readr::col_double(), Longitude = readr::col_double()))))

  }

  return(data_list)

}




# need to write an option for a report
# essentially, which folders were used for which files
# maybe also if anything is missing, if any of the files are missing in places


#' Extract data from archive efficiently
#' @param police_force A string.
#' @param data_series A string.
#' @param month_min A string.
#' @param month_max A string.
#' @import dplyr
#' @import tidyr
#'
#' @return A tibble.
#' @export
#' @example arch_extract(police_force = "norfolk", data_series = "stop-and-search", month_min = "2020-01", month_max = "2020-03")
arch_extract <- function(police_force = ".", data_series = ".", month_min = ".", month_max = ".") {

  url_list <- choose_files(police_force, data_series, month_min, month_max)

  df <- extract_list(url_list) %>%
    bind_rows()

  return(df)

}


# check <- arch_extract(police_force = "norfolk", data_series = "stop-and-search", month_min = "2020-01", month_max = "2020-03")
