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

# you should be able to choose all different tests. and if conducted save their outputs in a sensible big list.
# https://stackoverflow.com/questions/41582136/r-what-do-you-call-the-and-operators-and-how-do-they-differ


#' Converts police force code to full name
#'
#' @param pf_code A string containing a police force code name
#'
#' @return A named character string containing the corresponding police force name
#'
#' @example
#' get_pf_name(metropolitan)
get_pf_name <- function(pf_code) {

  pf_name <- get_force_list() %>%
    filter(id == pf_code) %>%
    select(name) %>%
    as.character()

  return(pf_name)

}


get_ds_name <- function(ds_code) {

  ds_name <- case_when(ds_code == "stop-and-search" ~ "Stop & search",
                       ds_code == "outcomes" ~ "Outcomes",
                       ds_code == "street" ~ "Street")

  return(ds_name)

}


# would be nice if there was one horizontal line for folders indicating which got downloaded
# and a vertical line for files? indicating which are present always, sometimes, never??

#' Plots file presence/absence in folders
#' @param police_force A string.
#' @param data_series A string.
#' @param month_min A string.
#' @param month_max A string.
#' @return A ggplot object.
#' @export
#' @examples
#' fs_graph(police_force = "wiltshire", data_series = "stop-and-search", month_min = "2019-01", month_max = "2024-06")
fs_graph <- function(police_force = ".", data_series = ".", month_min = ".", month_max = ".", tile_colours = c("#bdbebd", "#8bce7b", "#17a420")) {

  # pf_name <- get_pf_name(police_force)
  # ds_name <- get_ds_name(data_series)


  text_colour <- "white"

  # folders along x axis files along y axis.

  report <- archive_contents %>%
    should_contain() %>%
    arch_filter(police_force, data_series, month_min, month_max, option = should_contain) %>%
    select(file_name, folder, contains) %>%
    mutate(contains = if_else(contains == T, "Present", "Absent")) %>%
    # complete(file_name, folder, fill = list(contains = "outside range")) %>%
    left_join(choose_files(police_force, data_series, month_min, month_max, option = "table") %>%
                mutate(selected = 1)) %>%
    mutate(contains = if_else(!(is.na(selected)), "Present - Selected", contains)) %>%
    mutate(folder = substr(folder, 4, 10),
           file_name = substr(file_name, 1, 7)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = folder, y = forcats::fct_rev(file_name), fill = contains),
                       colour = "black") +
    ggplot2::coord_equal() +
    # ggplot2::scale_fill_manual(values = c("#c44920", "#20c452")) +
    ggplot2::scale_fill_manual(values = tile_colours) +
    ggplot2::scale_y_discrete(position = "left",
                              name = "File",
                              expand = c(0,0)) +
    ggplot2::scale_x_discrete(position = "bottom",
                              name = "Folder",
                              expand = c(0,0)) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill='transparent'),
                   strip.text = ggplot2::element_text(colour = text_colour),
                   plot.title = ggplot2::element_text(colour = text_colour),
                   plot.background = ggplot2::element_rect(fill='transparent'),
                   panel.background = ggplot2::element_rect(fill='transparent'),
                   legend.background =  ggplot2::element_rect(fill='transparent'),
                   axis.ticks = ggplot2::element_line(colour = text_colour),
                   # panel.grid.major = ggplot2::element_line(colour="darkgray", size = 0.1),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 7, colour = text_colour),
                   axis.text.x = ggplot2::element_text(size = 7, colour = text_colour, angle=90),
                   axis.title.x = ggplot2::element_text(size = 11, colour = text_colour),
                   axis.title.y = ggplot2::element_text(size = 11, colour = text_colour),
                   legend.title = ggplot2::element_blank(),
                   legend.key.size = ggplot2::unit(0.5, "lines"),
                   legend.text = ggplot2::element_text(size = 11,
                                                       colour = text_colour),
                   legend.position = c(0.1, 0.1)) +
    ggplot2::labs(title = paste("Police force:", get_pf_name(police_force),
                                "\nData series:", get_ds_name(data_series),
                                "\nData presence/absence for the period",
                                month_min, "to", month_max, sep = " "))


}


#  check <- fs_graph(police_force = "wiltshire", data_series = "stop-and-search", month_min = "2019-01", month_max = "2024-06",
#                    tile_colours = c("#a4a5a4", "#8bce7b", "#17a420"))
# # # # #
#  check


# check <- paste("Police force:", get_pf_name("wiltshire"),
#                "\nData series:", get_ds_name("outcomes"),
#                "\nData presence/absence for the period",
#                "2019-01", "to", "2024-01", sep = " ")
