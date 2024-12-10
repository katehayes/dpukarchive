

#' Adds column detailing whether folder should contain file
#'
#' @param df A tibble.
#'
#' @return A tibble.
#' @export
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
#' @export
file_status <- function(df) {

  df %>%
    filter(should_contain == T) %>%
    group_by(file_name) %>%
    mutate(file_status = if_else(all(contains == T), "always present", if_else(all(contains == F), "never present", "sometimes present"))) %>%
    ungroup()

}


# you should be able to choose all different tests. and if conducted save their outputs in a sensible big list.
# https://stackoverflow.com/questions/41582136/r-what-do-you-call-the-and-operators-and-how-do-they-differ


#' Plots file presence/absence in folders
#' @param police_force A string.
#' @param data_series A string.
#' @param month_min A string.
#' @param month_max A string.
#' @return A ggplot object.
#' @export
#' @examples
#' fs_graph(police_force = "wiltshire", data_series = "stop-and-search", month_min = "2019-01", month_max = "2024-06")
fs_graph <- function(police_force = ".", data_series = ".", month_min = ".", month_max = ".") {

  text_colour <- "white"

  # folders along x axis files along y axis.

  report <- should_contain() %>%
    arch_filter(police_force, data_series, month_min, month_max) %>%
    filter(should_contain == T) %>%
    select(file_name, folder, contains) %>%
    mutate(contains = if_else(contains == T, "Present", "Absent")) %>%
    # complete(file_name, folder, fill = list(contains = "outside range")) %>%
    mutate(folder = substr(folder, 4, 10),
           file_name = substr(file_name, 1, 7)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = folder, y = forcats::fct_rev(file_name), fill = contains),
                       colour = "black") +
    ggplot2::scale_fill_manual(values = c("#c44920", "#20c452")) +
    ggplot2::scale_y_discrete(position = "left",
                              name = "File",
                              expand = c(0,0)) +
    ggplot2::scale_x_discrete(position = "bottom",
                              name = "Folder",
                              expand = c(0,0)) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill='transparent'),
                   strip.text = ggplot2::element_text(colour = text_colour),
                   plot.background = ggplot2::element_rect(fill='transparent'),
                   panel.background = ggplot2::element_rect(fill='transparent'),
                   legend.background =  ggplot2::element_rect(fill='transparent'),
                   axis.ticks = ggplot2::element_line(colour = text_colour),
                   panel.grid.major = ggplot2::element_line(colour="darkgray", size = 0.5),
                   axis.text.y = ggplot2::element_text(size = 7, colour = text_colour),
                   axis.text.x = ggplot2::element_text(size = 7, colour = text_colour, angle=90),
                   axis.title.x = ggplot2::element_text(size = 11, colour = text_colour),
                   axis.title.y = ggplot2::element_text(size = 11, colour = text_colour),
                   legend.title = ggplot2::element_blank(),
                   legend.key.size = ggplot2::unit(0.5, "lines"),
                   legend.text = ggplot2::element_text(size = 11,
                                                       colour = text_colour),
                   legend.position = c(0.1, 0.1))

  # +ggplot2::labs(title = paste())


}


