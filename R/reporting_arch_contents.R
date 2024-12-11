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


# take away the present selected out of the basic function - you could of course also just select most recent

# would be nice if there was one horizontal line for folders indicating which got downloaded
# and a vertical line for files? indicating which are present always, sometimes, never??

# should be a warning if there is no copy of the file available in any folder?

#' Plots file presence/absence in folders
#' @param police_force A string.
#' @param data_series A string.
#' @param month_min A string.
#' @param month_max A string.
#' @return A ggplot object.
#' @export
#' @examples
#' fs_graph(police_force = "wiltshire", data_series = "stop-and-search", month_min = "2019-01", month_max = "2024-06")
fs_graph <- function(police_force = ".", data_series = ".", month_min = ".", month_max = ".",
                     tile_colours = c("#FF5733", "#36cc86","#217f58"),
                     text_colour = "white",
                     background_colour = "#0c1117") {

  plot_title <- paste("Police force:", get_pf_name(police_force),
                      "\nData series:", get_ds_name(data_series),
                      "\nTime period:",
                      month_min, "to", month_max, sep = " ")

  # folder_files <- list()
  #
  #
  #
  # note <-


  # folders along x axis files along y axis.

  report <- archive_contents %>%
    should_contain() %>%
    arch_filter(police_force, data_series, month_min, month_max, option = should_contain) %>%
    select(file_name, folder, contains) %>%
    mutate(contains = if_else(contains == T, "Present", "Missing")) %>%
    # complete(file_name, folder, fill = list(contains = "outside range")) %>%
    left_join(choose_files(police_force, data_series, month_min, month_max, option = "table") %>%
                mutate(selected = 1)) %>%
    mutate(contains = if_else(!(is.na(selected)), "Present - Selected", contains)) %>%
    mutate(folder = substr(folder, 4, 10),
           file_name = substr(file_name, 1, 7)) %>%
    ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = 0.5, xmax = 68.5, ymin = 0.5, ymax = 66.5, alpha=0.1, color=NA, fill=text_colour)+
    ggplot2::geom_tile(ggplot2::aes(x = folder, y = forcats::fct_rev(file_name), fill = contains),
                       colour = background_colour) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(1,70), ylim = NULL, expand = TRUE, clip = "off") +
    # ggplot2::scale_fill_manual(values = c("#c44920", "#20c452")) +
    ggplot2::scale_fill_manual(values = tile_colours) +
    ggplot2::scale_y_discrete(position = "left",
                              name = "Files") +
    ggplot2::scale_x_discrete(position = "bottom",
                              name = "Folders") +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill=background_colour),
                   strip.text = ggplot2::element_text(colour = text_colour),
                   plot.title = ggplot2::element_text(size = 10, colour = text_colour),
                   plot.caption = ggplot2::element_text(size = 10, colour = text_colour, hjust = 0),
                   plot.background = ggplot2::element_rect(fill=background_colour),
                   legend.background =  ggplot2::element_rect(fill='transparent'),
                   # axis.ticks = ggplot2::element_line(colour = text_colour),
                   axis.ticks = ggplot2::element_blank(),
                   # axis.line = ggplot2::element_line(colour=text_colour, size = 0.2),
                   panel.background = ggplot2::element_rect(fill='transparent'),
                   # panel.background = ggplot2::element_rect(colour=text_colour, linewidth = 0.25, fill=background_colour), # "#ececec"
                   # panel.grid.major = ggplot2::element_line(colour="darkgray", size = 0.1),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 5, colour = text_colour,
                                                       hjust = 0, vjust = 0.5),
                   axis.text.x = ggplot2::element_text(size = 5, colour = text_colour,
                                                       angle=90,
                                                       hjust = 0, vjust = 1),
                   axis.title.x = ggplot2::element_text(size = 10, colour = text_colour),
                   axis.title.y = ggplot2::element_text(size = 10, colour = text_colour),
                   legend.title = ggplot2::element_blank(),
                   legend.key.size = ggplot2::unit(0.5, "lines"),
                   legend.margin = ggplot2::margin(t = 0, unit='cm'),
                   legend.text = ggplot2::element_text(size = 10,
                                                       colour = text_colour),
                   legend.position = c(0.991, 0),
                   legend.justification = c(0, 0),
                   plot.margin = ggplot2::unit(c(1,13,1,1), "lines")) +
    ggplot2::annotate("text",
                      x = 70, y = 66,
                      label = plot_title,
                      colour = text_colour,
                      size = 3.75,
                      hjust = 0, vjust = 1) + # 0 is left/top aligned, 0.5 centered, 1 right/bottom
    ggplot2::annotate("rect", xmin = 0.5, xmax = 68.5, ymin = 0.5, ymax = 66.5, alpha=1, color=text_colour, fill=NA)


 return(report)

}

# #
# # police_force <- "wiltshire"
# data_series <- "stop-and-search"
# month_min <- "2019-01"
# month_max <- "2024-06"
# # tile_colours <- c("#ecbcac", "#acecbc", "#1cca8a")
# #
# tile_colours <- c("#FF5733", "#36cc86","#217f58")
# tile_colours <- c("#FF5733", "#36cc86")
# tile_colours <- c("#a14b40", "#217f58", "#36cc86")
#
# # tile_colours <- c("#bdbebd", "#8bce7b", "#17a420")
# #
# text_colour <- "white"
# background_colour <- "#0c1117" # can be "transparent"
# #
# #
# #
# report <- archive_contents %>%
#   should_contain() %>%
#   arch_filter(police_force = "west-midlands", data_series, month_min, month_max, option = should_contain) %>%
#   select(file_name, folder, contains) %>%
#   mutate(contains = if_else(contains == T, "Present", "Missing")) %>%
#   # complete(file_name, folder, fill = list(contains = "outside range")) %>%
#   left_join(choose_files(police_force = "west-midlands", data_series, month_min, month_max, option = "table") %>%
#               mutate(selected = 1)) %>%
#   mutate(contains = if_else(!(is.na(selected)), "Present - Selected", contains)) %>%
#   mutate(folder = substr(folder, 4, 10),
#          file_name = substr(file_name, 1, 7)) %>%
#   mutate(pf = "West Midlands Police") %>%
#   rbind(archive_contents %>%
#           should_contain() %>%
#           arch_filter(police_force = "wiltshire", data_series, month_min, month_max, option = should_contain) %>%
#           select(file_name, folder, contains) %>%
#           mutate(contains = if_else(contains == T, "Present", "Missing")) %>%
#           # complete(file_name, folder, fill = list(contains = "outside range")) %>%
#           left_join(choose_files(police_force = "wiltshire", data_series, month_min, month_max, option = "table") %>%
#                       mutate(selected = 1)) %>%
#           mutate(contains = if_else(!(is.na(selected)), "Present - Selected", contains)) %>%
#           mutate(folder = substr(folder, 4, 10),
#                  file_name = substr(file_name, 1, 7)) %>%
#           mutate(pf = "Wiltshire Police")) %>%
#   rbind(archive_contents %>%
#           should_contain() %>%
#           arch_filter(police_force = "metropolitan", data_series, month_min, month_max, option = should_contain) %>%
#           select(file_name, folder, contains) %>%
#           mutate(contains = if_else(contains == T, "Present", "Missing")) %>%
#           # complete(file_name, folder, fill = list(contains = "outside range")) %>%
#           left_join(choose_files(police_force = "metropolitan", data_series, month_min, month_max, option = "table") %>%
#                       mutate(selected = 1)) %>%
#           mutate(contains = if_else(!(is.na(selected)), "Present - Selected", contains)) %>%
#           mutate(folder = substr(folder, 4, 10),
#                  file_name = substr(file_name, 1, 7)) %>%
#           mutate(pf = "Metropolitan Police Service")) %>%
#   ggplot2::ggplot() +
#   ggplot2::annotate("rect", xmin = 0.5, xmax = 68.5, ymin = 0.5, ymax = 66.5, alpha=0.1, color=NA, fill=text_colour)+
#   ggplot2::geom_tile(ggplot2::aes(x = folder, y = forcats::fct_rev(file_name), fill = contains),
#                      colour = background_colour) +
#   ggplot2::facet_wrap(~pf, nrow = 1) +
#   ggplot2::coord_fixed(ratio = 1, xlim = c(1,70), ylim = NULL, expand = TRUE, clip = "off") +
#   # ggplot2::scale_fill_manual(values = c("#c44920", "#20c452")) +
#   ggplot2::scale_fill_manual(values = tile_colours) +
#   ggplot2::scale_y_discrete(position = "left",
#                             name = "Files") +
#   ggplot2::scale_x_discrete(position = "bottom",
#                             name = "Folders") +
#   ggplot2::theme(strip.background = ggplot2::element_rect(fill=background_colour),
#                  strip.text = ggplot2::element_text(colour = text_colour),
#                  plot.title = ggplot2::element_text(size = 10, colour = text_colour),
#                  plot.caption = ggplot2::element_text(size = 10, colour = text_colour, hjust = 0),
#                  plot.background = ggplot2::element_rect(fill=background_colour),
#                  legend.background =  ggplot2::element_rect(fill='transparent'),
#                  # axis.ticks = ggplot2::element_line(colour = text_colour),
#                  axis.ticks = ggplot2::element_blank(),
#                  # axis.line = ggplot2::element_line(colour=text_colour, size = 0.2),
#                  panel.background = ggplot2::element_rect(fill='transparent'),
#                  # panel.background = ggplot2::element_rect(colour=text_colour, linewidth = 0.25, fill=background_colour), # "#ececec"
#                  # panel.grid.major = ggplot2::element_line(colour="darkgray", size = 0.1),
#                  panel.grid.major = ggplot2::element_blank(),
#                  axis.text.y = ggplot2::element_text(size = 5, colour = text_colour,
#                                                      hjust = 0, vjust = 0.5),
#                  axis.text.x = ggplot2::element_text(size = 5, colour = text_colour,
#                                                      angle=90,
#                                                      hjust = 0, vjust = 1),
#                  axis.title.x = ggplot2::element_text(size = 10, colour = text_colour),
#                  axis.title.y = ggplot2::element_text(size = 10, colour = text_colour),
#                  legend.title = ggplot2::element_blank(),
#                  legend.key.size = ggplot2::unit(0.5, "lines"),
#                  legend.margin = ggplot2::margin(t = 0, unit='cm'),
#                  legend.text = ggplot2::element_text(size = 10,
#                                                      colour = text_colour)) +
#                  # legend.position = c(0.991, 0),
#                  # legend.justification = c(0, 0),
#                  # plot.margin = ggplot2::unit(c(1,11,1,1), "lines")) +
#   # ggplot2::coord_cartesian(clip = "off", xlim = c(1, 70)) +
#   # ggplot2::annotate("text",
#   #                   x = 70, y = 66,
#   #                   label = paste("Police force:", get_pf_name(police_force),
#   #                                 "\nData series:", get_ds_name(data_series),
#   #                                 "\nPeriod:",
#   #                                 month_min, "to", month_max, sep = " "),
#   #                   colour = text_colour,
#   #                   size = 3.75,
#   #                   hjust = 0, vjust = 1) + # 0 is left/top aligned, 0.5 centered, 1 right/bottom
#   # ggplot2::annotate("segment", x = 0.5, xend = 68.5, y = 0.5, yend = 0.5,
#   #                   colour = text_colour, linewidth = 0.3) +
#   # ggplot2::annotate("segment", x = 0.5, xend = 68.5, y = 66.5, yend = 66.5,
#   #                   colour = text_colour, linewidth = 0.3) +
#   # ggplot2::annotate("segment", x = 0.5, xend = 0.5, y = 0.5, yend = 66.5,
#   #                   colour = text_colour, linewidth = 0.3) +
#   # ggplot2::annotate("segment", x = 68.5, xend = 68.5, y = 0.5, yend = 66.5,
#   #                   colour = text_colour, linewidth = 0.3)
# ggplot2::annotate("rect", xmin = 0.5, xmax = 68.5, ymin = 0.5, ymax = 66.5, alpha=1, color=text_colour, fill=NA,
#                   linewidth = 0.2) +
#   ggplot2::labs(title = "Data series: stop & search\nTime period: 2019-01 to 2024-06")
#
# report
#
#
# report <- archive_contents %>%
#   should_contain() %>%
#   arch_filter(police_force = "west-midlands", data_series, month_min, month_max, option = should_contain) %>%
#   select(file_name, folder, contains) %>%
#   mutate(contains = if_else(contains == T, "Present", "Missing")) %>%
#   # complete(file_name, folder, fill = list(contains = "outside range")) %>%
#   mutate(folder = substr(folder, 4, 10),
#          file_name = substr(file_name, 1, 7)) %>%
#   mutate(pf = "West Midlands Police") %>%
#   rbind(archive_contents %>%
#           should_contain() %>%
#           arch_filter(police_force = "wiltshire", data_series, month_min, month_max, option = should_contain) %>%
#           select(file_name, folder, contains) %>%
#           mutate(contains = if_else(contains == T, "Present", "Missing")) %>%
#           # complete(file_name, folder, fill = list(contains = "outside range")) %>%
#           mutate(folder = substr(folder, 4, 10),
#                  file_name = substr(file_name, 1, 7)) %>%
#           mutate(pf = "Wiltshire Police")) %>%
#   rbind(archive_contents %>%
#           should_contain() %>%
#           arch_filter(police_force = "metropolitan", data_series, month_min, month_max, option = should_contain) %>%
#           select(file_name, folder, contains) %>%
#           mutate(contains = if_else(contains == T, "Present", "Missing")) %>%
#           # complete(file_name, folder, fill = list(contains = "outside range")) %>%
#           mutate(folder = substr(folder, 4, 10),
#                  file_name = substr(file_name, 1, 7)) %>%
#           mutate(pf = "Metropolitan Police Service")) %>%
#   ggplot2::ggplot() +
#   ggplot2::annotate("rect", xmin = 0.5, xmax = 68.5, ymin = 0.5, ymax = 66.5, alpha=0.1, color=NA, fill=text_colour)+
#   ggplot2::geom_tile(ggplot2::aes(x = folder, y = forcats::fct_rev(file_name), fill = contains),
#                      colour = background_colour) +
#   ggplot2::facet_wrap(~pf, nrow = 1) +
#   ggplot2::coord_fixed(ratio = 1, xlim = c(1,70), ylim = NULL, expand = TRUE, clip = "off") +
#   # ggplot2::scale_fill_manual(values = c("#c44920", "#20c452")) +
#   ggplot2::scale_fill_manual(values = tile_colours) +
#   ggplot2::scale_y_discrete(position = "left",
#                             name = "Files") +
#   ggplot2::scale_x_discrete(position = "bottom",
#                             name = "Folders") +
#   ggplot2::theme(strip.background = ggplot2::element_rect(fill=background_colour),
#                  strip.text = ggplot2::element_text(colour = text_colour),
#                  plot.title = ggplot2::element_text(size = 10, colour = text_colour),
#                  plot.caption = ggplot2::element_text(size = 10, colour = text_colour, hjust = 0),
#                  plot.background = ggplot2::element_rect(fill=background_colour),
#                  legend.background =  ggplot2::element_rect(fill='transparent'),
#                  # axis.ticks = ggplot2::element_line(colour = text_colour),
#                  axis.ticks = ggplot2::element_blank(),
#                  # axis.line = ggplot2::element_line(colour=text_colour, size = 0.2),
#                  panel.background = ggplot2::element_rect(fill='transparent'),
#                  # panel.background = ggplot2::element_rect(colour=text_colour, linewidth = 0.25, fill=background_colour), # "#ececec"
#                  # panel.grid.major = ggplot2::element_line(colour="darkgray", size = 0.1),
#                  panel.grid.major = ggplot2::element_blank(),
#                  axis.text.y = ggplot2::element_text(size = 5, colour = text_colour,
#                                                      hjust = 0, vjust = 0.5),
#                  axis.text.x = ggplot2::element_text(size = 5, colour = text_colour,
#                                                      angle=90,
#                                                      hjust = 0, vjust = 1),
#                  axis.title.x = ggplot2::element_text(size = 10, colour = text_colour),
#                  axis.title.y = ggplot2::element_text(size = 10, colour = text_colour),
#                  legend.title = ggplot2::element_blank(),
#                  legend.key.size = ggplot2::unit(0.5, "lines"),
#                  legend.margin = ggplot2::margin(t = 0, unit='cm'),
#                  legend.text = ggplot2::element_text(size = 10,
#                                                      colour = text_colour)) +
#   # legend.position = c(0.991, 0),
#   # legend.justification = c(0, 0),
#   # plot.margin = ggplot2::unit(c(1,11,1,1), "lines")) +
#   # ggplot2::coord_cartesian(clip = "off", xlim = c(1, 70)) +
#   # ggplot2::annotate("text",
#   #                   x = 70, y = 66,
#   #                   label = paste("Police force:", get_pf_name(police_force),
#   #                                 "\nData series:", get_ds_name(data_series),
#   #                                 "\nPeriod:",
#   #                                 month_min, "to", month_max, sep = " "),
#   #                   colour = text_colour,
# #                   size = 3.75,
# #                   hjust = 0, vjust = 1) + # 0 is left/top aligned, 0.5 centered, 1 right/bottom
# # ggplot2::annotate("segment", x = 0.5, xend = 68.5, y = 0.5, yend = 0.5,
# #                   colour = text_colour, linewidth = 0.3) +
# # ggplot2::annotate("segment", x = 0.5, xend = 68.5, y = 66.5, yend = 66.5,
# #                   colour = text_colour, linewidth = 0.3) +
# # ggplot2::annotate("segment", x = 0.5, xend = 0.5, y = 0.5, yend = 66.5,
# #                   colour = text_colour, linewidth = 0.3) +
# # ggplot2::annotate("segment", x = 68.5, xend = 68.5, y = 0.5, yend = 66.5,
# #                   colour = text_colour, linewidth = 0.3)
# ggplot2::annotate("rect", xmin = 0.5, xmax = 68.5, ymin = 0.5, ymax = 66.5, alpha=1, color=text_colour, fill=NA,
#                   linewidth = 0.2) +
#   ggplot2::labs(title = "Data series: stop & search\nTime period: 2019-01 to 2024-06")
#
# report
#
#
# # wiltshire, metropolitan and wm make a nice three-graph example.
# check <- fs_graph(police_force = "west-midlands", data_series = "stop-and-search", month_min = "2019-01", month_max = "2024-06")
# # # # # # #
# check
#

# check <- paste("Police force:", get_pf_name("wiltshire"),
#                "\nData series:", get_ds_name("outcomes"),
#                "\nData presence/absence for the period",
#                "2019-01", "to", "2024-01", sep = " ")


