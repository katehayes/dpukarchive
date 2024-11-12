
arch_extract <- function(police_force = ".", data_series = ".", month_min = ".", month_max = ".") {

  op <- archive_contents %>%
    filter(police == police_force, series == data_series) %>%
    filter(year_month >= month_min,
           year_month <= month_max) %>%
    filter(contains == T) %>%
    select(file_name, folder)

  nm <- op %>%
    distinct(folder)

  op <- op %>%
    mutate(count = 1) %>%
    pivot_wider(names_from = folder,
                values_from = count,
                values_fill = 0)  %>%
    column_to_rownames(var="file_name")

  # # https://math.mit.edu/~goemans/18434S06/setcover-tamara.pdf
  # # https://stackoverflow.com/questions/39184257/set-cover-approximation-in-r
  # # incidence matrix  (each row corresponds to an element and each column corresponds to a set)

  solution <- lp("min", rep(1, ncol(op)), op, rep(">=", nrow(op)), rep(1, nrow(op)))$solution

  zip_list <- op %>%
    rownames_to_column("file_name") %>%
    pivot_longer(!file_name,
                 names_to = "folder",
                 values_to = "present") %>%
    left_join(nm %>%
                cbind(solution)) %>%
    filter(solution == 1) %>%
    select(!solution) %>%
    group_by(file_name) %>%
    mutate(selected = if_else(folder == max(folder[present == 1]), 1, 0)) %>%
    ungroup() %>%
    filter(selected == 1)  %>%
    select(!c(present, selected)) %>%
    mutate(folder = substr(folder, 4, nchar(folder)),
           folder = paste("https://data.police.uk/data/archive/", folder, ".zip", sep = ""),
           file_name = paste(substr(file_name, 1, 7), file_name, sep = "/")) %>%
    # unstack(file_name ~ folder)
    with(split(file_name,
               factor(folder, levels = unique(folder))))


  data_list <- list()


  for (i in 1:length(zip_list)) {

    temp <- tempfile()

    download.file(names(zip_list)[i], temp)

    data_list[[i]] <- bind_rows(lapply(zip_list[[i]], function(fn) read_csv(unz(temp, fn), col_types = cols(Latitude = col_double(), Longitude = col_double()))))

  }

  df <- bind_rows(data_list)

  return(df)

}

# check <- arch_extract(police_force = "west-midlands", data_series = "stop-and-search", month_min = "2015-01", month_max = "2023-08")
