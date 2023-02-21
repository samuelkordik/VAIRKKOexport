#' Finds all CSV files with local paths
#'
#' Looks for all CSV files in project directory that have columns with a path
#' that is local (aka not relative), then adds columns and saves out as a new file.
#'
#' @return list of CSV files to be filtered
#' @export
#'
add_relative_paths <- function() {
  file_list <- fs::dir_ls(path = here::here(),
                          recurse = TRUE,
                          type = "file",
                          glob = "*.csv")

  map(file_list, add_rel_paths_table)
}

add_rel_paths_table <- function(filename, confidence = .8) {
  table <- read_csv(filename)

  # check to see if it needs
  table %>% summarise(across(where(is_character), ~ mean(fs::is_file(.x),na.rm=TRUE))) %>% pivot_longer(everything(), names_to = "col", values_to = "is_file_mean") %>%
    filter(is_file_mean > confidence) %>% count() -> file_path_count

  if (file_path_count > 0) {
    rel_filename <- paste0(fs::path_ext_remove(filename),"_relative.csv")
    table %>% mutate(across(where(~ is_character(.x) && mean(fs::is_file(.x), na.rm=TRUE) > .8), ~stringr::str_remove(.x, paste0(here::here(),"/")), .names = "{.col}_relative")) %>%
      write_csv(rel_filename)
    return(rel_filename)
  } else {
    return(NA)
  }
}
