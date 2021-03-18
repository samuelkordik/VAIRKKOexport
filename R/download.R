#' Downloads Behavioral Incidents as HTML files
#'
#' Downloads the "Print" version of all incidents and saves as an html file
#'
#' @param path destination for files to be saved in.
#'
#' @export
#'
download_all_incident_prints <- function(path) {
  # Get session
  s <- get_session() %>%
    session_jump_to("https://suite.vairkko.com/APP/index.cfm/BehaviorTracking/Dashboard")

  # Get incidents
  incidents <- s %>% html_element("#tableincidents") %>% html_table()
  incidents[!duplicated(as.list(incidents))]



}

download_print_version <- function(incidentId, s, path) {
  s %>% session_jump_to(paste0("https://suite.vairkko.com/APP/index.cfm/BehaviorTracking/Print?IncidentID=", incidentId)) -> print_version
  print_version %>% read_html() %>%
    #html_element("body") %>%
    as.character() %>%
    str_remove_all(regex("<script.+?</script>",dotall=TRUE, multiline=TRUE)) %>%
    str_remove(regex('<div class="row no-print">.+?</div>', dotall=TRUE, multiline=TRUE)) %>%
    #cat()
    write_file(paste0(path, "Incident ", incidentId, ".html"))
}
