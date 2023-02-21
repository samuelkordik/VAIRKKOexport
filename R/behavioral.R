
#' Exports Behavioral Incidents
#'
#' Retrieves behavioral incidents, with nested tibbles for heirarchal internal data.
#'
#' @param limit optional for how many incidents to limit scraping details to.
#'
#' @return Tibble of incidents
#' @export
#'
export_behavioral_incidents <- function(limit = NULL) {

  # Create session
  s <- get_session()
  stopifnot(class(s) == "rvest_session")

  # First, load incidents:
  incidents <- get_behavioral_incidents(s)

  stopifnot(any(class(incidents) == "tbl_df"))

  stopifnot(all.equal(colnames(incidents),
                      c("Status",
                           "Incident Date",
                           "Created By",
                           "Updated",
                           "Member",
                           "Title",
                           "Category",
                           "Actions",
                           "Appeals",
                           "Comments",
                           "Files",
                           "Points",
                        "IncidentID")))


  message(glue::glue("Scraped {nrow(incidents)} behavioral incidents."))

  # limit
  if (!is.null(limit)) {
    incidents <- head(incidents, limit)
    message(glue::glue("Limiting to first {limit} incidents"))
  }

  # add detail:
  pb <- progress::progress_bar$new(total = nrow(incidents),
                                   format = "Getting details for :incidentid [:bar] :percent eta: :eta",
                                   clear = FALSE)
  incidents <- incidents %>%
    mutate(incident_details = map(IncidentID, get_incident_detail, s, pb))


  incidents %>%
    unnest(incident_details) %>%
    unnest(detail_actions) %>%
    mutate(detail_narrative = str_trim(detail_narrative),
           detail_visibility = str_trim(detail_visibility))

}

get_behavioral_incidents <- function(s) {
  stopifnot(class(s) == "rvest_session")

  s %>% session_jump_to("https://suite.vairkko.com/APP/index.cfm/BehaviorTracking/Incidents") %>%
    html_element("#tableIncidentsResults") #>% html_table() -> incidents


  fast# Get incidents
  incidents[!duplicated(as.list(incidents))] -> incidents
  incidents %>% replace_na(list(Actions = 0, Appeals = 0, Comments = 0, Files = 0)) -> incidents
  incidents %>% mutate(IncidentID = str_extract(Title, "^\\d+?(?=:)"))
}

get_incident_detail <- function(incidentID, s, pb) {
  stopifnot(class(s) == "rvest_session")

  pb$tick(tokens = list(incidentid = incidentID))
  url <- paste0("https://suite.vairkko.com/APP/index.cfm/BehaviorTracking/IncidentDetail?IncidentID=", incidentID)
  incident_detail <- s %>% session_jump_to(url)

  detail_actions <- incident_detail %>% html_elements("#datatable_actions tbody tr") %>% get_action_details()

  if (nrow(detail_actions) == 0) {
    detail_actions <- detail_actions %>% add_row()
  }

  detail_files <- incident_detail %>% html_elements("#datatable_files tbody tr") %>% get_incident_files()

  if (nrow(detail_files) == 0) {
    detail_files <- detail_files %>% add_row()
  }

  incident_detail %>% html_elements("button.btn-primary") %>% html_attr("onclick") -> close_open
  closed <- str_detect(close_open, "reopenIncident") %>% any() %>% replace_na(FALSE)
  open <- str_detect(close_open, "closeoutIncident") %>% any() %>% replace_na(FALSE)

  stopifnot(open != closed)

  tibble(detail_incident_date = incident_detail %>% html_element("#IncidentDate") %>% html_attr("value"),
         detail_status = if_else(closed, "Closed", "Open"),
         detail_followers = incident_detail %>% html_elements("#FollowerID option[selected]") %>%
           html_text2()%>% paste(collapse="; "),
         detail_group_followers = incident_detail %>% html_elements("#FollowerGroupID option[selected]") %>%
           html_text2() %>% paste(collapse="; "),
         detail_categories = incident_detail %>% html_elements("#CategoryID option[selected]") %>%
           html_text2()%>% paste(collapse="; "),
         detail_previous_incident_id = incident_detail %>% html_elements("#PreviousIncidentID option[selected]") %>%
           html_attr("value") %>% paste(collapse="; "),
         detail_previous_incident = incident_detail %>% html_elements("#PreviousIncidentID option[selected]") %>%
           html_text2() %>% paste(collapse="; "),
         detail_narrative = incident_detail %>% html_element("#Detail") %>% html_text2()%>% paste(collapse="; "),
         detail_narrative_html = as.character(incident_detail %>% html_element("#Detail") %>% html_children()) %>% paste(collapse="\n"),
         detail_visibility = incident_detail %>% html_element("#content .alert") %>% html_text(),
         detail_appeals = incident_detail %>% html_element("#datatable_appeals") %>% get_appeals_details(),
         detail_comments = incident_detail %>% html_element("#datatable_comments") %>% get_comments_details()) %>%
    mutate(detail_actions = detail_actions%>% nest(data=everything()) %>% pull(data),
           detail_files = detail_files %>% nest(data=everything()) %>% pull(data))

}

get_incident_files <- function(incident_file) {
  tibble(file_uploaded_on = incident_file %>% html_element("td:nth-of-type(2)") %>% html_text2(),
         file_url = incident_file %>% html_element("td:nth-of-type(3) a") %>% html_attr("href"),
         file_name = incident_file %>% html_element("td:nth-of-type(3)") %>% html_text2(),
         file_association = incident_file %>% html_element("td:nth-of-type(4)") %>% html_text2(),
         file_visibility = incident_file %>% html_element("td:nth-of-type(5) span") %>% html_attr("data-original-title")
  )
}

#' Downloads incident files
#'
#' Walks through incident files list column and saves files to designated path.
#'
#' @param incidents Incidents tibble
#' @param path Path to save files to
#'
#' @return returns input invisibly
#' @export
download_incident_files <- function(incidents, path) {

  stopifnot(any(class(incidents) == "tbl_df"))

  incident_files <- incidents %>% select(detail_files) %>% unnest(detail_files) %>% drop_na()

  stopifnot(any(colnames(incident_files) == "file_url"))
  stopifnot(all(fs::dir_exists(path)))

  # Restart session
  if (exists("s")) rm(s)
  s <- get_session()

  message(glue::glue("Saving {nrow(incident_files)} files to {path}"))

  pb <- progress::progress_bar$new(total = nrow(incident_files),
                                   format = "Downloading :file [:bar] :percent eta: :eta",
                                   clear = FALSE)

  walk(incident_files$file_url, download_file, path, s, pb)
}

dedup_files <- function(file_name, path) {
  file_path <- paste0(path, "/", file_name)
  i <- 2
  while (fs::file_exists(file_path)) {
    file_path <- paste0(path, "/", i, "_",file_name)
    i <- i+1
  }
file_path
}

download_file <- function(file_url, path, s, pb) {
  stopifnot(class(s) == "rvest_session")

  if (!is.na(file_url)) {
    file_name_split <- file_url %>% str_split("/")
    file_content <- s %>% session_jump_to(file_url)
    file_name <- URLdecode(paste(tail(file_name_split[[1]],2),collapse="_"))
    file_path <- dedup_files(file_name, path)
    pb$tick(tokens = list(file = file_name))
    writeBin(file_content$response$content, file_path)
  }
}

get_action_details <- function(actions) {
  tibble(action_date = actions %>% html_element("td:nth-of-type(2) span") %>% html_text2(),
                           action_created = actions %>% html_element("td:nth-of-type(2) span") %>% html_attr("title"),
                           action_author = actions %>% html_element("td:nth-of-type(2) a") %>% html_text2(),
                           action_text = actions %>% html_element("td:nth-of-type(3)") %>% html_text2(),
                           action_type = actions %>% html_element("td:nth-of-type(3) span.label") %>% html_text2(),
                           action_files = actions %>% html_element("td:nth-of-type(4)") %>% html_text2(),
                           action_read_acknowledged = actions %>% html_element("td:nth-of-type(8)") %>% html_text2(),
                           action_id = actions %>% html_element("td:nth-of-type(1) a") %>%
                             html_attr("onclick") %>%
                             str_extract("(?<=ActionID=)\\d+"),
                           action_url = actions %>% html_element("td:nth-of-type(1) a") %>% html_attr("onclick") %>%
                             str_extract("(?<=\\').*(?=\\')")
  )
}

get_appeals_details <- function(appeals) {
  appeals <- appeals %>% html_table()

  appeals_summary <- glue::glue("Appeal created: {appeals$Created}
                                        Appeal detail: {appeals$Detail}
                                        Appeal read/acknowledged: {appeals$`Read/Acknowledged`}")
  paste0(appeals_summary, collapse = "\n\n")

}

get_comments_details <- function(comments) {
  comments <- comments %>% html_table()

  glue::glue("Comment Created: {comments$Created}
             {comments$Comment}
             Files: {comments$Files}") %>%
    paste0(collapse = "\n\n")


}

# Not needed due to VAIRKKO platform error
# get_extra_action_details <- function(action_url) {
#   action_detail <- s %>% session_jump_to(action_url)
#   tibble(action_title = action_detail %>% html_element("#actiontitle") %>% html_attr("value"),
#                           action_type = action_detail %>% html_element("#ActionTypeID option[selected]") %>% html_text2(),
#                           action_narrative = action_detail %>% html_element("#ActionDetail") %>% html_text2(),
#                           expectations = action_detail %>% html_element("#Expectations") %>% html_text2(),
#                           consequences = action_detail %>% html_element("#Consequences") %>% html_text2(),
#                           coaching = action_detail %>% html_element("#CoachingOffered") %>% html_text2()
#   )
# }


