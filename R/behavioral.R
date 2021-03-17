
export_behavioral_incidents <- function(limit=0) {
  # First, load incidents:
  incidents <- get_behavioral_incidents()

  # limit
  if(limit>0) {
    incidents <- head(incidents, limit)
  }

  # add detail:
  incidents <- incidents %>%
    mutate(incident_details = map(IncidentID, get_incident_detail)) %>%
             unnest(incident_details)
  # Get action details
  incidents <- incidents %>%
    mutate(action_details = map(detail_actions, get_action_details)) %>%
             unnest(action_details)

  # get file info and save files:
  #process_incident_files(incident_file)

  incidents

}

get_behavioral_incidents <- function() {
  # Get session
  s <<- session("https://suite.vairkko.com/APP/index.cfm/BehaviorTracking/Dashboard")


  f <- html_form(s)[[1]]

  f <- html_form_set(f,
                     companyid = get_credential("cid"),
                     username = get_credential("username"),
                     password = get_credential("password"))

  s <<- session_submit(s, f) %>%
    session_jump_to("https://suite.vairkko.com/APP/index.cfm/BehaviorTracking/Dashboard")

  # Get incidents
  incidents <- s %>% html_element("#tableincidents") %>% html_table()
}

get_incident_detail <- function(incidentID) {
  url <- paste0("https://suite.vairkko.com/APP/index.cfm/BehaviorTracking/IncidentDetail?IncidentID=", incidentID)
  incident_detail <- s %>% session_jump_to(url)

  tibble(detail_incident_date = incident_detail %>% html_element("#IncidentDate") %>% html_attr("value"),
         detail_followers = incident_detail %>% html_elements("#FollowerID option[selected]") %>%
           html_text2()%>% paste(collapse="; "),
         detail_group_followers = incident_detail %>% html_elements("#FollowerGroupID option[selected]") %>%
           html_text2() %>% paste(collapse="; "),
         detail_categories = incident_detail %>% html_elements("#CategoryID option[selected]") %>%
           html_text2()%>% paste(collapse="; "),
         detail_previous_incident_id = incident_detail %>% html_elements("#PreviousIncidentID option[selected]") %>%
           html_attr("value"),
         detail_previous_incident = incident_detail %>% html_elements("#PreviousIncidentID option[selected]") %>%
           html_text2(),
         detail_narrative = incident_detail %>% html_element("#Detail") %>% html_text2()%>% paste(collapse="; "),
         detail_narrative_html = as.character(incident_detail %>% html_element("#Detail") %>% html_children()),
         detail_actions = incident_detail %>% html_elements("#datatable_actions tbody tr"),
         detail_visibility = incident_detail %>% html_element("#content .alert") %>% html_text(),
         detail_appeals = incident_detail %>% html_element("#datatable_appeals") %>% html_table(),
         detail_comments = incident_detail %>% html_element("#datatable_comments") %>% html_table(),
         detail_files = incident_detail %>% html_elements("#datatable_files tbody tr")
  )
}

process_incident_files <- function(incident_file) {
  file_url = incident_file %>% html_element("td:nth-of-type(3) a") %>% html_attr("href")
  file_name_split <- file_url %>% str_split("/")
  file_content <- s %>% session_jump_to(file_details$file_url[1])
  file_name <- URLdecode(paste(tail(file_name_split[[1]],2),collapse="_"))

  message(glue::glue("Saving file {file_name}"))
  writeBin(file_content$response$content, file_name)

  tibble(file_uploaded_on = incident_file %>% html_element("td:nth-of-type(2)") %>% html_text2(),
         file_url = incident_file %>% html_element("td:nth-of-type(3) a") %>% html_attr("href"),
         file_name = incident_file %>% html_element("td:nth-of-type(3)") %>% html_text2(),
         file_association = incident_file %>% html_element("td:nth-of-type(4)") %>% html_text2(),
         file_visibility = incident_file %>% html_element("td:nth-of-type(5) span") %>% html_attr("data-original-title")
  )
}

get_action_details <- function(actions) {
  tibble(action_date = actions %>% html_element("td:nth-of-type(2) span") %>% html_text2(),
                           action_created = actions %>% html_element("td:nth-of-type(2) span") %>% html_attr("title"),
                           action_author = actions %>% html_element("td:nth-of-type(2) a") %>% html_text2(),
                           action_text = actions %>% html_element("td:nth-of-type(3)") %>% html_text2(),
                           action_type = actions %>% html_element("td:nth-of-type(3) span") %>% html_text2(),
                           action_files = actions %>% html_element("td:nth-of-type(4)") %>% html_text2(),
                           action_read_acknowledged = actions %>% html_element("td:nth-of-type(8)") %>% html_text2(),
                           action_id = actions %>% html_element("td:nth-of-type(1) a") %>%
                             html_attr("onclick") %>%
                             str_extract("(?<=ActionID=)\\d+"),
                           action_url = actions %>% html_element("td:nth-of-type(1) a") %>% html_attr("onclick") %>%
                             str_extract("(?<=\\').*(?=\\')")
  )
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


