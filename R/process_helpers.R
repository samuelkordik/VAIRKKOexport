import_incidents <- function() {
  incidents_path <- here::here("data/incidents_export2.rds")

  incidents <- export_behavioral_incidents()
  write_rds(incidents, here::here("data/incidents_export2.rds"))

  incidents <- read_rds(incidents_path)

}

trim_strings <- function(incidents) {
  incidents %>% mutate(detail_narrative = stringr::str_trim(detail_narrative),
                       detail_visibility = stringr::str_trim(detail_visibility)) %>%
    mutate(out.incident_name = stringr::str_trunc(Title, 100))
}

create_output_columns <- function(incidents) {
  incidents %>% mutate(out.incident_date = detail_incident_date,
                       out.username = Member,
                       out.reviewer_name = `Created By`,
                       out.target_date = Updated,
                       out.internal_name = IncidentID)
}

add_action_types <- function(incidents) {
  action_types_crosswalk <- read_csv(here::here("data/Action_Types_crosswalk.csv")) %>% mutate(action_type = `Action Type`)
  incidents %>% left_join(action_types_crosswalk)
}

set_resolution_status <- function(incidents) {
  incidents %>% mutate(out.resolution = `Incident Type Resolution`) %>%
    mutate(out.status = case_when(Status == "Closed" ~ "Closed",
                                  Category == "" ~ "Initiated",
                                  Actions == 0 ~ "Opened",
                                  TRUE ~ "In Progress")) %>%
    mutate(out.resolution_details = if_else(is.na(action_text), "", paste0(str_trunc(str_trim(action_text), 986), "\nSee archived detail for full text.")))
}

adjust_categories <- function(incidents) {
  incidents %>% mutate(adj_category = case_when(str_length(Category) > 0 ~ "",
                                                str_detect(str_to_lower(Title), "kudos|kuddo|came in for co-worker|great") ~ "Praise",
                                                str_detect(str_to_lower(Title), "safety|99b") ~ "Safety Violation",
                                                str_detect(str_to_lower(Title), "fleet|backing") ~ "Vehicle Operations Violation",
                                                str_detect(str_to_lower(Title), "damag") ~ "Gross Misconduct",
                                                str_detect(str_to_lower(Title), "call off|tardy|left shift|attendance|late|no show|left work early") ~ "Attendance/Tardiness",
                                                str_detect(str_to_lower(Title), "test|disregard") ~ "test",
                                                action_type %in% c("Demotion","Termination","Oral","Suspension","Written","Verbal Warning") ~ "Policy Violation",
                                                str_detect(str_to_lower(Title), "controlled medication|narcotics") ~ "Controlled Medication Policy Violation",
                                                Category == "" ~ "Policy Violation")) -> incidents
  violations_crosswalk <- read_csv(here::here("data/violations_crosswalk.csv"))
  violations_crosswalk <- violations_crosswalk[,1:2]
  incidents %>% left_join(violations_crosswalk) %>%
    unite(col="Violation", Violation, adj_category, na.rm=TRUE, sep="")  %>%
    mutate(out.violation = Violation)
}

create_output_columns2 <- function(incidents) {
  incidents %>% mutate(out.priority = "",
                       out.signed_date = str_extract(action_read_acknowledged, "(?<=First Seen\\n)\\d+/\\d+/\\d+"),
                       out.closed_date = if_else(Status == "Open", "", action_date)
  )
}

join_detail <- function(condition, text, noun_sg, noun_pl) {
  case_when(condition > 1 ~ paste0("This incident has ", condition, " ", noun_pl, ".",
                                   "\n", text, "\n",
                                   strrep('-', 60)),
            condition == 1 ~ paste0("This incident has ", condition, " ", noun_pl, ".",
                                    "\n", text,"\n",
                                    strrep('-', 60)),
            condition == 0 ~ paste0("This incident has ", condition, " ", noun_sg, ".")
  )
}

format_extra_narrative <- function(incidents) {
  incidents %>%
    replace_na(list(Actions = 0,
                    Appeals = 0,
                    Comments = 0,
                    Files = 0,
                    Points = 0)) %>%
    mutate(composite_action = glue::glue("ACTION DETAILS: {action_type}
                                       {if_else(is.na(action_created),'Action by ',paste0(action_created, ' by '))}{action_author}
                                       {str_trim(action_text)}
                                       Action Files: {if_else(action_files == '', 'None', str_trim(action_files))}
                                       Action read/acknowledged? {action_read_acknowledged}
                                       ")) %>%
    mutate(previous_incident = if_else(str_length(detail_previous_incident)>0,
                                       paste0('\nRelated to prior incident ', detail_previous_incident),
                                       "\nNo related incidents")) %>%
    mutate(composite_narrative = glue::glue("{detail_narrative}
                                          -------------------------------------------------------------
                                          {join_detail(Actions, composite_action, 'action', 'actions')}
                                          {join_detail(Appeals, detail_appeals, 'appeal', 'appeals')}
                                          {join_detail(Comments, detail_comments, 'comment', 'comments')}
                                          Files: {Files}{previous_incident}

                                          []
                                          ")) %>%
    mutate(out.description = paste0(str_trunc(composite_narrative, 986), "\nSee archived detail for full text."))
}

set_incident_type <- function(incidents) {
  incidents %>% mutate(incident_type = case_when(action_type %in% c("Demotion", "Oral", "Suspension", "Termination", "Verbal Warning", "Written") ~ "Disciplinary",
                                                 action_type == "Kudos" ~ "Praise",
                                                 Category %in% c("Attendance and Punctuality",
                                                                 "Continuing Education",
                                                                 "Controlled Medication Documentation Error",
                                                                 "Customer Service",
                                                                 "Distractions and Attentiveness",
                                                                 "Failure to complete Station Captain Duties",
                                                                 "Incorrect Address Shipped Due to Error",
                                                                 "Inventory / Op IQ",
                                                                 "Left notes out of an incident-Put units in danger",
                                                                 "Lost Equipment",
                                                                 "Policy Violation",
                                                                 "Self-Reported Violation",
                                                                 "Specific Behavior Concern(s)",
                                                                 "Station / Unit Inspection",
                                                                 "Unprepared for shift",
                                                                 "Best Practices Deviation") ~ "Employee Investigation",
                                                 Category %in% c("Backing Policy Violation",
                                                                 "PPE Deviation",
                                                                 "Stage Policy Violation",
                                                                 "Vehicle Operations") ~ "Safety Investigation",
                                                 Category %in% c("Standard of Care",
                                                                 "Protocol Violation / Deviation") ~ "Clinical Performance Investigation",
                                                 Violation %in% c("Vehicle Operations Violation", "Safety Violation") ~ "Safety Investigation",
                                                 Category %in% c("Kudos customer recognition",
                                                                 "Kudos Peer Recognition") ~ "Praise"
                                                 ,Violation == "Praise" ~ "Praise"
                                                 ,TRUE ~ "Employee Investigation"
  ))
}

input_employee_names <- function(incidents) {
  eeo <- readxl::read_xlsx(here::here("data/VAIRKKO_EEO_Report.xlsx"))
  eeo %>% mutate(Member =  paste0(str_replace(`Last Name`, " \\(disabled\\)", ""), ", ", `First Name`)) %>%
    select(Member, SSN) -> eeo
  incidents %>% left_join(eeo) %>% mutate(SSN = if_else(SSN == "Not On File", "", SSN)) -> incidents
  addl <- readxl::read_xlsx(here::here("data/Additional Employee IDs.xlsx"))
  incidents %>%
    left_join(addl %>% select(Member = `Name:`, SSN_x = `SSN:`, `Employee Id` = `Employee ID`)) %>%
    unite("SSN", SSN, SSN_x, na.rm = TRUE, sep="") -> incidents
  incidents
}

remove_tests <- function(incidents) {
  incidents %>% filter(Member != "Person, Test", Violation != "test")
}

get_output <- function(incidents) {
  incidents %>%
    mutate(`EIN Tax Id` = "",
           `EIN Name` = "",
           `Incident Time` = "",
           `Start Date` = "",
           `Reviewer EIN Id` = "",
           `Reviewer EIN Name` = "",
           `Employee External Id` = "",
           `Reviewer Signed Date` = ""
    ) %>%
    select(Username = out.username,
           `Employee Id`,
           SSN,
           `Employee External Id`,
           `EIN Tax Id`,
           `EIN Name`,
           `Incident Type` = incident_type,
           `Incident Name` = out.incident_name,
           `Internal Name` = out.internal_name,
           `Violation` = out.violation,
           `Incident Date` = out.incident_date,
           `Incident Time`,
           `Report Date` = detail_incident_date,
           `Start Date`,
           `Status` = out.status,
           `Priority` = out.priority,
           `Description` = out.description,
           `Employee Signed Date` = out.signed_date,
           `Reviewer Name` = out.reviewer_name,
           `Reviewer EIN Tax Id` = `Reviewer EIN Id`,
           `Reviewer EIN Name`,
           `Reviewer Signed Date`,
           `Closed Date` = out.closed_date,
           `Target Date` = out.target_date,
           `Resolved Date` = out.closed_date,
           `Resolution` = out.resolution,
           `Details` = out.resolution_details
    )
}
