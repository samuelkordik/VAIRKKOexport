#' Downloads Complaints as HTML files
#'
#' Downloads the "Print" version of all complaints and saves as an html file
#'
#' @param path destination for files to be saved in.
#'
#' @return
#' @export
#'
#' @examples
download_all_complaints_prints <- function(path) {
  # Get session
  s <- get_session()

  # Get incidents
  complaints <- get_complaints(s)

  pb <- progress::progress_bar$new(total = nrow(incidents),
                                   format = "Saving complaint :ID [:bar] :percent eta: :eta",
                                   clear = FALSE)
  #path <- here::here("archives")
  write_excel_csv(complaints, here::here(path, "Complaints_Table.csv"))
  walk(complaints$ID, download_print_complaint, s, path, pb)

}

download_print_complaint <- function(ID, s, path, pb) {
  #message(glue::glue("Saving complaint {ID} to {path}"))
  pb$tick(tokens = list(ID = ID))
  s %>% session_jump_to(paste0("https://suite.vairkko.com/APP/index.cfm/iForm/iFormPrint?incidentID=", ID)) -> print_version
  print_version %>% read_html() %>%
    #html_element("body") %>%
    as.character() %>%
    str_remove_all(regex("<script.+?</script>",dotall=TRUE, multiline=TRUE)) %>%
    str_remove(regex('<div class="row no-print">.+?</div>', dotall=TRUE, multiline=TRUE)) %>%
    #cat()
    write_file(paste0(path, "/iFORM_", ID, ".html"))
}

get_complaints <- function(s) {
  stopifnot(class(s) == "rvest_session")
  jurl <- "https://suite.vairkko.com/APP/index.cfm/iForm/AdminSplash?submittedForm=1&FText=&from=08%2F01%2F2015&to=08%2F31%2F2021&FID=&Ffilt=1&__ncforminfo=DGYOTpy44UmlPVK8yMSmFQDabRQT7AeJ74lAK0fzlRee7tX5RG7lqWPhLa-6SMYw4D2T8woii9eIJCSsOTBkmxMJX1u5W_t56Hjh1mWob0Q%3D"
  s %>% session_jump_to(jurl) %>%
    html_element("#iForm_Active") %>% html_table() -> complaints

  # Get incidents
  complaints[!duplicated(as.list(complaints))] -> complaints
  complaints
}

get_complaint_detail <- function(ID, s, pb) {
  pb$tick(tokens = list(ID=ID))
  s %>% session_jump_to(paste0("https://suite.vairkko.com/APP/index.cfm/iForm/AdminiFormShell?incidentID=", ID)) -> complaint_detail


  tibble(complaint_name = complaint_detail %>% html_element("#tabuser table tbody tr:nth-of-type(8) td:nth-of-type(2)") %>% html_text2(),
         complaint_phone = complaint_detail %>% html_element("#tabuser table tbody tr:nth-of-type(9) td:nth-of-type(2)") %>% html_text2(),
         pt_name = complaint_detail %>% html_element("#tabuser table tbody tr:nth-of-type(12) td:nth-of-type(2)") %>% html_text2()
  )
}

export_all_complaints <- function(path) {
  # Create session
  s <- get_session()
  stopifnot(class(s) == "rvest_session")

  # First, load incidents:
  complaints <- get_complaints(s)
  complaints %>% select(-1, -2) -> complaints

  message(glue::glue("Scraped {nrow(complaints)} complaints."))


  # add detail:
  pb <- progress::progress_bar$new(total = nrow(complaints),
                                   format = "Getting details for :ID [:bar] :percent eta: :eta",
                                   clear = FALSE)
  complaints <- complaints %>%
    mutate(complaint_details = map(ID, get_complaint_detail, s, pb))



  complaints %>%
    unnest(complaint_details, names_repair = "universal" ) -> complaints

  the_words <- c(complaints$incident_id, complaints$complaint_name, complaints$complaint_phone, complaints$pt_name)

  unique(the_words) -> the_words
  clipr::write_clip(the_words)
  }
