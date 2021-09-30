#' Downloads all iForms
#'
#' Provided with a path for downloading the iForms, \code{download_iforms} will
#' reate a timestamped directory for the export, download the "Print" version of
#' all iForms and save as a clean HTML file (no scripts, etc.) in directories
#' organized by Category and iForm name, save attachments in sub-directories,
#' and save a CSV master table listing all iForms, the number of attachments,
#' and their path (relative to the root path).
#'
#' Downloads the "Print" version of all iForms and saves as an html file. Saves
#' CSV master table in CSV format in root. Files are organized within parent
#' directories by category and child directories by iForm name. Files are named
#' with the ID number and iForm name.
#'
#' @param path The root destination for files to be saved in.
#' @param dir_template String naming template for the directory all exports are
#'   created in. This is passed through
#'   \code{\link[base:format.Date]{format.Date}}. See \url{https://strftime.org}
#'   for full list of date-time codes. A sensible default is set.
#' @param filename_template String naming template for iForm HTML file names.
#'   This is passed to \code{\link[glue:glue]{glue} and can access `ID`,
#'   `iForm`, and `Category` fields. Sensible default.
#' @param sanitize_replacement String passed through to
#'   \code{\link[fs:path_sanitize]{fs::path_sanitize}} to replace control and
#'   reserved characters in filenames. Defaults to "-".
#' @param start_date "From" date parameter. Either a date object or a character in Y-m-d format.
#' @param end_date "To" date parameter. Either a date object or a character in Y-m-d format.
#'
#' @details # Details This function will create an rvest session using stored
#'   authentication parameters. If not set, it will prompt in interactive mode
#'   to set those parameters. This function will overwrite any existing files
#'   without asking or notifying; using the timestamped directory for export
#'   should mitigate data loss risks but be aware. iForm names and Category
#'   names in VAIRKKO can use any character; these names are passed through
#'   \code{\link[fs:path_sanitize]{fs::path_sanitize}} to remove control
#'   characters, reserved characters, and reserved or invalid periods and
#'   spaces.
#'
#' @return Returns list of directories and files created, for logging or message
#'   purposes.
#' @export
download_iforms<- function(path,
                           start_date,
                           end_date,
                           dir_template = "%Y-%m-%d %H%M%S iForms Export",
                           filename_template = "{ID}_{iForm}.html",
                           sanitize_replacement = "-") {
  # Get session
  s <- get_session()

  # Get incidents
  iforms <- get_iforms(s, start_date, end_date)

  iforms <- iforms %>% select(ID, iForm, Category, `Days Open`, `Created By`, `Created On`, Subject, `Member Location`, `Member Department`, `Member Company`)

  # turn dates
  iforms <- iforms %>% mutate(`Created On` = lubridate::ymd(`Created On`))

  message("Creating directories…")

  fs::dir_create(path)
  root <- paste(path, format.Date(lubridate::now(), dir_template),sep="/")

  fs::dir_create(root)

  # Sanitize
  iforms %>%
    mutate(across(where(is.character), fs::path_sanitize, sanitize_replacement, .names="{.col}")) -> sanitized_iforms

  create_sub_dirs <- function(Category, iForm, root) {
    sub_path <- paste(root, Category, iForm, sep="/")
    message(sub_path)
    fs::dir_create(sub_path)
    message(glue::glue("Created {sub_path}"))
    sub_path
  }

  sanitized_iforms %>% distinct(Category, iForm) -> dir_lists

  pmap_chr(sanitized_iforms %>% distinct(Category, iForm), create_sub_dirs, root) -> dirs

  iforms_list <- sanitized_iforms %>% select(ID, Category, iForm)

  pb <- progress::progress_bar$new(total = nrow(iforms_list),
                                   format = "Saving :msg [:bar] :percent eta: :eta",
                                   clear = FALSE)

  iforms_list %>% mutate(path = "Not saved due to error", `Attachment Name` = NA_character_, `Attachment Path` = NA_character_) -> default_ret

  safe_download <- possibly(download_iform, otherwise = default_ret, quiet=TRUE)

  pmap_dfr(iforms_list, safe_download, s, root, filename_template, pb) -> save_output

  iforms %>% left_join(save_output, by=c("ID", "Category", "iForm")) -> iforms_save

  write_excel_csv(iforms_save, paste0(root, "/iForms_Table.csv"))
}

download_iform <- function(ID, Category, iForm, s, root, filename_template, pb) {
  pb$tick(0, tokens = list(msg = glue::glue("iForm ID {ID} to {Category}/{iForm}")))
  stopifnot(ID == 462073)
  s %>% session_jump_to(paste0("https://suite.vairkko.com/APP/index.cfm/iForm/iFormPrint?incidentID=", ID)) -> print_version

  file_name <- glue::glue(filename_template)
  file_path <- paste(root, Category, iForm, file_name, sep="/")

  print_version %>% read_html() %>%
    #html_element("body") %>%
    as.character() %>%
    str_remove_all(regex("<script.+?</script>",dotall=TRUE, multiline=TRUE)) %>%
    str_remove(regex('<div class="row no-print">.+?</div>', dotall=TRUE, multiline=TRUE)) %>%
    write_file(file_path)

  ret <- tribble(~ID, ~Category, ~iForm, ~path,
                 ID, Category, iForm, file_path)


  # Check for attachments
  s %>% session_jump_to(paste0("https://suite.vairkko.com/APP/index.cfm/iForm/AdminiFormShell?incidentID=", ID)) -> web_version
  web_version %>% html_elements("#datatable_tabletoolsDoc") %>% html_table() -> attach_names
  web_version %>% html_elements("#datatable_tabletoolsDoc div.btn-group li a[href^='https']") %>% html_attr("href") -> attach_links

  attach_df <- tibble(file_name = attach_names[[1]]$`Document Name`, url = attach_links)

  ret %>% mutate(attachment_count = length(attach_links)) -> ret
  attachment_path <- paste0(str_remove(file_path, regex('\\..*$')),
                            "_attachments")

  save_attachment <- function(file_name, url) {
    pb$tick(0, tokens = list(msg = glue::glue("attachment {file_name} for iForm ID {ID} to {attachment_path}")))
    s %>% session_jump_to(url) -> img

    httr::headers(img)$`content-disposition`[1] %>%
      str_extract_all("(?<=filename=)(.*)") %>%
      fs::path_sanitize() -> attachment_file_name

    attachment_file_path <- paste(attachment_path, attachment_file_name, sep="/")

    write_file(img$response$content, attachment_file_path)

    tribble(~ID, ~`Attachment Name`, ~`Attachment Path`,
            ID, attachment_file_name, attachment_file_path)
  }

  if (length(attach_links) > 0) {
    fs::dir_create(attachment_path)

    attach_default <- attach_df %>% select(`Attachment Name` = file_name) %>% mutate(ID = ID, `Attachment Path` = "Not saved, error")
    pmap_dfr(attach_df, possibly(save_attachment, otherwise = attach_default, quiet=FALSE)) -> attachments
  } else {
    tribble(~ID, ~`Attachment Name`, ~`Attachment Path`) -> attachments
  }

  ret %>% left_join(attachments, by="ID") -> ret
  pb$tick(tokens = list(msg = glue::glue("Finished iForm ID {ID} to {Category}/{iForm}")))
  ret
}

get_iforms <- function(s, start_date, end_date) {

  # pulls iforms table, ID, and information
  stopifnot(class(s) == "rvest_session")

  #TODO get closed and archived iForms

  # parse dates
  parse_date <- function(the_date) {
    stopifnot(is.character(the_date) | lubridate::is.timepoint(the_date))
    if (is.character(the_date)) {
      the_date <- lubridate::ymd(the_date)
    }
    stopifnot(!is.na(the_date))
    URLencode(format.Date(the_date, "%m/%d/%Y"), reserved=TRUE)
  }

  start_date <- parse_date(start_date)
  end_date <- parse_date(end_date)


  jurl <- glue::glue("https://suite.vairkko.com/APP/index.cfm/iForm/AdminSplash?submittedForm=1&FText=&from={start_date}&to={end_date}&FID=&Ffilt=1&__ncforminfo=DGYOTpy44UmlPVK8yMSmFQDabRQT7AeJ74lAK0fzlRee7tX5RG7lqWPhLa-6SMYw4D2T8woii9eIJCSsOTBkmxMJX1u5W_t56Hjh1mWob0Q%3D")
  message(jurl)
  s %>% session_jump_to(jurl) %>%
    html_element("#iForm_Active") %>% html_table() -> iforms

  # Get iforms
  iforms[!duplicated(as.list(iforms))] -> iforms
  iforms
}

get_iform_detail <- function(ID, s, pb) {
  pb$tick(tokens = list(ID=ID))
  s %>% session_jump_to(paste0("https://suite.vairkko.com/APP/index.cfm/iForm/AdminiFormShell?incidentID=", ID)) -> iform_detail

  # #tabdetails is details
  # #

  iform_detail %>% html_element("#tabdetails") %>%
    html_elements("section") -> details

  details <- map_dfr(details, get_iform_detail_data)

  user_questions <- iform_detail %>% html_element("#tabuser table") %>%
    html_table() %>% select(key = X1, value = X2)

  admin_questions <- iform_detail

  tibble(complaint_name = complaint_detail %>% html_element("#tabuser table tbody tr:nth-of-type(8) td:nth-of-type(2)") %>% html_text2(),
         complaint_phone = complaint_detail %>% html_element("#tabuser table tbody tr:nth-of-type(9) td:nth-of-type(2)") %>% html_text2(),
         pt_name = complaint_detail %>% html_element("#tabuser table tbody tr:nth-of-type(12) td:nth-of-type(2)") %>% html_text2()
  )
}

get_iform_detail_data <- function(section) {
  # Returns dataframe dict struct with key -> value
  tribble(
    ~key, ~value,
    section %>% html_element("label.label") %>% html_text(), section %>% html_element("label.input") %>% html_text()
  )
}
