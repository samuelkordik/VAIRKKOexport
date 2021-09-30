#' Stores Login Credentials
#'
#' Stores login credentials to VAIRKKO securely on the system keyring.
#' See ?keyring for details on system credential stores.
#'
#' @param static TRUE only displays the results without prompting for updates unless none is found.
#'
#' @return
#' @export
update_authentication <- function(static = FALSE) {

  user_key <- keyring::key_list("VAIRKKO_user")
  cid_key <- keyring::key_list("VAIRKKO_cid")

  user_key <- paste(user_key$username, collapse = "")
  cid_key <- paste(cid_key$username, collapse = "")

  # First time here?
  if (stringr::str_length(cid_key) == 0 | stringr::str_length(user_key) == 0) {
    if (interactive()) {
      print("No VAIRKKO credentials found!")
      get_new_credentials()
    } else {
      print("No VAIRKKO credentials found! Run update_authentication() in console to store credentials.")
    }

  } else {

    if (interactive() & !static) {
    do_change <- readline(prompt=glue::glue("Current VAIRKKO credentials:
                                            ----------------------------
                                            Company ID: {cid_key}
                                            Username: {user_key}
                                            ----------------------------
                                            Change this? (Yes/no/cancel)"))
    switch(str_to_upper(do_change),
           YES = {
             get_new_credentials()
           },
           NO = {
             print("Keeping existing credentials.")
           },
           CANCEL = {
             stop("User cancelled")
           })
    } else {
      print(glue::glue("Current VAIRKKO credentials:
                        ----------------------------
                        Company ID: {cid_key}
                        Username: {user_key}
                        ----------------------------
                       Run update_authentication() in console to store credentials."))
    }
  }
}

get_new_credentials <- function() {
  cid <- readline(prompt="Enter VAIRKKO company ID: ")
  username <- readline(prompt="Enter VAIRKKO username: ")
  password <- askpass::askpass(prompt="Enter your VAIRKKO password: ")

  store_authentication(cid, username, password)
}

#' Stores authentication
#'
#' @param cid Company ID
#' @param username Username
#' @param password Password
#'
#' @return
store_authentication <- function(cid, username, password) {
  keyring::key_set_with_value("VAIRKKO_user",username, password)
  keyring::key_set_with_value("VAIRKKO_cid", cid, cid)
}

get_credential <- function(credential = c("cid","username","password")) {

  switch(credential,
         cid = {
           cred <- keyring::key_list("VAIRKKO_cid")
           cred <- paste(cred$username, collapse="")
         },
         username = {
           cred <- keyring::key_list("VAIRKKO_user")
           cred <- paste(cred$username, collapse="")
         },
         password = {
           username <- keyring::key_list("VAIRKKO_user")
           cred <- keyring::key_get("VAIRKKO_user", username=paste(username$username, collapse=""))
         }
         )
  if (str_length(cred) == 0) {
    if (interactive()) {
      update_authentication()
    } else {
      stop("No credentials found in keyring. Use update_authentication() to set credentials.")
    }
  }

  cred
}

get_session <- function() {
  # Get session
  s <- session("https://suite.vairkko.com/APP/index.cfm/BehaviorTracking/Dashboard")


  f <- html_form(s)[[1]]

  f <- html_form_set(f,
                     companyid = get_credential("cid"),
                     username = get_credential("username"),
                     password = get_credential("password"))

  s <- session_submit(s, f) %>%
    session_jump_to("https://suite.vairkko.com/APP/index.cfm/BehaviorTracking/Dashboard")
  s
}


