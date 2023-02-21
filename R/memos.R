

download_memos <- function(path) {

    # Get session
  s <- get_session()

  s %>% session_jump_to("https://suite.vairkko.com/APP/index.cfm/Memos/AdminSplash?") -> memo_page
  memo_page %>%
    html_elements("#memoTable div.btn-group li a[onclick^='openiFrameFullScreen']") %>% html_attr("onclick") %>%
    str_extract("(?<=openiFrameFullScreen\\(')(.*?)'") %>% str_replace("'","") -> memo_urls

  memo_page %>% html_elements("#memoTable") %>% html_table() -> memo_table
  memo_table[[1]][,3:6] -> memo_table

  memo_table$memo_url <- memo_urls
  memo_table %>% mutate(memo_date = lubridate::mdy(`Posted On`)) -> memo_table

  memo_table %>% select(memo_title = `Title/Regarding`, memo_date, memo_url) -> download_table

  pb <- progress::progress_bar$new(total = nrow(download_table),
                                   format = "Saving :memo [:bar] :percent eta: :eta",
                                   clear = FALSE)


  safe_download <- possibly(get_memo, otherwise = NA_character_, quiet=FALSE)

  pmap_dfr(download_table, safe_download, s, pb, path) -> save_output

  memo_table %>% left_join(save_output, by="memo_url") -> saved_memos

  write_excel_csv(saved_memos, paste0(path, "/Memos_Table.csv"))

}

get_memo <- function(memo_title, memo_date, memo_url, s, pb, path) {

  pb$tick(tokens = list(memo = paste(memo_date, memo_title)))

  s %>% session_jump_to(memo_url) -> print_version
  file_path <- paste0(path, "/", format.Date(memo_date, "%Y-%m-%d"), "_", fs::path_sanitize(str_trunc(memo_title, 30), replacement = "-"), ".html")

  print_version %>% read_html() %>%
    as.character() %>%
    str_remove_all(regex("<script.+?</script>",dotall=TRUE, multiline=TRUE)) %>%
    str_remove(regex('<div class="row no-print">.+?</div>', dotall=TRUE, multiline=TRUE)) %>%
    str_remove(regex('div id="fullScreenFancyBoxHeader">.+?(?=</body>)', dotall=TRUE, multiline=TRUE)) %>%
    #cat()
    write_file(file_path)

  tribble(~memo_url, ~file_path,
          memo_url, file_path)
}
