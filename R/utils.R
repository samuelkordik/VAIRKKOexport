progress_walk <- function(.x, .f, ...) {

  pb <- progress::progress_bar$new(total = length(.x))
  purrr::walk(.x, ~{pb$tick(); .f(.x,...)})
}

