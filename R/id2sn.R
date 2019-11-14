sn2id <- function(x) {
  o <- tryCatch(
    suppressWarnings(scan(paste0("https://twitter.com/", x), character(),
      skip = 1005, nmax = 4, quiet = TRUE)),
    error = function(e) list(user_id = NA_character_, screen_name = x))
  if (is.list(o)) {
    return(o)
  }
  list(user_id = sub("\">", "", sub("data-user-id=\"", "", o[4], fixed = TRUE),
    fixed = TRUE),
    screen_name = x)
}

id2sn <- function(x) {
  o <- tryCatch(
    suppressWarnings(scan(paste0("https://twitter.com/intent/user?user_id=", x),
      character(), skip = 8, nmax = 5, quiet = TRUE)),
    error = function(e) list(user_id = x, name = NA_character_,
      screen_name = NA_character_))
  if (is.list(o)) {
    return(o)
  }
  list(user_id = x,
    name = paste(sub("<title>", "", o[1], fixed = TRUE), o[2]),
    screen_name = sub(")", "", sub("(@", "", o[3], fixed = TRUE), fixed = TRUE))
}
sid2sn <- function(twid) {
  o <- tryCatch(
    httr::HEAD(paste0("https://twitter.com/NA/status/", twid))[["url"]],
    error = function(e) list(screen_name = NA_character_))
  if (is.list(o)) {
    return(o)
  }
  o <- sub("https://twitter.com/", "", o, fixed = TRUE)
  sn2id(sub("/.*", "", o))
}
