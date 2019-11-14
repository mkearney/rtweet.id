#' Screen name to user ID
#'
#' Convert screen names to user IDs
#'
#' @param Input vector of screen names
#' @return Vector of user IDs
#' @export
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

#' User ID to screen name
#'
#' Convert user IDs to screen names
#'
#' @param Input vector of user IDs
#' @return Vector of screen names
#' @export
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

#' Status ID to screen name
#'
#' Converts Twitter status ID to screen name
#'
#' @param twid Input status ID
#' @return Screen name
#' @export
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
