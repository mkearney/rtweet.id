#' Screen name to user ID
#'
#' Convert screen names to user IDs
#'
#' @param Input vector of screen names
#' @return Vector of user IDs
#' @export
sn2id <- function(x) {
  do.call("rbind", dapr::lap(x, sn2id_))
}

sn2id_ <- function(x) {
  o <- tryCatch(
    suppressWarnings(scan(paste0("https://twitter.com/", x), character(),
      skip = 1005, nmax = 4, quiet = TRUE)),
    error = function(e) tibble::tibble(user_id = NA_character_, screen_name = x))
  if (is.data.frame(o)) {
    return(o)
  }
  tibble::tibble(user_id = sub("\">", "", sub("data-user-id=\"", "", o[4], fixed = TRUE),
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
  do.call("rbind", dapr::lap(x, id2sn_))
}

id2sn_ <- function(x) {
  o <- tryCatch(
    suppressWarnings(scan(paste0("https://twitter.com/intent/user?user_id=", x),
      character(), skip = 8, nmax = 10, quiet = TRUE)),
    error = function(e) tibble::tibble(user_id = x, name = NA_character_,
      screen_name = NA_character_))
  if (is.data.frame(o)) {
    return(o)
  }
  o <- paste(o, collapse = " ")
  tibble::tibble(user_id = x,
    name = tfse::regmatches_first(o, "(?<=title>)[^(]+(?= \\()"),
    screen_name = tfse::regmatches_first(o, "(?<=\\(@)\\S+(?=\\))")
  )
}

#' Status ID to screen name
#'
#' Converts Twitter status ID to screen name
#'
#' @param x Input status ID
#' @return Screen name
#' @export
sid2sn <- function(x) {
  do.call("rbind", dapr::lap(x, sid2sn_))
}

sid2sn_ <- function(twid) {
  o <- tryCatch(
    httr::HEAD(paste0("https://twitter.com/NA/status/", twid))[["url"]],
    error = function(e) tibble::tibble(screen_name = NA_character_))
  if (is.data.frame(o)) {
    return(o)
  }
  o <- sub("https://twitter.com/", "", o, fixed = TRUE)
  do.call("rbind", sn2id(sub("/.*", "", o)))
}
