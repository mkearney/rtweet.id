
#' Count follows
#'
#' Counts number of follow decisions
#'
#' @param x Friend networks in the form of a recursive list where each element
#'   contains a vector of friends (user IDs)
#' @return Counts represent the number of follow decisions. The first count
#'   will always be missing.
#' @export
new_follows <- function(x) {
  n <- length(x) - 1L
  o <- vector("integer", length(x))
  o[1] <- NA_integer_
  for (i in seq_len(n)) {
    o[i + 1L] <- sum(!unlist(x[[i + 1L]]) %in% unlist(x[[i]]))
  }
  o
}


#' Count unfollows
#'
#' Counts number of unfollow decisions
#'
#' @param x Friend networks in the form of a recursive list where each element
#'   contains a vector of friends (user IDs)
#' @return Counts represent the number of unfollow decisions. The first count
#'   will always be missing.
#' @export
new_unfollows <- function(x) {
  n <- length(x) - 1L
  o <- vector("integer", length(x))
  o[1] <- NA_integer_
  for (i in seq_len(n)) {
    o[i + 1L] <- sum(!unlist(x[[i]]) %in% unlist(x[[i + 1L]]))
  }
  o
}


#' Identify unfollowed accounts
#'
#' Returns user identifiers of unfollowed decisions
#'
#' @param x Friend networks in the form of a recursive list where each element
#'   contains a vector of friends (user IDs)
#' @return A list where each element contains the user IDs of accounts
#'   unfollowed (compared to the previous observation).
#' @export
who_unfollowed <- function(x) {
  u <- vector("list", length(x))
  for (i in seq_along(x)) {
    if (i == 1L) {
      u[[i]] <- character()
    } else {
      u[[i]] <- unlist(x[[i - 1L]])[!unlist(x[[i - 1L]]) %in% unlist(x[[i]])]
    }
  }
  u
}

