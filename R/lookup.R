#' Lookup screen names
#'
#' Returns screen names for any user IDs matched to internal users dataset
#'
#' @param x Input vector of Twitter user IDs
#' @return A vector of screen names (NAs returned for non-matches)
#' @export
lookup_screen_name <- function(x) {
  users_handles$screen_name[match(x, users_handles$user_id)]
}

#' Lookup user IDs
#'
#' Returns user IDs for any screen names matched to internal users dataset
#'
#' @param x Input vector of Twitter screen names
#' @return A vector of user IDs (NAs returned for non-matches)
#' @export
lookup_user_id <- function(x) {
  ## create lookup vectors that include all-lowercase version
  muisn <- luvalv(users_handles$user_id, users_handles$screen_name)
  muisn$sn[match(x, muisn$ui)]
}

#' Lookup bioguide ID
#'
#' Converts user IDs into Congressional bioguide IDs.
#'
#' @param x Input vector of user IDs
#' @param using A data frame lookup dictionary with 'member_user_id' and
#'   'bioguide' columns
#' @return Vector of bioguide IDs (NAs returned for non-matched values)
#' @export
lookup_bioguide <- function(x, using) {
  using$bioguide[match(x, using[["member_user_id"]])]
}


luvalv <- function(ui, sn) {
  match_sn <- c(sn, tolower(sn))
  list(ui = c(ui, ui)[!duplicated(match_sn)],
    sn = unique(match_sn))
}




