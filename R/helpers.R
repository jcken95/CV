#  Hope to bundle this into an R package at some future date.

#' Get information about a user on the stackexchange network
#' @param user_id the user's id
#' @param site site of the stackexchange network
#' @param as_tibble convert to tibble. Defaults to \code{TRUE}
get_stack_user <- function(user_id, site = "crossvalidated", as_tibble = TRUE) {
  user_info <- httr::GET("https://api.stackexchange.com/2.2/",
                         path = glue::glue("users/{user_id}"),
                         query = list(site = site))
  user_items <- httr::content(user_info, as = "parsed")$items[[1]]
  class(user_items) = "stackexchange"
  if (as_tibble) user_items <- as_tibble.stackexchange(user_items)
  user_items
}

#' Convert obhect with class "stackexchange" to tibble
as_tibble.stackexchange = function(x) {
  df <- tibble::tibble(
    display_name = x$display_name,
    user_id = x$user_id,
    reputation = x$reputation,
    badge_gold = x$badge_counts$gold,
    badge_silver = x$badge_counts$silver,
    badge_bronze = x$badge_counts$bronze
  )
  df
}
