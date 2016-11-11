## functions to check and help specify state
## (lots to do here and all subject to change...)
##---------------------------------------------------------

#' Specify how a display should be sorted
#'
#' @param name variable name to sort on
#' @param dir direction to sort ('asc' or 'desc')
#' @export
sort_spec <- function(name, dir = "asc") {
  list(name = name, dir = dir)
}
