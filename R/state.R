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

# #' Specify how a display should be filtered
# #'
# #' @param name variable name to sort on
# #' @param type either "select" or "regex"
# #' @param value If \code{type} is "select", a vector of values to select. If \code{type} is "regex", a string indicating a regular expression
# filter_cat_spec <- function(name, type = c("select", "regex"), value) {
#   if (type == "regex" && length(value) > 1)
#     stop_nice("If specifying a filter of type 'regex', the value must be a string.")
#   list(name = name, type = type, value = value)
# }

# #' Specify how a display should be filtered
# #'
# #' @param name variable name to sort on
# #' @param type either "select" or "regex"
# #' @param value If \code{type} is "select", a vector of values to select. If \code{type} is "regex", a string indicating a regular expression
# filter_num_spec <- function(name, from = NA, to = NA) {
#   list(name = name, from = from, to = to)
# }
