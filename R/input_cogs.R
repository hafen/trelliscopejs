#' Specify a collection of input cognostics
#' 
#' @param \ldots objects created by any of \code{\link{input_radio}},
#'   \code{\link{input_text}}, \code{\link{input_textarea}}
#' @param feedback_email optional feedback email address that input feedback can be sent to
#' @export
input_cogs <- function(..., feedback_email = NULL) {
  is_input <- unlist(lapply(list(...), function(x)
    inherits(x, "input_cog")))
  if (!all(is_input))
    stop("All 'input_cogs()' arguments must be of type 'input_cog'.",
      call. = FALSE)
  
  res <- structure(list(...), class = c("input_cogs", "list"))
  attr(res, "feedback_email") <- feedback_email

  res
}

#' Specify a radio button input
#' 
#' @param name name of the input
#' @param desc optional description of the input
#' @param options a character vector of options to select between
#' @param group optional categorization of the input for organizational purposes in the viewer (currently not implemented in the viewer)
#' @param default_label should this input be shown under the panel in the viewer by default?
#' @export
input_radio <- function(name, desc = NULL, options, group = NULL,
  default_label = TRUE) {

  structure(list(
    name = name,
    desc = ifelse(is.null(desc), name, desc),
    type = "input_radio",
    options = options,
    group = ifelse(is.null(group), "input", group),
    defLabel = default_label,
    defActive = TRUE,
    filterable = FALSE
  ), class = c("input_cog", "input_radio", "list"))
  }
