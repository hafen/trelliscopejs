#' Specify a collection of input cognostics to be stored in browser localStorage
#' 
#' @param \ldots objects created by any of \code{\link{input_radio}},
#'   \code{\link{input_text}}
#' @param feedback_email optional feedback email address that input feedback can be sent to
#' @param extra_cogs optional vector of names of non-input "regular" cognostics to include in the csv output
#' @export
input_cogs <- function(...,
  feedback_email = NULL,
  extra_cogs = NULL
) {
  is_input <- unlist(lapply(list(...), function(x)
    inherits(x, "input_cog")))
  if (!all(is_input))
    stop("All 'input_cogs()' arguments must be of type 'input_cog'.",
      call. = FALSE)

  if (is.null(extra_cogs))
    extra_cogs <- list()

  res <- structure(list(...), class = c("input_cogs", "list"))
  attr(res, "feedback_email") <- feedback_email
  attr(res, "input_csv_vars") <- extra_cogs
  attr(res, "input_type") <- "localStorage"

  res
}

#' Specify a collection of input cognostics to be stored using an API
#'
#' @param \ldots objects created by any of \code{\link{input_radio}},
#'   \code{\link{input_text}}
#' @param set_url URL of the API endpoint for setting a single input
#' @param get_url URL of the API endpoint for getting all inputs for the display
#' @param get_request_options request options for the API call to set inputs
#' @param set_request_options request options for the API call to get inputs
#' @details See [here](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch#supplying_request_options for more information about request options.
#' @export
input_cogs_api <- function(...,
  set_url,
  get_url,
  get_request_options = list(
    mode = "cors",
    method = "GET","headers" = list(
      `Content-Type` = "application/json",
      Accept = "application/json"
    )
  ),
  set_request_options = list(
    mode = "cors",
    method = "POST",
    headers = list(
      `Content-Type` = "application/json",
      Accept = "application/json"
    )
  )
) {
  is_input <- unlist(lapply(list(...), function(x)
    inherits(x, "input_cog")))
  if (!all(is_input))
    stop("All 'input_cogs()' arguments must be of type 'input_cog'.",
      call. = FALSE)

  input_api <- list(
    get = get_url,
    set = set_url,
    getRequestOptions = get_request_options,
    setRequestOptions = set_request_options
  )

  res <- structure(list(...), class = c("input_cogs", "", "list"))
  attr(res, "input_api") <- input_api
  attr(res, "input_type") <- "API"

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

#' Specify a text input
#' 
#' @param name name of the input
#' @param desc optional description of the input
#' @param width width (in characters) of the text box popout
#' @param height height (in lines of text) of the text box popout
#' @param group optional categorization of the input for organizational purposes in the viewer (currently not implemented in the viewer)
#' @param default_label should this input be shown under the panel in the viewer by default?
#' @export
input_text <- function(name, desc = NULL, width = 80, height = 3,
  group = NULL, default_label = TRUE) {

  structure(list(
    name = name,
    desc = ifelse(is.null(desc), name, desc),
    type = "input_text",
    width = width,
    height = height,
    group = ifelse(is.null(group), "input", group),
    defLabel = default_label,
    defActive = TRUE,
    filterable = FALSE
  ), class = c("input_cog", "input_text", "list"))
}
