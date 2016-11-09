trelliscope_widget <- function(id, url, width = NULL, height = NULL,
  www_dir = tempfile("viewhtml")) {

  # TODO: warn if width or height are too small

  # if (!grepl("^https?://", url))
  #   url <- paste0("file://", normalizePath(url))

  # forward options using x
  x <- list(
    id = id,
    url = url
  )

  # create widget
  htmlwidgets::createWidget(
    name = "TrelliscopeJS",
    x,
    width = width,
    height = height,
    package = "trelliscopecore",
    sizingPolicy = htmlwidgets::sizingPolicy(
      padding = 0, browser.fill = TRUE, knitr.defaultWidth = 900, knitr.defaultHeight = 550,
      knitr.figure = FALSE,
      viewer.defaultWidth = "100%", viewer.defaultHeight = "100%", viewer.padding = 0,
      browser.defaultWidth = "100%", browser.defaultHeight = "100%", browser.padding = 0),
    www_dir = www_dir
  )
}

#' Shiny bindings for Trelliscope
#'
#' Output and render functions for using TrelliscopeJS within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a TrelliscopeJS
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name Trelliscope-shiny
#'
#' @export
trelliscopeOutput <- function(outputId, width = "100%", height = "400px"){
  htmlwidgets::shinyWidgetOutput(outputId, "TrelliscopeJS", width, height,
    package = "trelliscopecore")
}

#' @rdname Trelliscope-shiny
#' @export
renderTrelliscope <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, TrelliscopeJSOutput, env, quoted = TRUE)
}
