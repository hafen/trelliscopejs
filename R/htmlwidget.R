trelliscope_widget <- function(id, config_info, www_dir, latest_display,
  dependencies = NULL, self_contained, spa = TRUE, width = NULL, height = NULL) {

  # TODO: warn if width or height are too small

  # forward options using x
  x <- list(
    id = id,
    config_info = config_info,
    self_contained = self_contained,
    latest_display = latest_display,
    spa = spa,
    in_knitr = getOption("knitr.in.progress", FALSE))

  if (spa) {
    width <- height <- "100%"
  }

  # create widget
  wdgt <- htmlwidgets::createWidget(
    name = "trelliscopejs_widget",
    x,
    width = width,
    height = height,
    package = "trelliscopejs",
    sizingPolicy = htmlwidgets::sizingPolicy(padding = 0, browser.fill = TRUE,
      knitr.defaultWidth = 900, knitr.defaultHeight = 550, knitr.figure = FALSE,
      viewer.defaultWidth = "100%", viewer.defaultHeight = "100%", viewer.padding = 0,
      viewer.fill = TRUE,
      browser.defaultWidth = "100%", browser.defaultHeight = "100%", browser.padding = 0),
    preRenderHook = prerender_selfcontained
  )

  # don't want these to get shipped in the html page
  # but we do want them to persist so we can edit the object...
  attr(wdgt, "trelliscope_pars") <- list(
    www_dir = www_dir
  )

  return(wdgt)
}

prerender_selfcontained <- function(x) {
  if (x$x$self_contained) {
    www_dir <- attr(x, "trelliscope_pars")$www_dir
    path <- file.path(www_dir, "appfiles")
    disp_path <- file.path(path, "displays", x$x$latest_display$group,
      x$x$latest_display$name)
    pfs <- list.files(file.path(disp_path, "json"), full.names = TRUE)
    panels <- lapply(pfs, function(f)
      readLines(f, warn = FALSE))
    names(panels) <- basename(gsub("\\.json$", "", pfs))

    x$x$config_info <- paste0("{ \"config\": ",
      paste(readLines(file.path(path, "config.json"), warn = FALSE),
        collapse = ""),
      ", \"displayList\": ",
      paste(readLines(file.path(path, "displays", "displayList.json"), warn = FALSE),
        collapse = ""),
      ", \"displayObj\": ",
      readLines(file.path(disp_path, "displayObj.json"), warn = FALSE),
      ", \"cogData\": ",
      paste(readLines(file.path(disp_path, "cogData.json"), warn = FALSE),
        collapse = ""),
      ", \"panels\": {",
      paste("\"", names(panels), "\": ", panels, sep = "", collapse = ", "),
      "}}")
  } else {
    x$x$config_info <- paste0("'", x$x$config_info, "'")
  }
  x
}

#' @importFrom knitr knit_print
#' @export
knit_print.facet_trelliscope <- function(x, ..., options = NULL) {
  x <- print.facet_trelliscope(x)
  knitr::knit_print(x, ..., options = options)
}

# nolint start

# Override print.htmlwidget for trelliscopejs_widget so we can control the output location
#' @export
print.trelliscopejs_widget <- function(x, ..., view = interactive()) {

  # hacky way to detect R Markdown Notebook
  print_fn <- try(get("print.htmlwidget"), silent = TRUE)
  if (!inherits(print_fn, "try-error")) {
    return(print_fn(x))
  }

  # if (x$x$self_contained) {
  #   # we want to use the
  #   class(x) <- gsub("trelliscopejs_widget", "trelliscopejs_widget2", class(x))
  #   return(print(x))
  # }

  # if we have a viewer then forward viewer pane height (if any)
  viewer <- getOption("viewer")
  if (!is.null(viewer)) {
    viewerFunc <- function(url) {

      # get the requested pane height (it defaults to NULL)
      paneHeight <- x$sizingPolicy$viewer$paneHeight

      # convert maximize to -1 for compatibility with older versions of rstudio
      # (newer versions convert 'maximize' to -1 interally, older versions
      # will simply ignore the height if it's less than zero)
      if (identical(paneHeight, "maximize"))
        paneHeight <- -1

      # call the viewer
      viewer(url, height = paneHeight)
    }
  } else {
    viewerFunc <- utils::browseURL
  }

  www_dir <- attr(x, "trelliscope_pars")$www_dir

  # TODO: check if www_dir is in tempdir() or not and handle / warn appropriately
  # based on self_contained, etc.

  # call html_print with the viewer
  el_tags <- htmltools::as.tags(x, standalone = FALSE)
  trelliscope_html_print(el_tags, www_dir = www_dir, viewer = if (view) viewerFunc)

  # return value
  invisible(x)
}

# nolint end

trelliscope_html_print <- function(html, www_dir = NULL, background = "white",
  viewer = getOption("viewer", utils::browseURL)) {
  if (is.null(www_dir))
    www_dir <- tempfile("viewhtml")
  if (!dir.exists(www_dir))
    dir.create(www_dir)
  www_dir <- normalizePath(www_dir)
  index_html <- file.path(www_dir, "index.html")
  save_html(html, file = index_html, background = background,
    libdir = "lib")
  if (!is.null(viewer))
    viewer(index_html)

  invisible(index_html)
}

# nolint start

#' Shiny bindings for Trelliscope
#'
#' Output and render functions for using trelliscopejs_widget within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a trelliscopejs_widget
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name Trelliscope-shiny
#'
#' @export
trelliscopeOutput <- function(outputId, width = "100%", height = "400px"){
  htmlwidgets::shinyWidgetOutput(outputId, "trelliscopejs_widget", width, height,
    package = "trelliscopejs")
}

#' @rdname Trelliscope-shiny
#' @export
renderTrelliscope <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, trelliscopeOutput, env, quoted = TRUE)
}

# nolint end
