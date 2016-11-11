write_thumb <- function(panel_example, path, width, height, thumb = TRUE) {
  if (thumb) {
    if (inherits(panel_example, "htmlwidget")) {
      widget_thumbnail(panel_example, path)
    } else {
      suppressMessages(
        make_png(panel_example, file = path,
          width = width, height = height))
    }
  }

  # need "!thumb" in case overwriting existing
  if (!file.exists(path) || !thumb) {
    suppressMessages(
      make_png(blank_thumb(), file = path,
        width = width, height = height))
  }
}

#' @importFrom graphics plot
widget_thumbnail <- function(p, thumb_path, timeout = 750) {
  phantom <- find_phantom()
  thumb_path <- path.expand(thumb_path)

  success <- FALSE
  if (phantom == "") {
    message("** phantomjs dependency could not be found - ",
      "thumbnail cannot be generated - visit http://phantomjs.org/download.html ",
      "to install")
  } else {
    res <- try({
      ff <- tempfile(fileext = ".html")
      ffjs <- tempfile(fileext = ".js")

      # don't want any padding
      p$sizingPolicy$padding <- 0 # nolint
      suppressMessages(htmlwidgets::saveWidget(p, ff, selfcontained = FALSE))

      js <- paste0("var page = require('webpage').create();
page.open('file://", ff, "', function() {
  window.setTimeout(function () {
    page.render('", thumb_path, "');
    phantom.exit();
  }, ", timeout, ");
});")
      cat(js, file = ffjs)
      system2(phantom, ffjs)
    })
    if (!inherits(res, "try-error")) {
      success <- TRUE
    }
    if (!file.exists(thumb_path))
      success <- FALSE
  }

  if (!success) {
    message("* could not create htmlwidget thumbnail... will use blank thumbnail...")
  }
}

# similar to webshot
find_phantom <- function() {
  phantom <- Sys.which("phantomjs")
  if (Sys.which("phantomjs") == "")
    if (identical(.Platform$OS.type, "windows"))
      phantom <- Sys.which(file.path(Sys.getenv("APPDATA"), "npm", "phantomjs.cmd"))
  phantom
}

#' @import ggplot2
blank_thumb <- function() {
  ggplot(data = data.frame(x = 0.5, y = 0.75, label = "no thumbnail")) +
    geom_text(aes(x = x, y = y, label = label), size = 8) +
    labs(x = NULL, y = NULL, title = NULL) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "null"),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.line = element_blank(),
      legend.position = "none",
      axis.ticks.length = unit(0, "null")
    )
}
