write_thumb <- function(panel_example, path, width, height) {
  if (inherits(panel_example, "htmlwidget")) {
    widget_thumbnail(panel_example, path)
  } else {
    suppressMessages(
      make_png(panel_example, file = path,
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
      p$sizingPolicy$padding <- 0
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
    message("* could not create htmlwidget thumbnail... creating an empty thumbnail...")
    grDevices::png(filename = thumb_path)
    graphics::plot(1, 1, type = "n", xlab = "", ylab = "", axes = FALSE)
    grDevices::dev.off()
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
