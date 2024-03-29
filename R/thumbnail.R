write_thumb <- function(panel_example, path, width, height, thumb = TRUE) {
  if (is.character(panel_example) && file.exists(panel_example)) {
    ext <- tools::file_ext(panel_example)
    if (ext == "png") {
      path <- paste0(tools::file_path_sans_ext(path), ".", ext)
      file.copy(thumb, path)
    }
  } else {
    if (thumb) {
      if (inherits(panel_example, "htmlwidget")) {
        widget_thumbnail(panel_example, path, width, height)
      } else {
        suppressMessages(
          make_png(panel_example, file = path,
            width = width, height = height))
      }
    }

    # need "!thumb" in case overwriting existing
    if (!file.exists(path) || !thumb) {
      suppressMessages(
        make_png(blank_image(), file = path,
          width = width, height = height))
    }
  }
}

#' @importFrom graphics plot
#' @importFrom webshot webshot
widget_thumbnail <- function(p, thumb_path, width, height, delay = 0.5) {
  thumb_path <- path.expand(thumb_path)

  success <- FALSE
  res <- try({
    ff <- tempfile(fileext = ".html")
    ffjs <- tempfile(fileext = ".js")

    # don't want any padding
    p$sizingPolicy$padding <- 0 # nolint
    suppressMessages(htmlwidgets::saveWidget(p, ff, selfcontained = FALSE))

    webshot::webshot(paste0("file://", ff), thumb_path, vwidth = width, vheight = height,
      delay = delay)
  }, silent = TRUE)
  if (!inherits(res, "try-error")) {
    success <- TRUE
  }
  if (!file.exists(thumb_path))
    success <- FALSE

  if (!success)
    message("* could not create htmlwidget thumbnail... will use blank thumbnail...")
}

#' @import ggplot2
blank_image <- function(txt = "no thumbnail") {
  ggplot(data = data.frame(x = 0.5, y = 0.75, label = txt)) +
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
