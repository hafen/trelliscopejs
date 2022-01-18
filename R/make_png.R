get_png_units <- function(width, height, orig_width = width, res = 72,
  base_point_size = 12, pixelratio = 2) {

  width <- as.numeric(gsub("px", "", width))
  height <- as.numeric(gsub("px", "", height))
  orig_width <- as.numeric(gsub("px", "", orig_width))

  # need to convert unit to pixels
  # need to have 'fac' factor if coming from unit
  width_is_unit <- height_is_unit <- FALSE
  if (inherits(width, "unit")) {
    width <- unit_to_px(width, res)
    orig_width <- width
    width_is_unit <- TRUE
  }
  if (inherits(height, "unit")) {
    height <- unit_to_px(height, res)
    orig_height <- height
    height_is_unit <- TRUE
  }

  fac <- max(min(width / orig_width, 1), 0.65) * 1.5
  pointsize <- base_point_size

  list(
    res = res * pixelratio * fac,
    width = width * pixelratio * ifelse(width_is_unit, fac, 1),
    height = height * pixelratio * ifelse(height_is_unit, fac, 1),
    pointsize = base_point_size * fac
  )
}

#' @importFrom grDevices png
make_png <- function(p, file, width, height, orig_width = width, res = 72,
  base_point_size = 12, pixelratio = 2) {

  pngfun <- grDevices::png
  # if (capabilities("aqua")) {
  # } else {
  #   pkg <- "Cairo" # nolint
  #   if (suppressWarnings(suppressMessages(require(pkg, character.only = TRUE)))) {
  #     pngfun <- Cairo::CairoPNG
  #   } else {
  #     pngfun <- grDevices::png
  #   }
  # }

  units <- get_png_units(width, height, orig_width, res,
    base_point_size, pixelratio)

  pngfun(filename = file,
    res = units$res,
    width = units$width,
    height = units$height,
    pointsize = units$pointsize)

  unknown_object <- FALSE
  dv <- grDevices::dev.cur()
  tryCatch({
    if (inherits(p, "trellis")) {
      # p$par.settings$fontsize <- list(text = pointsize, points = pointsize * 2 / 3)
      print(p)
    } else if (inherits(p, "ggplot")) {
      print(p)
    } else if (inherits(p, "gtable")) {
      grid::grid.draw(p)
    } else {
      unknown_object <- TRUE
      try(print(p), silent = TRUE)
    }
  },
  finally = grDevices::dev.off(dv))

  # if panel function didn't plot anything then make a blank panel
  # res = res * pixelratio,
  if (!file.exists(file)) {
    if (unknown_object) {
      cls <- paste(class(p), collapse = ", ")
      message("The panel object of class'", cls,
        "' is not a standard plot object and did not produce a panel file.")
    }
    pngfun(filename = file, width = width * pixelratio, height = height * pixelratio,
      pointsize = units$pointsize)
    blank_image("no panel")
    grDevices::dev.off()
  }
}
