#' @importFrom lattice xyplot panel.text
#' @importFrom grDevices png
make_png <- function(p, file, width, height, orig_width = width, res = 72,
  base_point_size = 12, pixelratio = 2) {

  if (capabilities("aqua")) {
    pngfun <- grDevices::png
  } else {
    pkg <- "Cairo"
    if (suppressWarnings(suppressMessages(require(pkg, character.only = TRUE)))) {
      pngfun <- Cairo::CairoPNG
    } else {
      pngfun <- grDevices::png
    }
  }

  fac <- max(min(width / orig_width, 1), 0.65) * 1.5
  pointsize <- base_point_size

  pngfun(filename = file,
    res = res * pixelratio * fac,
    width = width * pixelratio,
    height = height * pixelratio,
    pointsize = base_point_size * fac)

  dv <- grDevices::dev.cur()
  tryCatch({
    if (inherits(p, "trellis")) {
      # p$par.settings$fontsize <- list(text = pointsize, points = pointsize * 2 / 3)
      print(p)
    } else if (inherits(p, "ggplot")) {
      print(p)
    }
  },
  finally = grDevices::dev.off(dv))

  # if panel function didn't plot anything then make a blank panel
  if (!file.exists(file)) {
    pngfun(filename = file,
      width = width * pixelratio,
      height = height * pixelratio,
      # res = res * pixelratio,
      pointsize = pointsize)
    print(lattice::xyplot(NA ~ NA, xlab = "", ylab = "", scales = list(draw = FALSE),
      panel = function(x, y, ...) lattice::panel.text(0.5, 0.5, "no panel")))
    grDevices::dev.off()
  }
}
