#' Create a Trelliscope Display
#'
#' @param x an object to create at trelliscope display for
#' @param name name of the display
#' @param group group that the display belongs to
#' @param desc description of the display
#' @param cond_cols the column name(s) that comprise the conditioning variables
#' @param md_desc optional string of markdown that will be shown in the viewer for additional context about the display
#' @param path the base directory of the trelliscope application
#' @param height height in pixels of each panel
#' @param width width in pixels of each panel
#' @param state the initial state the display will open in
#' @param jsonp should json for display object be jsonp (TRUE) or json (FALSE)?
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(rbokeh)
#' p <- ggplot2::mpg %>%
#'   group_by(class, manufacturer) %>%
#'   summarise(
#'     panel = panel(
#'       figure(xlab = "City mpg", ylab = "Highway mpg") %>%
#'         ly_points(cty, hwy,
#'           hover = data_frame(model = paste(year, trans, model),
#'           cty = cty, hwy = hwy)) %>%
#'         y_range(c(9, 47)) %>%
#'         x_range(c(7, 37)))) %>%
#'   trelliscope(name = "city_vs_highway_mpg", path = "_test",
#'     cond_cols = c("class", "manufacturer"))
#' p
#' }
#' @export
trelliscope <- function(x, name, group = "common", desc = "",
  cond_cols = NULL, md_desc = "", path, height = 500, width = 500,
  state = NULL, jsonp = TRUE)
  UseMethod("trelliscope")

#' @export
trelliscope.data.frame <- function(x, name, group = "common", desc = "",
  cond_cols = NULL, md_desc = "", path = NULL, height = 500, width = 500,
  state = NULL, jsonp = TRUE) {

  if (is.null(path)) {
    www_dir <- tempfile("trelliscope")
  } else {
    www_dir <- normalizePath(path, mustWork = FALSE)
  }
  path <- file.path(www_dir, "appfiles")

  classes <- unlist(lapply(x, function(a) class(a)[1]))

  # nn <- nrow(x)
  # cond_vars <- which(unlist(lapply(x,
  #   function(a) length(unique(a)) == nn && (is.character(a) || is.factor(a)))))

  # if (length(cond_vars) == 0)
  #   stop("Could not find any unique conditioning variables.", call. = FALSE)

  name <- sanitize(name)
  group <- sanitize(group)

  id <- get_id(path)

  if (length(cond_cols) == 0)
    stop("Must specify conditioning variables.", call. = FALSE)

  keys <- apply(x[cond_cols], 1, function(a) paste(a, collapse = "_")) %>%
    sanitize()
  x$panelKey <- keys

  panel_col <- which(classes == "trelliscope_panels")
  if (length(panel_col) != 1)
    stop("A column containing the panel to be plotted must be specified using panel().",
      call. = FALSE)

  panels <- x[[panel_col]]
  names(panels) <- keys

  write_panels(
    panels,
    base_path = path,
    name = name
  )

  write_display_obj(
    as_cognostics(x[, -panel_col], cond_cols = cond_cols, key_col = "panelKey"),
    panel_example = panels[[1]],
    base_path = path,
    id = id,
    name = name
  )

  prepare_display(path, id)

  trelliscope_widget(id = id, url = "appfiles/config.jsonp", www_dir = www_dir)

  # structure(list(path = path, name = name, group = group),
  #   class = "trelliscope_display")
}

#' @export
print.trelliscope_display <- function(x) {
  view_display(x$path)
}

#' Panel Wrapper Function
#'
#' @param x a plot object
#' @export
panel <- function(x) {
  structure(list(x), class = "trelliscope_panels")
}
