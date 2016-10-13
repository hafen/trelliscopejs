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
#'
#' dd <- iris %>%
#'   group_by(Species) %>%
#'   summarise(
#'     msl = cog(mean(Sepal.Length), desc = "mean sepal length"),
#'     panel = panel(figure() %>%
#'       ly_points(Sepal.Length, Sepal.Width))) %>%
#'   trelliscope(name = "test", cond_cols = "Species")
#'
#' dd
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

  if (is.null(path))
    path <- tempfile("trelliscope")

  classes <- unlist(lapply(x, function(a) class(a)[1]))

  # nn <- nrow(x)
  # cond_vars <- which(unlist(lapply(x,
  #   function(a) length(unique(a)) == nn && (is.character(a) || is.factor(a)))))

  # if (length(cond_vars) == 0)
  #   stop("Could not find any unique conditioning variables.", call. = FALSE)

  if (length(cond_cols) == 0)
    stop("Could not find any unique conditioning variables.", call. = FALSE)

  keys <- apply(x[cond_cols], 1, function(a) paste(a, collapse = "_"))
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
    name = name
  )

  prepare_display(
    path,
    # only copy if there is no bundle.js file
    copy_viewer_files = ! file.exists(file.path(path, "bundle.js"))
  )

  structure(list(path = path, name = name, group = group),
    class = "trelliscope_display")
}

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
