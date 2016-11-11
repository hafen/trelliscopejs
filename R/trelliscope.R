#' Create a Trelliscope Display
#'
#' @param x an object to create at trelliscope display for
#' @param name name of the display
#' @param group group that the display belongs to
#' @param desc description of the display
#' @param md_desc optional string of markdown that will be shown in the viewer for additional context about the display
#' @param path the base directory of the trelliscope application
#' @param height height in pixels of each panel
#' @param width width in pixels of each panel
#' @param state the initial state the display will open in
#' @param nrow the number of rows of panels to display by default
#' @param ncol the number of columns of panels to display by default
#' @param jsonp should json for display object be jsonp (TRUE) or json (FALSE)?
#' @param self_contained should the Trelliscope display be a self-contained html document? (see note)
#' @param thumb should a thumbnail be created?
#' @note Note that \code{self_contained} is severely limiting and should only be used in cases where you would either like your display to show up in the RStudio viewer pane, in an interactive R Markdown Notebook, or in a self-contained R Markdown html document.
#' @example man-roxygen/ex-trelliscope.R
#' @export
trelliscope <- function(x, name, group = "common", desc = "",
  md_desc = "", path, height = 500, width = 500, state = NULL,
  nrow = 1, ncol = 1, jsonp = TRUE, self_contained = FALSE, thumb = TRUE)
  UseMethod("trelliscope")

#' @export
trelliscope.data.frame <- function(x, name, group = "common", desc = "",
  md_desc = "", path = NULL, height = 500, width = 500, state = NULL,
  nrow = 1, ncol = 1, jsonp = TRUE, self_contained = FALSE, thumb = TRUE) {

  classes <- unlist(lapply(x, function(a) class(a)[1]))

  cond_cols <- find_cond_cols(x)
  sort_cols <- find_sort_cols(x)

  # if we are no longer sorted by a cond_col but are sorted by something else
  # and if sort state is not already specified, then set that as state
  if (nrow(sort_cols) > 0) {
    cond_not_sorted <- !sort_cols$name %in% cond_cols
    other_sorted <- setdiff(sort_cols$name, cond_cols)
    if (is.null(state$sort) && cond_not_sorted && length(other_sorted) > 0) {
      if (is.null(state))
        state <- list()
      state$sort <- lapply(other_sorted, function(a) {
        list(name = a, dir = sort_cols$dir[sort_cols$name == a])
      })
      if (is.null(state$labels)) {
        state$labels <- c(cond_cols, other_sorted)
      }
    }
  }

  params <- resolve_app_params(path, self_contained, jsonp, name, group,
    state, nrow, ncol, thumb)

  keys <- apply(x[cond_cols], 1, function(a) paste(a, collapse = "_")) %>%
    sanitize()
  x$panelKey <- keys # nolint

  panel_col <- which(classes == "trelliscope_panels")
  if (length(panel_col) != 1)
    stop("A column containing the panel to be plotted must be specified using panel().",
      call. = FALSE)

  panels <- x[[panel_col]]
  names(panels) <- keys

  pb <- progress::progress_bar$new(
    format = ":what [:bar] :percent :current/:total eta::eta",
    total = 5 + length(panels), width = getOption("width") - 5)
  pb$tick(0, tokens = list(what = "calculating         "))

  write_panels(
    panels,
    base_path = params$path,
    name = params$name,
    group = params$group,
    jsonp = params$jsonp,
    pb = pb
  )

  write_display_obj(
    as_cognostics(x[, -panel_col], cond_cols = cond_cols, key_col = "panelKey"),
    panel_example = panels[[1]],
    base_path = params$path,
    id = params$id,
    name = params$name,
    group = params$group,
    desc = desc,
    height = height,
    width = width,
    md_desc = md_desc,
    state = params$state,
    jsonp = params$jsonp,
    self_contained = params$self_contained,
    thumb = params$thumb,
    pb = pb
  )

  prepare_display(params$path, params$id, params$self_contained, params$jsonp, pb = pb)

  trelliscope_widget(
    id = params$id,
    www_dir = params$www_dir,
    latest_display = list(name = params$name, group = params$group),
    self_contained = params$self_contained,
    dependencies = get_dependencies(panels[[1]]),
    config_info = "appfiles/config.jsonp",
    spa = params$spa
  )
}

# hacky way to get cond_cols:
#   (ideally, we'd use groups(), but dplyr peels one group off after summarise)
#   we know grouped variables show up first
#   so iterate through until their combination is unique
find_cond_cols <- function(x) {
  nn <- nrow(x)
  cond_cols <- NULL
  usable <- names(x)[sapply(x, is.atomic)]

  if (length(unique(x[[usable[1]]])) == nn) {
    cond_cols <- usable[1]
  } else {
    for (i in seq_len(ncol(x))[-1]) {
      if (length(unique(do.call(paste, c(x[usable[1:i]], sep = "_")))) == nn) {
        cond_cols <- usable[1:i]
        break
      }
    }
  }

  if (is.null(cond_cols)) {
    stop("Could not find unique group variables...")
  }

  cond_cols
}

# hacky way to see if the tbl has been sorted after summarise:
#   if none of the facet variables are still sorted but another variable is
#   then the user must have sorted on that variable
#   note: this will only detect first-order sorting...
find_sort_cols <- function(x) {
  sortable <- names(x)[sapply(x, is.atomic)]

  res <- lapply(sortable,
    function(nm) {
      res <- data_frame(name = nm, dir = NA)
      if (!is.unsorted(x[[nm]], na.rm = TRUE)) {
        res$dir <- "asc"
      } else if (!is.unsorted(rev(x[[nm]]), na.rm = TRUE)) {
        res$dir <- "desc"
      }
      res
    }
  ) %>% bind_rows()

  res %>% filter_(~ !is.na(dir))
}
