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

  panel_col <- names(which(classes == "trelliscope_panels"))
  if (length(panel_col) != 1)
    stop_nice("A column containing the panel to be plotted must be specified using panel().")

  atomic_cols <- names(x)[sapply(x, is.atomic)]
  non_atomic_cols <- setdiff(names(x), c(atomic_cols, panel_col))
  is_nested <- length(non_atomic_cols) > 0

  if (length(atomic_cols) == 0)
    stop_nice("There must be at least one atomic column in the data frame passed in",
      "to trelliscope.data.frame")

  cond_cols <- find_cond_cols(x[atomic_cols], is_nested)

  # if we are no longer sorted by a cond_col but are sorted by something else
  # and if sort state is not already specified, then set that as state
  if (is.unsorted(x[cond_cols[1]])) {
    sort_cols <- find_sort_cols(x[setdiff(atomic_cols, cond_cols)])

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
  }

  cogs <- list(as_cognostics(x[atomic_cols], cond_cols))
  if (length(non_atomic_cols) > 0) {
    usable <- non_atomic_cols[sapply(x[non_atomic_cols], function(a) is.data.frame(a[[1]]))]
    needs_auto <- usable[sapply(x[usable], function(a) {
      any(sapply(a, nrow) > 1)
    })]
    for (a in needs_auto) {
      cogs[[length(cogs) + 1]] <- x[c(cond_cols, a)] %>%
        auto_cogs() %>%
        select(-one_of(a)) %>%
        unnest() %>%
        as_cognostics(cond_cols) %>%
        select(-one_of(c(cond_cols, "panelKey")))
    }
    no_needs_auto <- setdiff(usable, needs_auto)
    for (a in no_needs_auto) {
      cogs[[length(cogs) + 1]] <- x[c(cond_cols, a)] %>%
        unnest() %>%
        as_cognostics(cond_cols) %>%
        select(-one_of(c(cond_cols, "panelKey")))
    }
  }

  cog_df <- bind_cols(cogs)

  params <- resolve_app_params(path, self_contained, jsonp, name, group,
    state, nrow, ncol, thumb)

  keys <- apply(x[cond_cols], 1, function(a) paste(a, collapse = "_")) %>%
    sanitize()
  x$panelKey <- keys # nolint

  panels <- x[[panel_col]]
  names(panels) <- keys

  pb <- progress::progress_bar$new(
    format = ":what [:bar] :percent :current/:total eta::eta",
    total = 5 + length(panels), width = getOption("width") - 8)
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
    cog_df,
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
find_cond_cols <- function(x, is_nested) {
  if (is_nested)
    return(names(x))

  nn <- nrow(x)
  nms <- names(x)
  cond_cols <- NULL

  if (length(unique(x[[1]])) == nn) {
    cond_cols <- nms[1]
  } else {
    for (i in seq_len(ncol(x))[-1]) {
      if (length(unique(do.call(paste, c(x[1:i], sep = "_")))) == nn) {
        cond_cols <- nms[1:i]
        break
      }
    }
  }

  # for (i in seq_len(ncol(x))) {
  #   n_unique <- length(unique(do.call(paste, c(x[1:i], sep = "_"))))
  #   if (n_unique == nn && i == ncol(x)) {
  #     cond_cols <- nms[seq_len(i)]
  #     break
  #   } else if (n_unique > nn && i > 1) {
  #     cond_cols <- nms[seq_len(i - 1)]
  #     break
  #   }
  # }

  if (is.null(cond_cols)) {
    stop_nice("Could not find unique group variables...")
  }

  cond_cols
}

# hacky way to see if the tbl has been sorted after summarise:
#   if none of the facet variables are still sorted but another variable is
#   then the user must have sorted on that variable
#   note: this will only detect first-order sorting...
find_sort_cols <- function(x) {
  if (ncol(x) == 0)
    return(data_frame())

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
