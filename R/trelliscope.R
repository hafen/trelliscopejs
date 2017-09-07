#' Create a Trelliscope Display
#'
#' @param x an object to create at trelliscope display for
#' @param name name of the display
#' @param group group that the display belongs to
#' @param panel_col optional string specifying the column to use for panels (if there are multiple plot columns in \code{x})
#' @param desc optional text description of the display
#' @param md_desc optional string of markdown that will be shown in the viewer for additional context about the display
#' @param path the base directory of the trelliscope application
#' @param height height in pixels of each panel
#' @param width width in pixels of each panel
#' @param auto_cog should auto cogs be computed (if possible)?
#' @param state the initial state the display will open in
#' @param nrow the number of rows of panels to display by default
#' @param ncol the number of columns of panels to display by default
#' @param jsonp should json for display object be jsonp (TRUE) or json (FALSE)?
#' @param self_contained should the Trelliscope display be a self-contained html document? (see note)
#' @param thumb should a thumbnail be created?
#' @note Note that \code{self_contained} is severely limiting and should only be used in cases where you would either like your display to show up in the RStudio viewer pane, in an interactive R Markdown Notebook, or in a self-contained R Markdown html document.
#' @example man-roxygen/ex-trelliscope.R
#' @export
trelliscope <- function(x, name, group = "common", panel_col = NULL, desc = "",
  md_desc = "", path, height = 500, width = 500, auto_cog = FALSE, state = NULL,
  nrow = 1, ncol = 1, jsonp = TRUE, self_contained = FALSE, thumb = FALSE)
  UseMethod("trelliscope")

#' @export
trelliscope.data.frame <- function(x, name, group = "common", panel_col = NULL,
  desc = "", md_desc = "", path = NULL, height = 500, width = 500, auto_cog = FALSE,
  state = NULL, nrow = 1, ncol = 1, jsonp = TRUE, self_contained = FALSE, thumb = FALSE) {

  panel_img_col <- names(which(unlist(lapply(x, function(a) {
    tp <- attr(a, "cog_attrs")$type
    if (is.null(tp))
      return(FALSE)
    if (tp == "panelSrc")
      return(TRUE)
  }))))
  if (length(panel_img_col) == 0)
    panel_img_col <- NULL

  # if the user specified panel_col, ignore panel_img_col unless they're the same
  if (!is.null(panel_col) && !is.null(panel_img_col)) {
    if (panel_col == panel_img_col) {
      panel_col <- NULL
    } else {
      panel_img_col <- NULL
    }
  }

  if (is.null(panel_col) && is.null(panel_img_col)) {
    classes <- unlist(lapply(x, function(a) class(a)[1]))
    panel_col <- names(which(classes == "trelliscope_panels"))

    if (length(panel_col) > 1) {
      panel_col <- panel_col[1]
      message("Multiple columns containing panels were found. Using ", panel_col, ".",
        "To explicitly specify which column to use, use the argument 'panel_col'.")
    }
  }

  if (length(panel_col) != 1 && length(panel_img_col) != 1)
    stop_nice("A column containing the panel to be plotted must be specified",
      "using panel() or img_panel().")

  cog_info <- cog_df_info(
    x,
    panel_col = panel_col,
    state = state,
    auto_cog = auto_cog
  )
  cog_df <- cog_info$cog_df
  cond_cols <- cog_info$cond_cols
  state <- cog_info$state

  params <- resolve_app_params(path, self_contained, jsonp, name, group,
    state, nrow, ncol, thumb)

  keys <- apply(x[cond_cols], 1, function(a) paste(a, collapse = "_")) %>%
    sanitize()
  x$panelKey <- keys # nolint

  if (length(panel_img_col) == 0) {
    panels <- x[[panel_col]]
    names(panels) <- keys
  } else {
    # don't need to write panels because they are supplied with img_panel
    panels <- list(structure(list(), class = "img_panel"))
  }

  # need to start progress bar before writing panels
  pb <- progress::progress_bar$new(
    format = ":what [:bar] :percent :current/:total eta::eta",
    total = 5 + length(panels), width = getOption("width") - 8)
  pb$tick(0, tokens = list(what = "calculating         "))

  if (length(panel_img_col) == 0) {
    write_panels(
      panels,
      base_path = params$path,
      name = params$name,
      width = width,
      height = height,
      group = params$group,
      jsonp = params$jsonp,
      pb = pb
    )
  } else {
    pb$tick(tokens = list(what = "writing panels      "))
  }

  write_display_obj(
    cog_df,
    panel_example = panels[[1]],
    panel_img_col = panel_img_col,
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

  res <- trelliscope_widget(
    id = params$id,
    www_dir = params$www_dir,
    latest_display = list(name = params$name, group = params$group),
    self_contained = params$self_contained,
    dependencies = get_dependencies(panels[[1]]),
    config_info = params$config_path,
    spa = params$spa,
    sc_deps = get_dependencies(panels[[1]])
  )

  if (params$in_knitr) {
    return(res)
  }

  print(res)
}

# hacky way to get cond_cols:
#   (ideally, we'd use groups(), but dplyr peels one group off after summarise)
#   we know grouped variables show up first
#   so iterate through until their combination is unique
find_cond_cols <- function(x, is_nested) {

  # if (is_nested)
  #   return(names(x))

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
