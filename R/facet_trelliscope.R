utils::globalVariables(".")

#' Facet Trelliscope
#'
#' @param ... all parameters passed onto \code{ggplot2::\link[ggplot2]{facet_wrap}}
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
#' @export
#' @example man-roxygen/ex-trelliscope.R
#' @importFrom ggplot2 facet_wrap
facet_trelliscope <- function(..., nrow = 1, ncol = 1, name = NULL, group = "common",
  desc = "", md_desc = "", path = NULL, height = 500, width = 500,
  state = NULL, jsonp = TRUE, self_contained = FALSE, thumb = TRUE) {
  ret <- list(
    name = name,
    group = group,
    desc = desc,
    md_desc = md_desc,
    height = height,
    width = width,
    state = state,
    jsonp = jsonp,
    path = path,
    self_contained = self_contained,
    nrow = nrow,
    ncol = ncol,
    thumb = thumb,
    facet_wrap = ggplot2::facet_wrap(...)
  )

  class(ret) <- "facet_trelliscope"
  ret
}

#' Add method for gg / facet_trelliscope
#' @param e1 a object with class gg
#' @param e2 if object is of class 'facet_trelliscope', then 'facet_trelliscope' will be appended to the class of e1
#' @export
#' @importFrom ggplot2 %+%
`+.gg` <- function (e1, e2) {
  if (inherits(e2, "facet_trelliscope")) {

    e1 <- e1 %+% (e2$facet_wrap)
    attr(e1, "trelliscope") <- e2[c("name", "group", "desc", "md_desc", "height",
      "width", "state", "jsonp", "self_contained", "path", "state", "nrow", "ncol",
      "thumb")]
    class(e1) <- c("facet_trelliscope", class(e1))
    return(e1)
    # return(print(e1))
  }

  e1 %+% e2
}


#' Print facet trelliscope object
#'
#' @param x plot object
#' @param ... ignored
#' @import dplyr
#' @importFrom stats as.formula
#' @export
print.facet_trelliscope <- function(x, ...) {

  attrs <- attr(x, "trelliscope")

  # copy for better name
  p <- x
  # remove special class
  class(p) <- setdiff(class(p), "facet_trelliscope")

  data <- as_data_frame(p$data)
  facet_params <- p$facet$params

  # character vect of facet columns
  facet_cols <- unlist(lapply(facet_params$facets, as.character))

  # group by all the facets
  for (facet_col in facet_cols)
    data <- group_by_(data, facet_col, add = TRUE)

  # add functions to summarise with
  functions_to_run <- list(count = stats::as.formula(paste0(
    "~ cog(n(), desc = \"number of observations\")"
  )))

  # if any columns are unique per group, add a summary for them
  tmp <- summarise_all(data, n_distinct) %>% ungroup() %>% select(-one_of(facet_cols))
  unique_cols <- names(tmp)[sapply(tmp, function(x) all(x == 1))]

  get_label <- function(data, col) {
    lbl <- attr(data[[col]], "label")
    if (is.null(lbl))
      lbl <- col
    lbl
  }

  for (col in unique_cols)
    functions_to_run[[col]] <- stats::as.formula(paste0(
      "~ cog(", col, "[1],", "desc = \"", get_label(data, col), "\"", ")"))

  num_cols <- names(data)[sapply(data, is.numeric)]
  num_cols <- setdiff(num_cols, c(unique_cols, facet_cols))

  # if numeric, get the mean
  for (col in num_cols) {
    lbl <- get_label(data, col)
    cog_name <- paste(col, "_mean", sep = "")
    functions_to_run[[cog_name]] <- stats::as.formula(paste0(
      "~ cog(", "mean(", col, "),", "desc = \"mean ", lbl, "\"", ")"))
  }

  # sumarise the data
  cog_df <- summarise_(data, .dots = functions_to_run)
  cog_df$panelKey <- apply(cog_df[facet_cols], 1, paste, collapse = "_") %>% # nolint
    sanitize()

  cog_df <- as_cognostics(cog_df, cond_cols = facet_cols, key_col = "panelKey")

  # wrapper function that swaps out the data with a subset and removes the facet
  make_plot_obj <- function(dt) {
    q <- p
    q$data <- dt
    q$facet <- ggplot2::FacetNull
    q
  }
  plot_df <- do(data, plot = make_plot_obj(.))

  # make sure the panelKeys match
  merged_df <- suppressMessages(left_join(cog_df, plot_df))

  # remove the plots and name them according to the panelKey
  panels <- merged_df$plot
  names(panels) <- merged_df$panelKey # nolint

  name <- attrs$name
  if (is.null(name))
    name <- paste("by_", paste(facet_cols, collapse = "_"), sep = "")

  params <- resolve_app_params(attrs$path, attrs$self_contained, attrs$jsonp,
    name, attrs$group, attrs$state, attrs$nrow, attrs$ncol, attrs$thumb)

  pb <- progress::progress_bar$new(
    format = ":what [:bar] :percent :current/:total eta::eta",
    total = 5 + length(panels), width = getOption("width") - 5)
  pb$tick(0, tokens = list(what = "calculating         "))

  write_panels(
    panels,
    base_path = params$path,
    name = params$name,
    group = params$group,
    width = attrs$width,
    height = attrs$height,
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
    desc = attrs$desc,
    height = attrs$height,
    width = attrs$width,
    md_desc = attrs$md_desc,
    state = params$state,
    jsonp = params$jsonp,
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
    config_info = "appfiles/config.jsonp",
    spa = params$spa
  )

  print(res)
}
