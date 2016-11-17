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
#' @importFrom tidyr nest nest_ unnest
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
  data <- data %>%
    group_by_(.dots = lapply(facet_cols, as.symbol)) %>%
    auto_cogs()

  cog_desc <- attr(data$auto_cogs, "cog_desc")

  cog_df <- data %>% select(-one_of("data")) %>% tidyr::unnest()
  cog_df <- as_cognostics(cog_df, cond_cols = facet_cols, cog_desc = cog_desc)

  # wrapper function that swaps out the data with a subset and removes the facet
  make_plot_obj <- function(dt) {
    q <- p
    q$data <- dt
    q$facet <- ggplot2::FacetNull
    q
  }
  panels <- (data %>%
    purrr::by_row(~ make_plot_obj(unnest(.x[c(facet_cols, "data")])),
      .labels = FALSE))[[1]]
  names(panels) <- cog_df$panelKey # nolint

  name <- attrs$name
  if (is.null(name))
    name <- paste("by_", paste(facet_cols, collapse = "_"), sep = "")

  params <- resolve_app_params(attrs$path, attrs$self_contained, attrs$jsonp,
    name, attrs$group, attrs$state, attrs$nrow, attrs$ncol, attrs$thumb)

  pb <- progress::progress_bar$new(
    format = ":what [:bar] :percent :current/:total eta::eta",
    total = 5 + length(panels), width = getOption("width") - 8)
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
    config_info = params$config_path,
    spa = params$spa
  )

  if (params$in_knitr) {
    return(res)
  }

  print(res)
}
