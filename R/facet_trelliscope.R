
#' Facet Trelliscope
#'
#' @param ... all parameters passed onto \code{ggplot2::\link[ggplot2]{facet_wrap}}
#' @param name name of the display
#' @param group group that the display belongs to
#' @param desc description of the display
#' @param md_desc optional string of markdown that will be shown in the viewer for additional context about the display
#' @param height height in pixels of each panel
#' @param width width in pixels of each panel
#' @param state the initial state the display will open in
#' @param jsonp should json for display object be jsonp (TRUE) or json (FALSE)?
#' @param path the base directory of the trelliscope application
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- qplot(cty, hwy, data = mpg) +
#'   facet_trelliscope(
#'     ~ class + manufacturer,
#'     path = "_test",
#'     name = "testytest",
#'     width = 800
#'   )
#' p
#' }
#' @importFrom ggplot2 facet_wrap
facet_trelliscope <- function(..., name = NULL, group = "common",
  desc = "", md_desc = "", height = 500, width = 500,
  state = NULL, jsonp = TRUE, path = tempdir()) {
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
    facet_wrap = ggplot2::facet_wrap(...)
  )

  class(ret) <- "facet_trelliscope"
  ret
}

#' Add method to transfer the
#' @param e1 a object with class gg
#' @param e2 if object is of class 'facet_trelliscope', then 'facet_trelliscope' will be appended to the class of e1
#' @export
`+.gg` <- function (e1, e2) {
  if (inherits(e2, "facet_trelliscope")) {

    e1 <- e1 %+% (e2$facet_wrap)
    attr(e1, "trelliscope_name") <- e2$name
    attr(e1, "trelliscope_group") <- e2$group
    attr(e1, "trelliscope_desc") <- e2$desc
    attr(e1, "trelliscope_md_desc") <- e2$md_desc
    attr(e1, "trelliscope_height") <- e2$height
    attr(e1, "trelliscope_width") <- e2$width
    attr(e1, "trelliscope_state") <- e2$state
    attr(e1, "trelliscope_jsonp") <- e2$jsonp
    attr(e1, "trelliscope_path") <- e2$path
    class(e1) <- c("facet_trelliscope", class(e1))
    return(e1)
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

  # copy for better name
  p <- x
  # remove special class
  class(p) <- setdiff(class(p), "facet_trelliscope")

  data <- as_data_frame(p$data)
  facet_params <- p$facet$params

  # character vect of facet columns
  facet_cols <- unlist(
    lapply(facet_params$facets, as.character)
  )

  # group by all the facets
  for (facet_col in facet_cols) {
    data <- group_by_(data, facet_col, add = TRUE)
  }

  # add functions to summarise with
  functions_to_run <- list(
    count = ~ n()
  )

  # if numeric, get the mean
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      functions_to_run[[
        paste(col, "_mean", sep = "")
      ]] <- stats::as.formula(paste(
        "~ cog(",
          "mean(", col, "),",
          "desc = \"mean ", col, "\"",
        ")",
        sep = ""
      ))
    }
  }

  # sumarise the data
  cog_df <- summarise_(data, .dots = functions_to_run)
  cog_df$panelKey <- apply(cog_df[facet_cols], 1, paste, collapse = "_") %>%
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
  merged_df <- suppressMessages(
    left_join(cog_df, plot_df)
  )

  # remove the plots and name them according to the panelKey
  panels <- merged_df$plot
  names(panels) <- merged_df$panelKey

  # get the base path
  base_path <- attr(p, "trelliscope_path")
  if (is.null(base_path)) {
    base_path <- tempdir()
  }

  plot_name <- attr(p, "trelliscope_name")
  if (is.null(plot_name)) {
    # try to make a plot name
    plot_name <- paste(
      "~ ", paste(
        facet_cols,
        collapse = " + "
      ),
      sep = ""
    )
  }

  write_panels(
    panels,
    base_path = base_path,
    name = plot_name,
    group = attr(p, "trelliscope_group"),
    width = attr(p, "trelliscope_width"),
    height = attr(p, "trelliscope_height"),
    jsonp = attr(p, "trelliscope_jsonp")
  )

  write_display_obj(
    cog_df,
    panel_example = panels[[1]],
    base_path = base_path,
    name = plot_name,
    group = attr(p, "trelliscope_group"),
    desc = attr(p, "trelliscope_desc"),
    height = attr(p, "trelliscope_height"),
    width = attr(p, "trelliscope_width"),
    md_desc = attr(p, "trelliscope_md_desc"),
    state = attr(p, "trelliscope_state"),
    jsonp = attr(p, "trelliscope_jsonp")
  )

  prepare_display(
    base_path,
    # only copy if there is no bundle.js file
    jsonp = attr(p, "trelliscope_jsonp"),
    copy_viewer_files = ! file.exists(file.path(base_path, "bundle.js"))
  )
  view_display(base_path)
}
