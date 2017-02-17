utils::globalVariables(c(".", "ggplotly"))


upgrade_scales_param <- function(scales, plot_facet) {
  if (length(scales) > 2) {
    stop("scales must not be longer than length 2")
  }
  if (is.na(scales) || is.null(scales) || length(scales) == 0) {
    stop("scales must be a character vector of size 1 or 2")
  }

  if (length(scales) == 1) {
    scales <- switch(scales,
      "same" = c("same", "same"),
      "free" = c("free", "free"),
      "free_x" = c("free", "same"),
      "free_y" = c("same", "free"),
      "sliced" = c("sliced", "sliced"),
      "sliced_x" = c("sliced", "same"),
      "sliced_y" = c("same", "sliced"),
      stop(
        "if scales is of length 1, it may only be one of the following values: ",
        "c('same', 'free', 'free_x', 'free_y', 'sliced', 'sliced_x', 'sliced_y')"
      )
    )
  }

  if (!all(scales %in% c("same", "free", "sliced"))) {
    stop("a length 2 scales parameter can only be made of 'same', 'free', or 'sliced' values")
  }


  # sliced is not allowed for faceted columns
  if (!inherits(plot_facet, "FacetNull")) {
    for (item_val in list(list(1, "x"), list(2, "y"))) {
      if (scales[item_val[[1]]] == "sliced") {
        message(
          "If a panel is being displayed with 'facet_wrap' or 'facet_grid', ",
          "the ", item_val[[2]], " scale can not be sliced.  Using 'free' instead."
        )
        scales[item_val[[1]]] <- "free"
      }
    }
  }


  list(x_info = list(name = "x", scale_type = scales[1]), y_info = list(name = "y", scale_type = scales[1]))
}


plot_gtable <- function(p) {
  ggplot_gtable(ggplot_build(p))
}

str_detect <- function(x, pattern) {
  grepl(pattern, x)
}


if (FALSE) {
  library(ggplot2)
  p <- qplot(displ, hwy, data = mpg) + facet_grid(year ~ drv)

  a <- extract_axis_left(p)
  b <- extract_axis_bottom(p)
  d <- extract_plot_content(p)

  pg = plot_gtable(p)
  a <- extract_axis_left(pg = pg)
  b <- extract_axis_bottom(pg = pg)
  d <- extract_plot_content(pg = pg)

  # width of left axis
  axis_left_width(a)

  # height of bottom axis
  axis_bottom_height(b)

  # width of plot content
  grid::convertWidth(sum(grid::convertWidth(a$widths, unitTo = "cm")), unitTo = "cm")
  # WRONG!
  gtabe::gtable_show_layout(d)


  # height of left must be supplied
  # width of bottom must be supplied
  # height,width of plot content must be supplied


  qplot(displ, hwy, data = mpg) +
    scale_y_continuous(limits = range(mpg$hwy)) +
    scale_x_continuous(limits = range(mpg$displ)) +
    facet_trelliscope(~ drv, ncol = 3, nrow = 1)


}


axis_left_width <- function(pg, unitTo = "cm") {
  grid::convertWidth(sum(grid::convertWidth(pg$widths, unitTo = unitTo)), unitTo = unitTo)
}
axis_bottom_height <- function(pg, unitTo = "cm") {
  grid::convertHeight(sum(grid::convertHeight(pg$heights, unitTo = unitTo)), unitTo = unitTo)
}



extract_axis_left <- function(p, pg = plot_gtable(p), include_strips = TRUE) {
  layout <- pg$layout
  layout_name <- layout$name

  # axis layout info
  al <- layout[str_detect(layout_name, "axis-l|ylab-l"), ]

  if (isTRUE(include_strips)) {
    alx <- layout[str_detect(layout_name, "axis-l|strip-t|ylab-l"), ]
  } else {
    alx <- al
  }

  # get only the axis left objects (and maybe strip top spacer)
  axis_panel <- pg[min(alx$b):max(alx$t), min(al$l):max(al$r)]

  # # force to align left
  # axis_panel <- gtable::gtable_add_cols(axis_panel, grid::unit(1, "null"), 0)

  axis_panel

}
extract_axis_bottom <- function(p, pg = plot_gtable(p), include_strips = TRUE) {
  layout <- pg$layout
  layout_name <- layout$name

  # axis layout info
  al <- layout[str_detect(layout_name, "axis-b|xlab-b"), ]

  if (isTRUE(include_strips)) {
    alx <- layout[str_detect(layout_name, "axis-b|strip-r|xlab-b"), ]
  } else {
    alx <- al
  }
  # get only the axis left objects (and maybe strip top spacer)
  axis_panel <- pg[min(al$b):max(al$t), min(alx$l):max(alx$r)]

  # # force to align top
  # axis_panel <- gtable::gtable_add_rows(
  #   axis_panel,
  #   grid::unit(1, "null"),
  #   max(axis_panel$layout$b) + 1 # make it the bottom row
  # )

  axis_panel
}


extract_plot_content <- function(p, pg = plot_gtable(p), include_strips = TRUE) {

  # ask about strips
  layout_names <- c("panel")
  strip_right_name <- "strip-r"
  strip_top_name <- "strip-t"

  all_layout_names <- c(layout_names, strip_right_name, strip_top_name)

  if (isTRUE(include_strips)) {
    layout_names <- all_layout_names
  }

  # get correct panel (and strips)
  layout_rows <- str_detect(pg$layout$name, paste(layout_names, collapse = "|"))

  layout_info <- pg$layout[layout_rows, ]
  top_bottom <- layout_info[, c("t", "b")]
  left_right <- layout_info[, c("l", "r")]

  plot_panel <- pg[
    min(top_bottom):max(top_bottom),
    min(left_right):max(left_right)
  ]

  plot_panel
}







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
#' @param scales should Scales be the same (\code{"same"}, the default), free (\code{"free"}), or sliced (\code{"sliced"}). May provide a single string or two strings, one for the X and Y axis respectively.
#' @param jsonp should json for display object be jsonp (TRUE) or json (FALSE)?
#' @param as_plotly should the panels be written as plotly objects?
#' @param plotly_args optinal named list of arguments to send to \code{ggplotly}
#' @param self_contained should the Trelliscope display be a self-contained html document? (see note)
#' @param thumb should a thumbnail be created?
#' @note Note that \code{self_contained} is severely limiting and should only be used in cases where you would either like your display to show up in the RStudio viewer pane, in an interactive R Markdown Notebook, or in a self-contained R Markdown html document.
#' @export
#' @example man-roxygen/ex-facet_trelliscope.R
#' @importFrom ggplot2 facet_wrap
facet_trelliscope <- function(
  facets,
  nrow = 1, ncol = 1, scales = "same", name = NULL, group = "common",
  desc = "", md_desc = "", path = NULL, height = 500, width = 500,
  state = NULL, jsonp = TRUE, as_plotly = FALSE, plotly_args = NULL,
  self_contained = FALSE, thumb = TRUE, auto_cog_data = TRUE) {

  if (as_plotly) {
    if (!requireNamespace("plotly", quietly = TRUE))
      stop("Package 'plotly' is needed for as_plotly = TRUE Please install it.",
        call. = FALSE)
  }

  ret <- list(
    facets = facets,
    facet_cols = facet_wrap(facets)$params$facets,
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
    scales = scales,
    thumb = thumb,
    as_plotly = as_plotly,
    plotly_args = plotly_args,
    auto_cog_data = auto_cog_data
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

    # e1 <- e1 %+% (e2$facet_wrap)
    attr(e1, "trelliscope") <- e2[c("facets", "facet_cols", "name", "group", "desc", "md_desc",
      "height", "width", "state", "jsonp", "self_contained", "path", "state", "nrow", "ncol",
      "scales", "thumb", "as_plotly", "plotly_args", "auto_cog_data")]
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

  pp <- ggplot_build(p)

  if (isTRUE(attrs$auto_cog_data)) {
    message("using data from the first layer")
    # data <- ggplot2::layer_data(p, 1) # first layer data # this is computed data
    data <- p$layers[[1]]$data # first layer data
    if (inherits(data, "waiver")) {
      # retrieve plot data
      data <- p$data
    }

  } else if (is.data.frame(attrs$auto_cog_data)) {
    data <- attrs$auto_cog_data
  }

  # character vect of facet columns
  # TODO need to work with facet_trelliscope(~ disp < 5)
  facet_cols <- unlist(lapply(attrs$facet_cols, as.character))
  if (!all(facet_cols %in% names(data))) {
    stop("all facet_trelliscope facet columns must be found in the data being used")
  }

  # group by all the facets
  data <- data %>%
    group_by_(.dots = lapply(facet_cols, as.symbol)) %>%
    auto_cogs()
  data$.id <- seq_len(nrow(data))

  cog_desc <- attr(data$auto_cogs, "cog_desc")

  cog_df <- data %>% select(-one_of("data")) %>% tidyr::unnest()
  cog_df <- as_cognostics(cog_df, cond_cols = facet_cols, cog_desc = cog_desc)

  # get ranges of all data
  scales_info <- upgrade_scales_param(attrs$scales, p$facet)
  scales_info <- add_range_info_to_scales(p, scales_info, attrs$facet_cols)

  # wrapper function that swaps out the data with a subset and removes the facet
  make_plot_obj <- function(dt, as_plotly = FALSE, plotly_args = NULL, pos = -1) {
    q <- p
    q$data <- dt
    q <- add_trelliscope_scales(q, scales_info, showWarnings = (pos == 1))
    if (as_plotly)
      q <- do.call(ggplotly, c(list(p = q), plotly_args))
    q
  }

  panels <- (
    data %>%
      purrr::by_row(~
        make_plot_obj(
          unnest(.x[c(facet_cols, "data")]),
          as_plotly = attrs$as_plotly,
          plotly_args = attrs$plotly_args,
          pos = .$.id
        ),
        .labels = FALSE
      )
  )[[1]]
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


plot_clone <- getFromNamespace("plot_clone", "ggplot2")

add_range_info_to_scales <- function(plot, scales_info, facet_cols) {
  x_scale_type <- scales_info$x_info$scale_type
  y_scale_type <- scales_info$y_info$scale_type

  if (
    any(
      x_scale_type != "free",
      y_scale_type != "free"
    )
  ) {

    # get the ranges from the data

    scale_plot <- plot_clone(plot)

    scales_val = switch(x_scale_type,
      free = switch(y_scale_type, same = "free_x", "free"),
      sliced = switch(y_scale_type, same = "free_x", "free"),
      same = switch(y_scale_type, same = "fixed", "free_y")
    )
    facet_part <- facet_wrap(~ ., scales = scales_val)

    if (inherits(scale_plot$facet, "FacetNull")) {
      # add a facet_wrap with scales == free and get limits
      # since can only be same here. build_plot with extra param and take limits
      facet_part$params$facets <- facet_cols

    } else {
      # can only do same (or free)
      # since can only be same here. build_plot with extra param and take limits
      facet_part$params$facets <- append(
        scale_plot$facet$params$rows,
        append(
          scale_plot$facet$params$cols,
          facet_cols
        )
      )
    }
    scale_plot <- scale_plot + facet_part

    scale_plot_built <- ggplot_build(scale_plot)

    calculate_scale_info <- function(scale_info) {
      if (scale_info$scale_type == "free") {
        return(scale_info)
      }

      plot_scales <- scale_plot_built$layout$panel_scales
      test_scale <- plot_scales[[scale_info$name]][[1]]

      if (inherits(test_scale, "ScaleDiscrete")) {
        scale_info$data_type <- "discrete"

        if (scale_info$scale_type == "sliced") {
          message(
            "facet_trelliscope does not know how to handle a 'sliced' scale for discrete data. ",
            "Using 'free' type"
          )

        } else {
          # isn't free, so can take first test_scale and reutrn range values
          scale_info$levels <- test_scale$range$range
        }


      } else {
        # continuous
        scale_info$data_type <- "continuous"

        if (scale_info$scale_type == "same") {
          # test scale is accurate for all panels
          scale_info$range <- test_scale$range$range
        }

        # "Behavior for relation="sliced" is similar, except that the length (max - min) of the scales are constrained to remain the same across panels."
        if (scale_info$scale_type == "sliced") {
          range_list <- lapply(plot_scales[[scale_info$name]], function(ps) {
            ps$range$range
          })
          diffs <- unlist(lapply(range_list, diff))

          max_diff <- diffs[which.max(diffs)]

          scale_info$width <- max_diff
        }
      }

      return(scale_info)
    }

    scales_info$x_info <- calculate_scale_info(scales_info$x_info)
    scales_info$y_info <- calculate_scale_info(scales_info$y_info)
  }

  scales_info
}


add_trelliscope_scales <- function(p, scales_info, ...) {
  p %>%
    add_trelliscope_scale(scales_info$x_info$name, scales_info$x_info, ...) %>%
    add_trelliscope_scale(scales_info$y_info$name, scales_info$y_info, ...)
}


# the goal is to add a scale if a scale doesn't already exist.
# if a scale exists, we should NOT overwrite it.
add_trelliscope_scale <- function(p, axis_name, scale_info, showWarnings = FALSE) {
  axis_scales <- p$scales$get_scales(axis_name)
  if (!is.null(axis_scales$limits)) {
    # return if there already is a limit set for this axis
    return(p)
  }

  scale_type <- scale_info$scale_type

  if (
    is.null(p$mapping[[axis_name]])
  ) {
    # this is a possibly calculated axis, leave alone
    if (
      isTRUE(showWarnings) &&
      scale_type != "free" &&
      is.null(p$scales$get_scales(axis_name))
    ) {
      # warn as it isn't a free axis
      message(
        "Axis: '", axis_name, "' is missing a global aesthetic. ",
        "Add a custom scale to change default behavior",
        call. = FALSE
      )
    }

    return(p)
  }

  if (scale_type != "free") {

    if (scale_info$data_type == "continuous") {
      scale_fn <- switch(axis_name,
        "x" = scale_x_continuous,
        "y" = scale_y_continuous,
      )

      if (scale_type == "free") {
        # do nothing
        p <- p + scale_fn(limits = c(NA, NA)) # "Use NA to refer to the existing minimum or maximum."

      } else if (scale_type == "same") {
        p <- p + scale_fn(limits = scale_info$range)

      } else if (scale_type == "sliced") {
        eval(p$mapping[[axis_name]], envir = p$data) %>%
          range(na.rm = TRUE) ->
        dt_range

        mid_range_val <- mean(dt_range)

        width <- scale_info$width
        limits <- c(mid_range_val - 1/2 * width, mid_range_val + 1/2 * width)

        if (!isTRUE(all.equal(dt_range, limits))) {
          # this if check is done to avoid silly R floating point rounding errors
          # this situation should only happen twice. one for each axis
          p <- p + scale_fn(limits = limits)
        }

      }

    } else if (scale_info$data_type == "discrete") {

      data_column <- eval(p$mapping[[axis_name]], envir = p$data)

      scale_fn <- switch(axis_name,
        "x" = scale_x_discrete,
        "y" = scale_y_discrete,
      )

      if (scale_type == "free") {
        # at least have them appear in the same order
        p <- p + scale_fn(limits = scale_info$levels, drop = TRUE)

      } else if (scale_type == "same") {
        p <- p + scale_fn(limits = scale_info$levels, drop = FALSE)

      }

    }
  }

  p
}
