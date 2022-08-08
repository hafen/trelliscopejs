utils::globalVariables(c(".", "ggplotly"))

#' Facet Trelliscope
#'
#' @param facets formula to facet the panels on. Similar to \code{ggplot2::\link[ggplot2]{facet_wrap}}'s \code{facets}
#' @param name name of the display
#' @param group group that the display belongs to
#' @param desc description of the display
#' @param md_desc optional string of markdown that will be shown in the viewer for additional context about the display
#' @param path the base directory of the trelliscope application
#' @param height height in pixels of each panel
#' @param width width in pixels of each panel
#' @param inputs optional set of input specifications (using \code{\link{input_cogs}}) to allow user input for each panel
#' @param state the initial state the display will open in
#' @param views an optional list of pre-specified views of the display (experimental)
#' @param nrow the number of rows of panels to display by default
#' @param ncol the number of columns of panels to display by default
#' @param scales should scales be the same (\code{"same"}, the default), free (\code{"free"}), or sliced (\code{"sliced"}). May provide a single string or two strings, one for the X and Y axis respectively.
#' @param jsonp should json for display object be jsonp (TRUE) or json (FALSE)?
#' @param as_plotly should the panels be written as plotly objects?
#' @param plotly_args optional named list of arguments to send to \code{ggplotly}
#' @param plotly_cfg optional named list of arguments to send to plotly's \code{config} method
#' @param split_sig optional string that specifies the "signature" of the data splitting. If not specified, this is calculated as the md5 hash of the sorted unique facet variables. This is used to identify "related displays" - different displays that are based on the same faceting scheme. This parameter should only be specified manually if a display's faceting is mostly similar to another display's.
#' @param self_contained should the Trelliscope display be a self-contained html document? (see note)
#' @param thumb should a thumbnail be created?
#' @param require_token require a special token for all displays to be visible (experimental)
#' @param google_analytics_id optional string specifying Google Analytics ID
#' @param id set a hard-coded ID for this app (do not set this if the display will be part of a larger web page)
#' @param order an integer indicating the order that the display should appear in if using multiple displays
#' @param disclaimer an optional string of html to include as a disclaimer for the set of displays
#' @param auto_cog should auto cogs be computed (if possible)?
#' @param data data used for faceting. Defaults to the first layer data
#' @note Note that \code{self_contained} is severely limiting and should only be used in cases where you would either like your display to show up in the RStudio viewer pane, in an interactive R Markdown Notebook, or in a self-contained R Markdown html document.

#' @template param-split-layout
#' @note Note that \code{self_contained} is severely limiting and should only be used in cases where you would either like your display to show up in the RStudio viewer pane, in an interactive R Markdown Notebook, or in a self-contained R Markdown html document.
#' @export
#' @example man-roxygen/ex-facet_trelliscope.R
#' @importFrom ggplot2 facet_wrap
#' @importFrom rlang as_name
facet_trelliscope <- function(
  facets,
  nrow = 1, ncol = 1, scales = "same", name = NULL, group = "common",
  desc = ggplot2::waiver(), md_desc = ggplot2::waiver(), path = NULL,
  height = 500, width = 500, inputs = NULL,
  state = NULL, views = NULL, jsonp = TRUE, as_plotly = FALSE,
  plotly_args = NULL, plotly_cfg = NULL, split_sig = NULL,
  google_analytics_id = NULL,
  self_contained = FALSE, thumb = TRUE, require_token = FALSE, id = NULL,
  order = 1, disclaimer = FALSE, update_plots = TRUE, auto_cog = FALSE, split_layout = FALSE,
  data = ggplot2::waiver()
) {
  if (split_layout)
    stop("Sorry - the viewer doesn't support rendering split layout yet...")

  if (as_plotly) {
    if (!requireNamespace("plotly", quietly = TRUE))
      stop("Package 'plotly' is needed for as_plotly = TRUE Please install it.",
        call. = FALSE)
  }

  # facets_list <- ggplot2:::as_facets_list(facets)
  # facets <- rlang::flatten_if(facets_list, rlang::is_list)

  ret <- list(
    facets = facets,
    facet_cols = facet_wrap(facets)$params$facets,
    name = name,
    group = group,
    desc = desc,
    md_desc = md_desc,
    height = height,
    width = width,
    inputs = inputs,
    state = state,
    jsonp = jsonp,
    split_sig = split_sig,
    path = path,
    self_contained = self_contained,
    google_analytics_id = google_analytics_id,
    nrow = nrow,
    ncol = ncol,
    scales = scales,
    thumb = thumb,
    as_plotly = as_plotly,
    plotly_args = plotly_args,
    plotly_cfg = plotly_cfg,
    auto_cog = auto_cog,
    split_layout = split_layout,
    id = id,
    disclaimer = disclaimer,
    update_plots = update_plots,
    data = data,
    views = views
  )

  class(ret) <- "facet_trelliscope"
  ret
}

#' @export
ggplot_add.facet_trelliscope <- function(object, plot, object_name) {
  attr(plot, "trelliscope") <- object[
    c("facets", "facet_cols", "name", "group",
      "desc", "md_desc", "height", "width", "inputs", "state", "jsonp", "self_contained", "google_analytics_id",
      "path", "state", "nrow", "ncol", "scales", "thumb", "as_plotly",
      "split_sig", "plotly_args", "plotly_cfg", "auto_cog", "split_layout",
      "id", "disclaimer", "update_plots", "data", "views")]
  class(plot) <- c("facet_trelliscope", class(plot))
  return(plot)
}

# #' Add method for gg / facet_trelliscope
# #' @param e1 a object with class gg
# #' @param e2 if object is of class 'facet_trelliscope', then 'facet_trelliscope' will be appended to the class of e1
# #' @export
# #' @importFrom ggplot2 %+%
# `+.gg` <- function (e1, e2) {
#   if (inherits(e2, "facet_trelliscope")) {

#     # e1 <- e1 %+% (e2$facet_wrap)
#     attr(e1, "trelliscope") <- e2[c("facets", "facet_cols", "name", "group",
#       "desc", "md_desc", "height", "width", "state", "jsonp", "self_contained",
#       "path", "state", "nrow", "ncol", "scales", "thumb", "as_plotly",
#       "split_sig", "plotly_args", "plotly_cfg", "auto_cog", "split_layout",
#       "data")]
#     class(e1) <- c("facet_trelliscope", class(e1))
#     return(e1)
#     # return(print(e1))
#   }

#   e1 %+% e2
# }


#' Print facet trelliscope object
#'
#' @param x plot object
#' @param ... ignored
#' @import dplyr
#' @importFrom stats as.formula
#' @importFrom tidyr nest nest_ unnest
#' @importFrom ggplot2 ggplot_build
#' @export
print.facet_trelliscope <- function(x, ...) {

  attrs <- attr(x, "trelliscope")

  # copy for better name
  p <- x
  # remove special class
  class(p) <- setdiff(class(p), "facet_trelliscope")

  pp <- ggplot2::ggplot_build(p)

  if (isTRUE(attrs$split_layout)) {
    first_panel_scales <- pp$layout$get_scales(1)
    if (!identical(first_panel_scales$x$position, "bottom")) {
      stop("x axis must be on the bottom if the layout is split")
    }
    if (!identical(first_panel_scales$y$position, "left")) {
      stop("y axis must be on the left if the layout is split")
    }
  }

  if (inherits(attrs$data, "waiver")) {
    message("using data from the first layer")
    # data <- ggplot2::layer_data(p, 1) # first layer data # this is computed data
    data <- p$layers[[1]]$data # first layer data
    if (inherits(data, "waiver")) {
      # retrieve plot data
      data <- p$data
    }
  } else {
    # user supplied
    data <- attrs$data
  }

  if (is.null(data)) {
    stop("non-NULL data must be provided either in the first plot layer ",
      "or in the 'data' parameter")
  }

  # character vector of facet columns
  # TODO need to work with facet_trelliscope(~ disp < 5)
  facet_cols <- unlist(lapply(attrs$facet_cols, rlang::as_name))
  facet_cols <- setdiff(facet_cols, "~")
  if (!all(facet_cols %in% names(data))) {
    stop("all facet_trelliscope facet columns must be found in the ",
      "data being used")
  }

  for (fcol in facet_cols) {
    if (is.factor(data[[fcol]]))
      data[[fcol]] <- as.character(data[[fcol]])
  }

  data_cog_attrs <- lapply(data, function(x) {
    attr(x, "cog_attrs")
  })

  # group by all the facets
  data <- data %>%
    ungroup() %>%
    dplyr::mutate(.id = row_number()) %>%
    dplyr::mutate(.id = seq_len(nrow(data))) %>%
    dplyr::group_by_at(facet_cols) %>%
    tidyr::nest() %>%
    dplyr::ungroup()

  # get ranges of all data
  scales_info <- upgrade_scales_param(attrs$scales, p$facet)
  scales_info <- add_range_info_to_scales(p, scales_info, attrs$facet_cols)

  # wrapper function that swaps out the data with a subset and removes the facet
  make_plot_obj <- function(dt, pos = -1) {
    q <- p
    # dt$data <- lapply(dt$data, function(x) {
    #   idx <- which(unlist(lapply(x, function(a) inherits(a, "cog"))))
    #   for (ii in idx) {
    #     class(x[[ii]]) <- setdiff(class(x[[ii]]), "cog")
    #   }
    #   x
    # })
    # q$data <- tidyr::unnest(dt, data)
    nms <- setdiff(names(dt), "data")
    tmp <- dt$data[[1]]
    for (nm in nms) tmp[[nm]] <- dt[[nm]]
    q$data <- tmp[, c(nms, setdiff(names(tmp), nms))]
    q <- add_trelliscope_scales(q, scales_info, show_warnings = (pos == 1))
    q
  }

  data$panel <- lapply(seq_len(nrow(data)), function(i) {
    make_plot_obj(data[i, ])
  })

  cog_info <- select(data, -data) %>% cog_df_info(
    panel_col = "panel",
    state = attrs$state,
    auto_cog = attrs$auto_cog,
    nested_data_list = data$data,
    nested_cog_attrs = data_cog_attrs
  )
  cog_df <- cog_info$cog_df
  attrs$state <- cog_info$state

  panels <- data$panel

  if (isTRUE(attrs$as_plotly)) {
    plotly_args <- attrs$plotly_args
    panels <- panels %>%
      lapply(function(q) {
        do.call(plotly::ggplotly, c(list(p = q), plotly_args))
      })
    if (!is.null(attrs$plotly_cfg)) {
      plotly_cfg <- attrs$plotly_cfg
      panels <- panels %>%
        lapply(function(q) {
          do.call(plotly::config, c(list(p = q), plotly_cfg))
        })
    }
  }

  names(panels) <- cog_df$panelKey # nolint

  name <- attrs$name
  if (is.null(name))
    name <- paste("by_", paste(facet_cols, collapse = "_"), sep = "")

  params <- resolve_app_params(
    path = attrs$path,
    self_contained = attrs$self_contained,
    jsonp = attrs$jsonp,
    split_sig = attrs$split_sig,
    name = name,
    group = attrs$group,
    state = attrs$state,
    nrow = attrs$nrow,
    ncol = attrs$ncol,
    thumb = attrs$thumb,
    split_layout = attrs$split_layout,
    id = attrs$id,
    disclaimer = attrs$disclaimer,
    # update_plots = attrs$update_plots,
    inputs = attrs$inputs
  )

  pb <- progress::progress_bar$new(
    format = ":what [:bar] :percent :current/:total eta::eta",
    total = 5 + length(panels), width = getOption("width") - 8)
  pb$tick(0, tokens = list(what = "calculating         "))

  if (attrs$update_plots) {
    write_panels(
      panels,
      base_path = params$path,
      name = params$name,
      group = params$group,
      width = attrs$width,
      height = attrs$height,
      jsonp = params$jsonp,
      split_layout = params$split_layout,
      pb = pb
    )
  }

  if (inherits(attrs$desc, "waiver"))
    attrs$desc <- ifelse(is.null(p$labels$title), "", p$labels$title)

  if (inherits(attrs$md_desc, "waiver"))
    attrs$md_desc <- ifelse(is.null(p$labels$subtitle), "", p$labels$subtitle)

  if (params$split_layout) {
    has_legend <- "guide-box" %in% plot_gtable(panels[[1]])$layout$name
  } else {
    # if it isn't split, we will say there is no legend to draw
    # as it will be drawn in the regular plot
    has_legend <- FALSE
  }

  split_aspect <- NULL
  if (params$split_layout) {
    # we need to store the aspect ratio of the axes if it's split
    left_axis <- extract_axis_left(panels[[1]])
    bottom_axis <- extract_axis_bottom(panels[[1]])

    lft <- get_png_units(axis_left_width(left_axis), attrs$height)
    bot <- get_png_units(attrs$width, axis_bottom_height(bottom_axis))

    split_aspect <- list(
      left = lft$height / lft$width,
      bottom = bot$height / bot$width)
  }

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
    inputs = params$inputs,
    google_analytics_id = attrs$google_analytics_id,
    md_desc = attrs$md_desc,
    state = params$state,
    jsonp = params$jsonp,
    thumb = params$thumb,
    split_layout = params$split_layout,
    split_aspect = split_aspect,
    has_legend,
    split_sig = params$split_sig,
    views = attrs$views,
    order = order,
    pb = pb
  )

  prepare_display(
    params$path, params$id, params$self_contained, params$jsonp,
    attrs$require_token, params$disclaimer, pb = pb)

  res <- trelliscope_widget(
    id = params$id,
    www_dir = params$www_dir,
    latest_display = list(name = params$name, group = params$group),
    self_contained = params$self_contained,
    dependencies = get_dependencies(panels[[1]]),
    config_info = params$config_path,
    spa = params$spa
  )

  # return early for knitr or shiny
  if (params$in_knitr || params$in_shiny) {
    return(res)
  }

  attr(res, "fidelius_pars") <- attr(x, "fidelius_pars") 

  print(res)
}





upgrade_scales_param <- function(scales, plot_facet) {
  if (length(scales) > 2)
    stop("scales must not be longer than length 2")

  if (any(is.na(scales)) || is.null(scales) || length(scales) == 0)
    stop("scales must be a character vector of size 1 or 2")

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

  if (!all(scales %in% c("same", "free", "sliced")))
    stop("a length 2 scales parameter can only be made of 'same', 'free', or 'sliced' values")

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

  list(
    x_info = list(name = "x", scale_type = scales[1]),
    y_info = list(name = "y", scale_type = scales[2]))
}

plot_gtable <- function(p) {
  ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
}

str_detect <- function(x, pattern) {
  grepl(pattern, x)
}

axis_left_width <- function(pg, unit_to = "cm") {
  grid::convertWidth(sum(grid::convertWidth(pg$widths, unitTo = unit_to)),
    unitTo = unit_to)
}

axis_bottom_height <- function(pg, unit_to = "cm") {
  grid::convertHeight(sum(grid::convertHeight(pg$heights, unitTo = unit_to)),
    unitTo = unit_to)
}

legend_width_or_height <- function(pg, section, default_value, unit_to = "cm") {
  val <- grid::convertHeight(pg[[section]][1], unitTo = unit_to,
    valueOnly = TRUE)
  if (val == 0) {
    default_value
  } else {
    grid::unit(val, unit_to)
  }
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


#' @importFrom gtable gtable_filter
extract_legend <- function(p, pg = plot_gtable(p)) {
  if (!("guide-box" %in% pg$layout$name)) {
    return(NULL)
  }

  gg_legend <- gtable_filter(pg, "guide-box")

  gg_legend
}

plot_clone <- utils::getFromNamespace("plot_clone", "ggplot2")

#' @importFrom utils packageVersion
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

    scales_val <- switch(x_scale_type,
      free = switch(y_scale_type, same = "free_x", "free"),
      sliced = switch(y_scale_type, same = "free_x", "free"),
      same = switch(y_scale_type, same = "fixed", "free_y")
    )

    if (packageVersion("ggplot2") > "2.2.1") {
      facet_part <- facet_wrap(vars(facet_cols), scales = scales_val)
    } else {
      facet_part <- facet_wrap(facet_cols, scales = scales_val)
    }

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

    calculate_scale_info <- function(scale_info, plot_scales) {
      test_scale <- plot_scales[[1]]
      scale_info$scale <- test_scale

      if (inherits(test_scale, "ScaleDiscrete")) {
        scale_info$data_type <- "discrete"

        if (scale_info$scale_type == "sliced") {
          message(
            "facet_trelliscope does not know how to handle a 'sliced' scale for discrete data. ",
            "Using 'free' type"
          )
          scale_info$scale_type <- "free"
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

        # Behavior for relation="sliced" is similar, except that the length (max - min)
        # of the scales are constrained to remain the same across panels."
        if (scale_info$scale_type == "sliced") {
          range_list <- lapply(plot_scales, function(ps) {
            ps$range$range
          })
          diffs <- unlist(lapply(range_list, diff))

          max_diff <- diffs[which.max(diffs)]

          scale_info$width <- max_diff
        }
      }

      return(scale_info)
    }

    if (packageVersion("ggplot2") > "2.2.1") {
      scales_info$x_info <- calculate_scale_info(
        scales_info$x_info,
        scale_plot_built$layout$panel_scales_x
      )
      scales_info$y_info <- calculate_scale_info(
        scales_info$y_info,
        scale_plot_built$layout$panel_scales_y
      )
    } else {
      scales_info$x_info <- calculate_scale_info(
        scales_info$x_info,
        scale_plot_built$layout$panel_scales[[scales_info$x_info$name]]
      )
      scales_info$y_info <- calculate_scale_info(
        scales_info$y_info,
        scale_plot_built$layout$panel_scales[[scales_info$y_info$name]]
      )
    }
  }

  scales_info
}

add_trelliscope_scales <- function(p, scales_info, ...) {
  p %>%
    add_trelliscope_scale(scales_info$x_info$name, scales_info$x_info, ...) %>%
    add_trelliscope_scale(scales_info$y_info$name, scales_info$y_info, ...)
}

#' @importFrom rlang eval_tidy
# the goal is to add a scale if a scale doesn't already exist.
# if a scale exists, we should NOT overwrite it.
add_trelliscope_scale <- function(p, axis_name, scale_info, show_warnings = FALSE) {
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
      isTRUE(show_warnings) &&
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
      # scale_fn <- switch(axis_name,
      #   "x" = scale_x_continuous,
      #   "y" = scale_y_continuous,
      # )
      #
      scale_fn <- switch(class(scale_info$scale)[1],
        "ScaleContinuousPosition" =
          switch(axis_name, "x" = scale_x_continuous, "y" = scale_y_continuous),
        "ScaleContinuousTime" =
          switch(axis_name, "x" = scale_x_time, "y" = scale_y_time),
        "ScaleContinuousDate" =
          switch(axis_name, "x" = scale_x_date, "y" = scale_y_date),
        "ScaleContinuousDatetime" =
          switch(axis_name, "x" = scale_x_datetime, "y" = scale_y_datetime)
      )

      if (scale_type == "free") {
        # "Use NA to refer to the existing minimum or maximum."
        p <- p + scale_fn(limits = c(NA, NA))

      } else if (scale_type == "same") {
        # have to make the scale and set the information manually as dates are formatted as numeric
        # p <- p + scale_fn(limits = c(NA, NA))
        scale_item <- scale_fn()
        scale_item$limits <- scale_info$range
        p <- p + scale_item

      } else if (scale_type == "sliced") {
        if (packageVersion("ggplot2") > "2.2.1") {
          dt_range <- rlang::eval_tidy(p$mapping[[axis_name]], data = p$data) %>%
            range(na.rm = TRUE)
        } else {
          dt_range <- eval(p$mapping[[axis_name]], envir = p$data) %>%
            range(na.rm = TRUE)
        }

        mid_range_val <- mean(dt_range)

        width <- scale_info$width
        limits <- c(mid_range_val - 1 / 2 * width, mid_range_val + 1 / 2 * width)

        if (!isTRUE(all.equal(dt_range, limits))) {
          # this if check is done to avoid silly R floating point rounding errors
          # this situation should only happen twice. one for each axis
          p <- p + scale_fn(limits = limits)
        }
      }
    } else if (scale_info$data_type == "discrete") {
      # data_column <- eval(p$mapping[[axis_name]], envir = p$data)

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
