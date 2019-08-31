#' Write a list of plot objects as panels in a Trelliscope display
#'
#' @param plot_list a named list of plot objects to be written as panels (objects can be trellis, ggplot2, or htmlwidget) with the list names being the keys for the panels
#' @param pb optional progress bar object to pass in and use to report progress
#' @param ... params passed directly to \code{\link{write_panel}}
#' @import progress
#' @export
write_panels <- function(plot_list, ..., pb = NULL) {

  nms <- names(plot_list)
  if (length(nms) == 0) {
    stop_nice("panels must be a named list, with the names being used as the panel key")
  }

  if (is.null(pb))
    pb <- progress::progress_bar$new(
      total = length(nms), width = getOption("width") - 5,
      format = ":what [:bar] :percent :current/:total eta::eta")

  lapply(nms, function(nm) {
    pb$tick(tokens = list(what = "writing panels      "))
    write_panel(plot_list[[nm]], key = nm, ...)
  })

  invisible(NULL)
}

#' Write a plot object as a panel in a Trelliscope display
#'
#' @param plot_object a plot object to be written (can be trellis, ggplot2, or htmlwidget)
#' @param key a string identifying the panel key, which will be used as the panel file name and which the \code{panelKey} column of the cognostics data frame should point to
#' @param base_path the base directory of the trelliscope application
#' @param name name of the display that the panel belongs to
#' @param group group name of the display that the panel belongs to
#' @param width width in pixels of each panel
#' @param height height in pixels of each panel
#' @param jsonp should json for panel be jsonp (TRUE) or json (FALSE)?
#' @template param-split-layout
#' @export
write_panel <- function(plot_object, key, base_path, name, group = "common",
  width, height, jsonp = TRUE, split_layout = FALSE) {

  panel_path <- file.path(base_path, "displays", group, name,
    ifelse(jsonp, "jsonp", "json"))

  if (!dir.exists(panel_path)) {
    res <- dir.create(panel_path, recursive = TRUE)
    if (!res) {
      warning("There was an issue creating directory to store panels in: ", panel_path,
        ".  Panel will not be created.")
      return(invisible(NULL))
    }
  }

  if (inherits(plot_object, "ggplot")) {
    if (split_layout) {
      pg <- plot_gtable(plot_object)
      left_axis <- extract_axis_left(pg = pg)
      bottom_axis <- extract_axis_bottom(pg = pg)
      plot_content <- extract_plot_content(pg = pg)

      write_ggplot2_component(
        plot_content, width = width, height = height, key = paste0(key, "_plot"), name = name,
        jsonp, panel_path
      )
      write_ggplot2_component(
        left_axis,
        width = axis_left_width(left_axis),
        height = height, key = paste0(key, "_axis_left"), name = name,
        jsonp, panel_path
      )
      write_ggplot2_component(
        bottom_axis, width = width,
        height = axis_bottom_height(bottom_axis),
        key = paste0(key, "_axis_bottom"), name = name,
        jsonp, panel_path
      )

      gg_legend <- extract_legend(pg = pg)
      if (!is.null(gg_legend)) {

        legend_width <- legend_width_or_height(gg_legend, "widths", width)
        legend_height <- legend_width_or_height(gg_legend, "heights", height)

        write_ggplot2_component(
          gg_legend,
          width = legend_width,
          height = legend_height,
          key = paste0(key, "_legend"),
          name = name,
          jsonp, panel_path
        )
      }
    } else {
      # behave like a normal plot
      write_ggplot2_component(plot_object, width, height, key, name, jsonp, panel_path)
    }

  } else if (inherits(plot_object, c("trellis"))) {
    write_ggplot2_component(plot_object, width, height, key, name, jsonp, panel_path)

  } else if (inherits(plot_object, "htmlwidget")) {
    p <- htmltools::as.tags(plot_object)
    txt <- get_jsonp_text(jsonp, paste0("__panel__._", key, "_", name))
    cat(paste0(txt$st, p[[2]]$children[[1]], txt$nd),
      file = file.path(panel_path,
        paste0(key, ifelse(jsonp, ".jsonp", ".json"))))
  } else {
    message("panel not written - must be trellis, ggplot, or htmlwidget object")
  }
}

unit_to_px <- function(x, res) {
  ret <- unclass(grid::convertWidth(x, unitTo = "cm")) * res / 2.54
  attr(ret, "unit") <- NULL
  attr(ret, "valid.unit") <- NULL
  ret
}

write_ggplot2_component <- function(
  plot_component, width, height, key, name, jsonp, panel_path,
  file = tempfile()
) {
  ff <- file
  # dir.create(file.path(".", "_ignore", "_pics"), showWarnings = FALSE, recursive = TRUE)
  # ff <- file.path(".", "_ignore", "_pics", paste0(key, ".png"))
  make_png(p = plot_component, file = ff,
    width = width, height = height)
  dat <- paste0("\"", encode_png(ff), "\"")
  txt <- get_jsonp_text(jsonp, paste0("__panel__._", key, "_", name))
  cat(paste0(txt$st, dat, txt$nd),
    file = file.path(panel_path,
      paste0(key, ifelse(jsonp, ".jsonp", ".json"))))
}

#' Write cognostics data for a display in a Trelliscope app
#'
#' @param cogdf a data frame of cognostics, prepared with \code{\link{as_cognostics}}
#' @param base_path the base directory of the trelliscope application
#' @param id a unique id for the application
#' @param name name of the display
#' @param group group that the display belongs to
#' @param jsonp should json for cognostics be jsonp (TRUE) or json (FALSE)?
#' @importFrom jsonlite toJSON
#' @export
write_cognostics <- function(cogdf, base_path, id, name, group = "common", jsonp = TRUE) {
  display_path <- file.path(base_path, "displays", group, name)
  txt <- get_jsonp_text(jsonp, paste0("__loadCogData__", id, "_", group, "_", name))
  jsn <- jsonlite::toJSON(cogdf)
  chk <- jsonlite::validate(jsn)
  if (!chk)
    stop_nice("There are issues with the cognostics data that are causing",
      "invalid json to be generated:\n", attr(chk, "err"))

  cat(paste0(txt$st, jsn, txt$nd),
    file = file.path(display_path,
      paste0("cogData.", ifelse(jsonp, "jsonp", "json"))))
}

#' Write a "display object" file for a Trelliscope app
#'
#' @param cogdf a data frame of cognostics, prepared with \code{\link{as_cognostics}}
#' @param panel_example an example object of one panel of a display (can be trellis, ggplot2, or htmlwidget object)
#' @param base_path the base directory of the trelliscope application
#' @param id a unique id for the application
#' @param name name of the display
#' @param group group that the display belongs to
#' @param desc description of the display
#' @param height height in pixels of each panel
#' @param width width in pixels of each panel
#' @param md_desc optional string of markdown that will be shown in the viewer for additional context about the display
#' @param state the initial state the display will open in
#' @param jsonp should json for display object be jsonp (TRUE) or json (FALSE)?
#' @param split_sig optional string "signature" specifying the data splitting
#' @param panel_img_col which column (if any) is a panel image column?
#' @param self_contained should the Trelliscope display be a self-contained html document?
#' @param thumb should a thumbnail be created?
#' @template param-split-layout
#' @template param-split-aspect
#' @template param-has-legend
#' @param pb optional progress bar object to pass in and use to report progress
#' @importFrom digest digest
#' @export
write_display_obj <- function(cogdf, panel_example, base_path, id, name, group = "common",
  desc = "", height = 500, width = 500, md_desc = "", state = NULL, jsonp = TRUE,
  split_sig = NULL, panel_img_col = NULL, self_contained = FALSE, thumb = TRUE, 
  split_layout = FALSE, split_aspect = NULL, has_legend = FALSE, pb = NULL) {

  display_path <- file.path(base_path, "displays", group, name)
  panel_path <- file.path(display_path, ifelse(jsonp, "jsonp", "json"))

  if (! inherits(cogdf, "cognostics"))
    stop_nice("cogdf must be a cognostics object -",
      "call as_cognostics() to cast it as such.")

  if (inherits(panel_example, "htmlwidget")) {
    if (!is.null(panel_example$height))
      height <- panel_example$height
    if (!is.null(panel_example$width))
      width <- panel_example$width
  }

  if (!is.null(pb))
    pb$tick(tokens = list(what = "building display obj"))

  panelInterface <- list()
  if (inherits(panel_example, "htmlwidget")) {
    panelInterface$type <- ifelse(inherits(panel_example, "htmlwidget"),
      "htmlwidget", "image")
    panelInterface$deps <- get_and_write_widget_deps(panel_example, base_path, self_contained)
  } else if (inherits(panel_example, "img_panel")) {
    panelInterface$type <- "image_src"
    panelInterface$panelCol <- panel_img_col
  } else {
    panelInterface$type <- "image"
  }

  key_sig <- digest::digest(sort(cogdf$panelKey))
  if (!is.null(split_sig)) {
    key_sig <- split_sig
  }

  imgSrcLookup <- NULL
  if (panelInterface$type == "image_src") {
    imgSrcLookup <- as.list(cogdf[[panelInterface$panelCol]])
    names(imgSrcLookup) <- cogdf$panelKey
    cogdf[[panelInterface$panelCol]] <- NULL
  }

  disp_obj <- list(
    name = name,
    group = group,
    desc = desc,
    mdDesc = md_desc,
    updated = Sys.time(),
    n = nrow(cogdf),
    height = height,
    width = width,
    has_legend = has_legend,
    split_layout = split_layout,
    split_aspect = split_aspect,
    keySig = key_sig, # nolint
    cogInterface = list(name = name, group = group, type = "JSON"),
    panelInterface = panelInterface,
    imgSrcLookup = imgSrcLookup,
    cogInfo = get_cog_info(cogdf),
    cogDistns = get_cog_distributions(cogdf)
  )

  # TODO: add state validation
  if (is.null(state))
    state <- list()

  if (is.null(state$layout))
    state$layout <- list(nrow = 1, ncol = 1, arrange = "row")
  if (is.null(state$labels)) {
    def_labels <- which(sapply(disp_obj$cogInfo, function(x) x$defLabel)) # nolint
    if (length(def_labels) > 0)
      state$labels <- I(names(disp_obj$cogInfo)[def_labels]) # nolint
  }

  if (is.null(state$sort)) {
    cond_vars <- which(sapply(disp_obj$cogInfo, function(x) x$group == "condVar")) # nolint
    if (length(cond_vars) > 0) {
      nms <- names(disp_obj$cogInfo)[cond_vars] # nolint
      state$sort <- lapply(seq_along(nms), function(ii)
        list(order = ii, name = nms[ii], dir = "asc"))
    }
  }
  disp_obj$state <- state

  if (!dir.exists(display_path))
    dir.create(display_path, recursive = TRUE)

  txt <- get_jsonp_text(jsonp, paste0("__loadDisplayObj__", id, "_", group, "_", name))
  cat(paste0(txt$st,
    jsonlite::toJSON(disp_obj, auto_unbox = TRUE), txt$nd),
    file = file.path(display_path,
      paste0("displayObj.", ifelse(jsonp, "jsonp", "json"))))

  if (!is.null(pb))
    pb$tick(tokens = list(what = "writing cognostics  "))

  write_cognostics(cogdf, base_path, id = id, name = name, group = group, jsonp = jsonp)

  thumb_message <- ifelse(thumb, "writing thumbnail   ", "empty thumbnail     ")

  # if thumb is false, we still write a blank one...
  if (!is.null(pb))
    pb$tick(tokens = list(what = thumb_message))
  write_thumb(panel_example,
    file.path(display_path, "thumb.png"), width = width, height = height,
    thumb = thumb)
}

#' Set up all auxiliary files needed for a Trelliscope app
#'
#' @param base_path the base directory of the trelliscope application
#' @param id a unique id for the application
#' @param self_contained should the Trelliscope display be a self-contained html document?
#' @param jsonp should json for display list and app config be jsonp (TRUE) or json (FALSE)?
#' @param pb optional progress bar object to pass in and use to report progress
#' @export
prepare_display <- function(
  base_path,
  id,
  self_contained = FALSE,
  jsonp = TRUE,
  pb = NULL
) {

  # write the id to a plain text file
  cat(id, file = file.path(base_path, "id"))
  if (!is.null(pb))
    pb$tick(tokens = list(what = "writing display list"))
  update_display_list(base_path, jsonp = jsonp)
  if (!is.null(pb))
    pb$tick(tokens = list(what = "writing app config  "))

  write_config(
    base_path,
    id = id,
    self_contained = self_contained,
    jsonp = jsonp
  )
}

#' Update Trelliscope app display list file
#'
#' @param base_path the base directory of the trelliscope application
#' @param jsonp should json for display list be jsonp (TRUE) or json (FALSE)?
#' @importFrom jsonlite fromJSON
#' @export
update_display_list <- function(base_path, jsonp = TRUE) {
  id <- readLines(file.path(base_path, "id"), warn = FALSE)

  # read all displayObj files and write them into a display list
  groups <- list.files(file.path(base_path, "displays"), full.names = TRUE)
  names <- unlist(lapply(groups, function(gp) {
    list.files(gp, full.names = TRUE)
  }))

  display_list <- lapply(names, function(path) {
    ff <- file.path(path, "displayObj.json")
    if (file.exists(ff)) {
      obj <- jsonlite::fromJSON(ff)
    } else {
      ff <- file.path(path, "displayObj.jsonp")
      tmp <- readLines(ff, warn = FALSE)
      rgxp <- paste0("^__loadDisplayObj__[a-zA-Z0-9_/\\.]+\\((.*)\\)")
      obj <- jsonlite::fromJSON(gsub(rgxp, "\\1", tmp))
    }
    list(
      group = obj$group,
      name = obj$name,
      desc = obj$desc,
      n = obj$n,
      height = obj$height,
      width = obj$width,
      updated = obj$updated,
      keySig = obj$keySig # nolint
    )
  })

  txt <- get_jsonp_text(jsonp, paste0("__loadDisplayList__", id))
  cat(paste0(txt$st,
      jsonlite::toJSON(display_list, pretty = TRUE, auto_unbox = TRUE), txt$nd),
      file = file.path(base_path, "displays",
        paste0("displayList.", ifelse(jsonp, "jsonp", "json"))))
}

#' Write Trelliscope app configuration file
#'
#' @param base_path the base directory of the trelliscope application
#' @param id a unique id for the application
#' @param self_contained should the Trelliscope display be a self-contained html document?
#' @param jsonp should json for app config be jsonp (TRUE) or json (FALSE)?
#' @template param-split-layout
#' @template param-has-legend
#' @export
write_config <- function(base_path, id, self_contained = FALSE, jsonp = TRUE, split_layout = FALSE, has_legend = FALSE) {
  cfg <- as.character(jsonlite::toJSON(
    list(
      display_base = ifelse(self_contained, "__self__", "displays"),
      data_type = ifelse(jsonp, "jsonp", "json"),
      cog_server = list(
        type = ifelse(jsonp, "jsonp", "json"),
        info = list(base = ifelse(self_contained, "__self__", "displays"))
      ),
      split_layout = split_layout,
      has_legend = has_legend
    ),
    pretty = TRUE,
    auto_unbox = TRUE
  ))

  txt <- get_jsonp_text(jsonp, paste0("__loadTrscopeConfig__", id))
  cat(paste0(txt$st, cfg, txt$nd),
    file = file.path(base_path,
      paste0("config", ifelse(jsonp, ".jsonp", ".json"))))
}

# display_analyze <- function(base_path) {
#   # make sure everything is in place and formatted properly for viewing
#   # for example, make sure keys in cognostics match panel file names, etc.
# }
