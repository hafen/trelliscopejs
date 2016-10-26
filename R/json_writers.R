#' Write a list of plot objects as panels in a Trelliscope display
#'
#' @param plot_list a named list of plot objects to be written as panels (objects can be trellis, ggplot2, or htmlwidget) with the list names being the keys for the panels
#' @param base_path the base directory of the trelliscope application
#' @param name name of the display that the panel belongs to
#' @param group group name of the display that the panel belongs to
#' @param width width in pixels of each panel
#' @param height height in pixels of each panel
#' @param jsonp should json for panels be jsonp (TRUE) or json (FALSE)?
#' @param progress = TRUE
#' @import progress
#' @export
write_panels <- function(plot_list, base_path, name, group = "common",
  width = 500, height = 500, jsonp = TRUE, progress = TRUE) {

  nms <- names(plot_list)
  if (length(nms) == 0) {
    stop("panels must be a named list, with the names being used as the panel key")
  }

  message("writing panels...")
  pb <- progress::progress_bar$new(
    total = length(nms),
    format = "[:bar] :percent :current/:total eta::eta"
  )
  lapply(nms, function(nm) {
    pb$tick()
    write_panel(plot_list[[nm]], key = nm, base_path = base_path,
      name = name, group = group,
      width = width, height = height, jsonp = jsonp)
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
#' @export
write_panel <- function(plot_object, key, base_path, name, group = "common",
  width, height, jsonp = TRUE) {

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

  if (inherits(plot_object, c("trellis", "ggplot"))) {
    ff <- tempfile()
    make_png(p = plot_object, file = ff,
      width = width, height = height)
    dat <- paste0("\"", encode_png(ff), "\"")
    txt <- get_jsonp_text(jsonp, paste0("__panel__._", key))
    cat(paste0(txt$st, dat, txt$nd),
      file = file.path(panel_path,
        paste0(key, ifelse(jsonp, ".jsonp", ".json"))))
  } else if (inherits(plot_object, "htmlwidget")) {
    p <- htmltools::as.tags(plot_object)
    txt <- get_jsonp_text(jsonp, paste0("__panel__._", key))
    cat(paste0(txt$st, p[[2]]$children[[1]], txt$nd),
      file = file.path(panel_path,
        paste0(key, ifelse(jsonp, ".jsonp", ".json"))))
  } else {
    message("panel not written - must be trellis, ggplot, or htmlwidget object")
  }
}

#' Cast a data frame as a cognostics data frame
#'
#' @param x a data frame
#' @param cond_cols the column name(s) that comprise the conditioning variables
#' @param key_col the column name that indicates the panel key
#' @export
as_cognostics <- function(x, cond_cols, key_col = NULL) {
  # make each column a true cognostic so things are consistent downstream

  if (is.null(key_col))
    key_col <- "panelKey"
  if (! key_col %in% names(x))
    stop("The data frame must either have a column name 'panelKey'",
      " or the column containing the key must be specified by key_col.",
      call. = FALSE)
  x$panelKey <- cog(x[[key_col]], desc = "panel key", type = "key",
    group = "panelKey", default_active = TRUE, filterable = FALSE)

  if (! all(cond_cols %in% names(x)))
    stop("The data frame must have all specified cond_cols: ",
      paste(cond_cols, collapse = ", "))

  for (cl in cond_cols) {
    x[[cl]] <- cog(x[[cl]], desc = "conditioning variable",
      type = ifelse(is.numeric(x[[cl]]), "numeric", "factor"),
      group = "condVar", default_label = TRUE)
  }

  # TODO: make sure cond_cols are unique and key_col is unique

  # any variables that aren't cogs, fill them in...
  has_no_cog <- which(!sapply(x, function(x) inherits(x, "cog")))
  if (length(has_no_cog) > 0)
    for (idx in has_no_cog)
      x[[idx]] <- cog(x[[idx]])

  # get rid of cogs that are all NA
  na_cogs <- which(sapply(x, function(a) all(is.na(a))))
  if (length(na_cogs) > 0)
    x[na_cogs] <- NULL

  class(x) <- c(class(x), "cognostics")
  x
}

#' Write cognostics data for a display in a Trelliscope app
#'
#' @param cogdf a data frame of cognostics, prepared with \code{\link{as_cognostics}}
#' @param base_path the base directory of the trelliscope application
#' @param name name of the display
#' @param group group that the display belongs to
#' @param jsonp should json for cognostics be jsonp (TRUE) or json (FALSE)?
#' @importFrom jsonlite toJSON
#' @export
write_cognostics <- function(cogdf, base_path, name, group = "common", jsonp = TRUE) {
  display_path <- file.path(base_path, "displays", group, name)
  txt <- get_jsonp_text(jsonp, "__loadCogData__")
  cat(paste0(txt$st, jsonlite::toJSON(cogdf), txt$nd),
    file = file.path(display_path,
      paste0("cogData.", ifelse(jsonp, "jsonp", "json"))))
}

#' Write a "display object" file for a Trelliscope app
#'
#' @param cogdf a data frame of cognostics, prepared with \code{\link{as_cognostics}}
#' @param panel_example an example object of one panel of a display (can be trellis, ggplot2, or htmlwidget object)
#' @param base_path the base directory of the trelliscope application
#' @param name name of the display
#' @param group group that the display belongs to
#' @param desc description of the display
#' @param height height in pixels of each panel
#' @param width width in pixels of each panel
#' @param md_desc optional string of markdown that will be shown in the viewer for additional context about the display
#' @param state the initial state the display will open in
#' @param jsonp should json for display object be jsonp (TRUE) or json (FALSE)?
#' @importFrom digest digest
#' @export
write_display_obj <- function(cogdf, panel_example, base_path, name, group = "common",
  desc = "", height = 500, width = 500, md_desc = "", state = NULL, jsonp = TRUE) {

  display_path <- file.path(base_path, "displays", group, name)
  panel_path <- file.path(display_path, ifelse(jsonp, "jsonp", "json"))

  if (! inherits(cogdf, "cognostics"))
    stop("cogdf must be a cognostics object - ",
      "call as_cognostics() to cast it as such.", call. = FALSE)

  if (inherits(panel_example, "htmlwidget")) {
    if (!is.null(panel_example$height))
      height <- panel_example$height
    if (!is.null(panel_example$width))
      width <- panel_example$width
  }

  message("building display object...")
  dispobj <- list(
    name = name,
    group = group,
    desc = desc,
    mdDesc = md_desc,
    updated = Sys.time(),
    n = length(list.files(panel_path)),
    height = height,
    width = width,
    keySig = digest::digest(sort(cogdf$panelKey)),
    cogInterface = list(name = name, group = group, type = "JSON"),
    panelInterface = list(
      type = ifelse(inherits(panel_example, "htmlwidget"), "htmlwidget", "image"),
      deps = get_and_write_widget_deps(panel_example, base_path)
    ),
    cogInfo = get_cog_info(cogdf),
    cogDistns = get_cog_distributions(cogdf)
  )

  # TODO: add state validation
  if (is.null(state))
    state <- list()

  if (is.null(state$layout))
    state$layout <- list(nrow = 1, ncol = 1, arrange = "row")
  if (is.null(state$labels)) {
    def_labels <- which(sapply(dispobj$cogInfo, function(x) x$defLabel))
    if (length(def_labels) > 0)
      state$labels <- I(names(dispobj$cogInfo)[def_labels])
  }
  if (is.null(state$sort)) {
    def_labels <- which(sapply(dispobj$cogInfo, function(x) x$defLabel))
    if (length(def_labels) > 0) {
      nms <- names(dispobj$cogInfo)[def_labels]
      state$sort <- lapply(seq_along(nms), function(ii)
        list(order = ii, name = nms[ii], dir = "asc"))
    }
  }
  dispobj$state <- state

  txt <- get_jsonp_text(jsonp, "__loadDisplayObj__")
  cat(paste0(txt$st,
    jsonlite::toJSON(dispobj, auto_unbox = TRUE), txt$nd),
    file = file.path(display_path,
      paste0("displayObj.", ifelse(jsonp, "jsonp", "json"))))

  message("writing cognostics...")
  write_cognostics(cogdf, base_path, name = name, group = group, jsonp = jsonp)

  message("writing thumbnail...")
  write_thumb(panel_example,
    file.path(display_path, "thumb.png"), width = width, height = height)

}

#' Set up all auxilliary files needed for a Trelliscope app
#'
#' @param base_path the base directory of the trelliscope application
#' @param jsonp should json for display list and app config be jsonp (TRUE) or json (FALSE)?
#' @param copy_viewer_files should the viewer files be copied (using \code{\link{copy_viewer_files}})
#' @export
prepare_display <- function(base_path, jsonp = TRUE, copy_viewer_files = TRUE) {
  message("writing display list")
  update_display_list(base_path, jsonp = jsonp)
  message("writing app config...")
  write_config(base_path, jsonp = jsonp)
  if (copy_viewer_files) {
    message("copying viewer files...")
    copy_viewer_files(base_path)
  }
}

#' Update Trelliscope app display list file
#'
#' @param base_path the base directory of the trelliscope application
#' @param jsonp should json for display list be jsonp (TRUE) or json (FALSE)?
#' @importFrom jsonlite fromJSON
#' @export
update_display_list <- function(base_path, jsonp = TRUE) {
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
      obj <- jsonlite::fromJSON(gsub("^__loadDisplayObj__\\((.*)\\)", "\\1", tmp))
    }
    list(
      group = obj$group,
      name = obj$name,
      desc = obj$desc,
      n = obj$n,
      height = obj$height,
      width = obj$width,
      updated = obj$updated,
      keySig = obj$keySig
    )
  })

  txt <- get_jsonp_text(jsonp, "__loadDisplayList__")
  cat(paste0(txt$st,
      jsonlite::toJSON(display_list, pretty = TRUE, auto_unbox = TRUE), txt$nd),
      file = file.path(base_path, "displays",
        paste0("displayList.", ifelse(jsonp, "jsonp", "json"))))
}

#' Write Trelliscope app configuration file
#'
#' @param base_path the base directory of the trelliscope application
#' @param jsonp should json for app config be jsonp (TRUE) or json (FALSE)?
#' @export
write_config <- function(base_path, jsonp = TRUE) {
  cfg <- as.character(jsonlite::toJSON(
    list(
      display_base = "displays",
      data_type = ifelse(jsonp, "jsonp", "json"),
      cog_server = list(
        type = ifelse(jsonp, "jsonp", "json"),
        info = list(base = "displays")
      )
    ),
    pretty = TRUE,
    auto_unbox = TRUE
  ))

  txt <- get_jsonp_text(jsonp, "__loadTrscopeConfig__")
  cat(paste0(txt$st, cfg, txt$nd),
    file = file.path(base_path,
      paste0("config", ifelse(jsonp, ".jsonp", ".json"))))
}

#' Copy Trelliscope viewer files to local app directory
#'
#' @param base_path the base directory of the trelliscope application
#' @param src the location of the Trelliscope viewer files
#' @importFrom curl curl_download
#' @export
copy_viewer_files <- function(
  base_path,
  src = "https://raw.githubusercontent.com/hafen/trelliscopejs-demo/gh-pages/") {

  dir.create(file.path(base_path, "static", "fonts", "IcoMoon", "fonts"),
    recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(base_path, "static/fonts/OpenSans"),
    recursive = TRUE, showWarnings = FALSE)

  to_copy <- c(
    "bundle.js",
    "bundle.js.map",
    "index.html",
    "favicon.ico",
    paste("static", "fonts", c(
      paste("IcoMoon", c(
        "style.css",
        paste("fonts", c(
          "icomoon.eot",
          "icomoon.svg",
          "icomoon.ttf",
          "icomoon.woff"
        ), sep = "/")
      ), sep = "/"),
      paste("OpenSans", c(
        "opensans-light-webfont.woff",
        "opensans-light-webfont.woff2",
        "opensans-regular-webfont.woff",
        "opensans-regular-webfont.woff2",
        "stylesheet.css"
      ), sep = "/")
    ), sep = "/")
  )

  for (ff in to_copy) {
    curl::curl_download(
      paste0(src, ff),
      file.path(base_path, ff))
  }
}

#' View a Trelliscope Display
#'
#' @param base_path the base directory of the trelliscope application
#' @export
#' @importFrom utils browseURL
view_display <- function(base_path) {
  utils::browseURL(normalizePath(file.path(base_path, "index.html")))
}

# display_analyze <- function(base_path) {
#   # make sure everything is in place and formatted properly for viewing
#   # for example, make sure keys in cognostics match panel file names, etc.
# }
