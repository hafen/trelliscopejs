#' Panel Wrapper Function
#' Wrapper function to specify a plot object for a panel for use in dplyr summarise()
#'
#' @param x a plot object
#' @examples
#' \donttest{
#' library(rbokeh)
#' library(dplyr)
#' ggplot2::mpg %>%
#'   group_by(manufacturer, class) %>%
#'   summarise(
#'     panel = panel(
#'       figure(xlab = "City mpg", ylab = "Highway mpg") %>%
#'         ly_points(cty, hwy)))
#' }
#' @export
panel <- function(x) {
  structure(list(x), class = "trelliscope_panels")
}

#' Cast a vector of URLs pointing to images as an image panel source
#'
#' @param x a vector of URLs pointing to images
#' @export
img_panel <- function(x) {
  cog(x, desc = "panel image source URL", type = "panelSrc",
    filterable = FALSE, sortable = FALSE)
}

#' Cast a vector of URLs pointing to local images as an image panel source
#'
#' @param x a vector of URLs pointing to images
#' @export
#' @note \code{x} must be paths relative to the \code{path} argument passed to \code{\link{trelliscope}}.
#' @examples
#' \dontrun{
#' # assuming images are available locally in relative path pokemon_local/images
#' pokemon$img <- img_panel_local(paste0("images/", basename(pokemon$url_image)))
#' trelliscope(pokemon, name = "pokemon", path = "pokemon_local")
#' }
img_panel_local <- function(x) {
  cog(x, desc = "panel image source URL", type = "panelSrcLocal",
    filterable = FALSE, sortable = FALSE)
}


#' Set labels for a data frame
#'
#' @param dat a data frame to apply labels to
#' @param label_list a named list with names matching those of \code{dat} and values being labels
#'
#' @return data frame with labels attached as attributes (attached to each column and named "label")
#' @export
set_labels <- function(dat, label_list) {
  nms <- intersect(names(dat), names(label_list))
  for (nm in nms)
    attr(dat[[nm]], "label") <- label_list[[nm]]
  dat
}

is_in_knitr <- function() {
  getOption("knitr.in.progress", FALSE)
}

is_in_shiny <- function() {
  res <- FALSE
  tmp <- try(utils::getFromNamespace(".globals", "shiny")$running, silent = TRUE)
  if (!inherits(tmp, "try-error"))
    res <- tmp
  res
}

resolve_app_params <- function(path, self_contained, jsonp, split_sig, name, 
  group, state, nrow = 1, ncol = 1, thumb = TRUE, split_layout = FALSE,
  id = NULL) {

  spa <- TRUE # "single-page application"

  if (is.null(thumb))
    thumb <- TRUE

  in_notebook <- FALSE
  if (in_rmarkdown_notebook()) {
    # spa <- FALSE # (don't need to set spa to FALSE because it's in an iframe)
    if (!self_contained) {
      message("** note: When inside an R Markdown document, the only way to embed a ",
        "Trelliscope display within the notebook is to use self_contained = TRUE.")
      # message("** note: Currently trelliscope displays can only be created from ",
      #   "within an RMarkdown notebook with self_contained = TRUE. Setting this ",
      #   "option and ignoring specified path (if any).")
      # path <- tempfile("trelliscope")
      # self_contained <- TRUE
    }
    in_notebook <- TRUE
  }

  in_knitr <- is_in_knitr()
  in_shiny <- is_in_shiny()

  if (in_knitr || in_shiny)
    spa <- FALSE # results are inline with others

  if (shiny_running())
    spa <- FALSE

  orig_path <- path

  if (is.null(path)) {
    if (in_knitr || in_shiny) {
      www_dir <- getwd()
    } else {
      www_dir <- tempfile("trelliscope")
    }
  } else {
    www_dir <- normalizePath(path, mustWork = FALSE)
  }
  path <- file.path(www_dir, "appfiles")
  if (!dir.exists(path))
    dir.create(path, recursive = TRUE)
  path <- normalizePath(path)

  # if outside knitr, config.jsonp will always be available to index.html inside appfiles
  config_path <- paste0("appfiles/config.json", ifelse(jsonp, "p", ""))
  if (in_knitr || in_shiny && !self_contained) {
    if (!grepl("^[A-Za-z0-9_]", orig_path))
      stop_nice("Path for trelliscope output while inside knitr or Shiny must be relative.")
    if (in_shiny) {
      if (!grepl("^www/", orig_path))
        stop_nice("Path for trelliscope output while inside Shiny must go inside www/...")
      config_path <- paste(gsub("^www/", "", orig_path), config_path, sep = "/")
    } else {
      config_path <- paste(orig_path, config_path, sep = "/")
    }
  }

  if (self_contained) {
    jsonp <- FALSE
    # no point in thumbnail if self contained
    # (because there won't be a list of displays to select from)
    thumb <- FALSE
  }

  if (is.null(state))
    state <- list()
  check_n_val <- function(val, val_name) {
    stop_val <- function() {
      stop_nice(paste0(
        "Parameter: '", val_name, "' should be a single, positive numeric integer. ",
        "Received: ", val
      ))
    }
    if (length(val) != 1) stop_val()
    if (!is.numeric(val)) stop_val()
    if (!isTRUE(all.equal(val, floor(val)))) stop_val()
    if (val <= 0) stop_val()
    val
  }
  state$layout <- list(
    nrow = check_n_val(nrow, "nrow"),
    ncol = check_n_val(ncol, "ncol"),
    arrange = "row"
  )

  # TODO: check sort state

  # TODO: check filter state

  # make sure split_layout is a boolean
  split_layout <- isTRUE(split_layout)

  if (is.null(id))
    id <- get_id(path)

  list(
    path = path,
    www_dir = www_dir,
    config_path = config_path,
    jsonp = jsonp,
    split_sig = split_sig,
    self_contained = self_contained,
    name = sanitize(name),
    group = sanitize(group),
    id = id,
    spa = spa,
    state = state,
    in_knitr = in_knitr,
    in_shiny = in_shiny,
    in_notebook = in_notebook,
    thumb = thumb,
    split_layout = split_layout
  )
}

# hacky way to check if we are in RMarkdown notebook
in_rmarkdown_notebook <- function() {
  print_fn <- try(get("print.htmlwidget"), silent = TRUE)
  !inherits(print_fn, "try-error")
}


#' @importFrom digest digest
get_id <- function(path) {
  ff <- file.path(path, "id")
  if (!file.exists(ff))
    return(digest::digest(Sys.time(), algo = "crc32"))
  readLines(ff, warn = FALSE)
}

sanitize <- function(x) {
  gsub("[^a-zA-Z0-9_]", "_", x)
}

## low-level helpers
##---------------------------------------------------------

get_jsonp_text <- function(jsonp, fn_name) {
  if (jsonp) {
    list(
      st = paste0(fn_name, "("),
      nd = ")"
    )
  } else {
    return(list(st = "", nd = ""))
  }
}

get_cog_info <- function(x) {
  if (! inherits(x, "cognostics"))
    stop_nice("Cognostics data frame must be a cognostics object - ",
      "call as_cognostics() to cast it as such.")

  nms <- names(x)
  cog_info <- do.call(rbind, lapply(seq_along(x), function(i) {
    tmp <- attr(x[[i]], "cog_attrs")
    data.frame(name = nms[i], tmp, stringsAsFactors = FALSE)
  }))

  tmp <- lapply(seq_len(nrow(cog_info)), function(i) {
    res <- as.list(cog_info[i, ])
    if (res$type == "factor" && length(unique(x[[res$name]])) <= 5000) {
      res$levels <- unique(x[[res$name]])
      res$levels[is.na(res$levels)] <- "NA"
    }
    # can add range for numeric, etc.
    if (res$type %in% c("numeric", "integer")) {
      res$range <- range(x[[res$name]], na.rm = TRUE)
      res$nnna <- length(which(!is.na(x[[res$name]])))
      res$breaks <- pretty(res$range, log2(res$nnna) + 1)
      res$delta <- diff(res$breaks[1:2])
      res$type <- "numeric"
    }
    # don't want to enable filter on singleton cogs
    if (length(unique(x[[res$name]])) == 1) {
      res$filterable <- FALSE
    }
    res
  })
  names(tmp) <- sapply(tmp, function(x) x$name)
  tmp
}

#' @importFrom graphics hist
#' @importFrom DistributionUtils skewness
#' @importFrom grDevices nclass.Sturges
get_cog_distributions <- function(cogdf, cat_cutoff = 5000) {
  cog_distns <- lapply(cogdf, function(x) {

    type <- attr(x, "cog_attrs")$type
    if (inherits(x, c("Date", "POSIXct"))) {
      type <- "factor"
      x <- as.character(x)
    }

    res <- list(
      type = type,
      dist = NULL
    )

    if (type == "factor") {
      res$has_dist <- FALSE
      if (length(unique(x)) <= cat_cutoff) {
        x[is.na(x)] <- "NA"
        tmp <- table(x)
        tmp <- tmp[order(tmp, decreasing = TRUE)]
        res$dist <- as.list(tmp)
        res$has_dist <- TRUE
        res$max <- as.numeric(max(tmp))
      }
    } else if (type == "numeric") {
      # n0 <- length(which(x == 0))
      # log <- FALSE
      skw <- DistributionUtils::skewness(x, na.rm = TRUE)

      hst <- graphics::hist(x, plot = FALSE)
      res <- list(
        type = type,
        dist = list(
          raw = list(
            breaks = hst$breaks,
            freq = hst$counts
          )
        )
      )
      res$log_default <- FALSE
      if (!is.na(skw) && skw > 1.5 && all(x >= 0, na.rm = TRUE) && length(x[x > 0]) > 1) {
        # log <- TRUE
        x <- x[x > 0]
        x2 <- log10(x)
        rng <- range(x2, na.rm = TRUE)
        brks <- 10 ^ seq(rng[1], rng[2], length = grDevices::nclass.Sturges(x))
        brks[1] <- min(min(x, na.rm = TRUE), brks[1])
        brks[length(brks)] <- max(max(x, na.rm = TRUE), brks[length(brks)])
        lhst <- hist(x, breaks = brks, plot = FALSE)
        res$dist$log <- list(
          breaks = lhst$breaks,
          freq = lhst$counts
        )
        res$log_default <- TRUE
      }
    }
    res
  })

  names(cog_distns) <- names(cogdf)
  cog_distns
}

get_dependencies <- function(widget_object) {
  if (inherits(widget_object, "htmlwidget")) {
    pt <- htmltools::as.tags(widget_object)
    deps <- htmltools::htmlDependencies(pt)
  } else {
    deps <- NULL
  }
  deps
}

#' @import htmltools
get_and_write_widget_deps <- function(widget_object, base_path, self_contained) {
  if (!inherits(widget_object, "htmlwidget")) {
    # message("object is not an htmlwidget so there are no widgets to write")
    return(invisible(NULL))
  }

  if (self_contained) {
    return(list(name = class(widget_object)[1]))
  } else {
    deps <- get_dependencies(widget_object)

    libdir <- file.path(base_path, "lib")
    dir.create(base_path, recursive = TRUE, showWarnings = FALSE)
    oldwd <- getwd()
    on.exit(setwd(oldwd), add = TRUE)
    setwd(base_path)
    dir.create(libdir, recursive = TRUE, showWarnings = FALSE)

    deps2 <- do.call(c, lapply(deps, function(x) {
      x <- htmltools::copyDependencyToDir(x, libdir, FALSE)
      x <- htmltools::makeDependencyRelative(x, base_path, FALSE)
      res <- list()
      if (!is.null(x$script)) {
        res[[length(res) + 1]] <- list(type = "script",
          url = paste(x$src, x$script, sep = "/"))
      }
      if (!is.null(x$stylesheet)) {
        res[[length(res) + 1]] <- list(type = "stylesheet",
          url = paste(x$src, x$stylesheet, sep = "/"))
      }
      res
    }))

    return(list(name = class(widget_object)[1], assets = deps2))
  }
}

#' @importFrom base64enc base64encode
encode_png <- function(file) {
  paste("data:image/png;base64,",
    base64enc::base64encode(
      readBin(file, "raw", n = file.info(file)$size)),
    sep = "")
}

shiny_running <- function() {
  frames <- sys.frames()
  calls <- lapply(sys.calls(), `[[`, 1)
  call_name <- function(call)
    if (is.function(call)) "<closure>" else deparse(call)[1]
  call_names <- vapply(calls, call_name, character(1))
  target_call <- grep("^runApp$", call_names)
  if (length(target_call) == 0)
    return(FALSE)
  target_frame <- frames[[target_call]]
  namespace_frame <- parent.env(target_frame)
  isNamespace(namespace_frame) && environmentName(namespace_frame) == "shiny"
}


# # useful if you want to use dplyr group_by to get to list of panels
# to_list <- function(x) {
#   (x %>%
#     nest() %>%
#     select(data) %>%
#     lapply(identity))$data
# }
