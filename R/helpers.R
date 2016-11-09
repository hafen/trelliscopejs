
#' @importFrom digest digest
get_id <- function(path) {
  ff <- file.path(path, "id")
  if (!file.exists(ff))
    return(digest::digest(Sys.time(), algo = "crc32"))
  readLines(ff, warn = FALSE)
}

sanitize <- function(x) {
  gsub("[^a-zA-Z0-9_/\\.]", "_", x)
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
    stop("Cognostics data frame must be a cognostics object - ",
      "call as_cognostics() to cast it as such.", call. = FALSE)

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
      n0 <- length(which(x == 0))
      skw <- DistributionUtils::skewness(x, na.rm = TRUE)
      log <- FALSE

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
      if (!is.nan(skw) && skw > 1.5 && all(x >= 0, na.rm = TRUE)) {
        log <- TRUE
        x <- x[x > 0]
        x2 <- log10(x)
        rng <- range(x2, na.rm = TRUE)
        brks <- 10 ^ seq(rng[1], rng[2], length = grDevices::nclass.Sturges(x))
        lhst <- hist(x, breaks = brks, plot = FALSE)
        res$dist$log <- list(
          breaks = hst$breaks,
          freq = hst$counts
        )
        res$log_default <- TRUE
      }
    }
    res
  })

  names(cog_distns) <- names(cogdf)
  cog_distns
}

#' @import htmltools
get_and_write_widget_deps <- function(widget_object, base_path) {
  if (!inherits(widget_object, "htmlwidget")) {
    message("object is not an htmlwidget so there are no widgets to write")
    return(invisible(NULL))
  }

  pt <- htmltools::as.tags(widget_object)
  deps <- htmltools::htmlDependencies(pt)

  libdir <- file.path(base_path, "lib")
  dir.create(base_path, showWarnings = FALSE)
  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(base_path)
  dir.create(libdir, showWarnings = FALSE)

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

  list(name = class(widget_object)[1], assets = deps2)
}

#' @importFrom base64enc base64encode
encode_png <- function(file) {
  paste("data:image/png;base64,",
    base64enc::base64encode(
      readBin(file, "raw", n = file.info(file)$size)),
    sep = "")
}

# # useful if you want to use dplyr group_by to get to list of panels
# to_list <- function(x) {
#   (x %>%
#     nest() %>%
#     select(data) %>%
#     lapply(identity))$data
# }
