#' Cast Column as a Cognostic
#'
#' Cast a column of a cognostics data frame as a cognostic object
#'
#' @param val a scalar value (numeric, characer, date, etc.)
#' @param desc a description for this cognostic value
#' @param group optional categorization of the cognostic for organizational purposes in the viewer (currently not implemented in the viewer)
#' @param type the desired type of cognostic you would like to compute (see details)
#' @param default_label should this cognostic be used as a panel label in the viewer by default?
#' @param default_active should this cognostic be active (available for sort / filter / sample) by default?
#' @param filterable should this cognostic be filterable?  Default is \code{TRUE}.  It can be useful to set this to \code{FALSE} if the cognostic is categorical with many unique values and is only desired to be used as a panel label.
#' @param sortable should this cognostic be sortable?
#' @param log when being used in the viewer for visual univariate and bivariate filters, should the log be computed?  Useful when the distribution of the cognostic is very long-tailed or has large outliers.  Can either be a logical or a positive integer indicating the base.
#'
#' @return object of class "cog"
#'
#' @details Different types of cognostics can be specified through the \code{type} argument that will affect how the user is able to interact with those cognostics in the viewer.  This can usually be ignored because it will be inferred from the implicit data type of \code{val}.  But there are special types of cognostics, such as geographic coordinates and relations (not implemented) that can be specified as well.  Current possibilities for \code{type} are "key", "integer", "numeric", "factor", "date", "time", "href".
#'
#' @export
#' @examples
#' \dontrun{
#' mpg %>%
#'   group_by(manufacturer, class) %>%
#'   summarise(
#'     mean_city_mpg = cog(mean(cty), desc = "Mean city mpg"),
#'     mean_hwy_mpg = cog(mean(hwy), desc = "Mean highway mpg"),
#'     most_common_drv = cog(tail(names(table(drv)), 1), desc = "Most common drive type"),
#'     panel = panel(
#'       figure(xlab = "City mpg", ylab = "Highway mpg",
#'         xlim = c(9, 47), ylim = c(7, 37)) %>%
#'         ly_points(cty, hwy,
#'           hover = data_frame(model = paste(year, model),
#'           cty = cty, hwy = hwy)))) %>%
#'   trelliscope(name = "city_vs_highway_mpg", nrow = 1, ncol = 2)
#' }
cog <- function(val = NULL, desc = "", group = "common",
  type = NULL, default_label = FALSE, default_active = TRUE,
  filterable = TRUE, sortable = TRUE, log = NULL) {

  cog_types <- list(
    key      = as.character,
    integer  = as.integer,
    numeric  = as.numeric,
    factor   = as.character,
    date     = as.Date,
    time     = as.POSIXct,
    panelSrc = as.character,
    # color    = as.character,
    # geo      = as.cogGeo,
    # rel      = as.cogRel,
    # hier     = as.cogHier,
    href     = as.character
  )

  types <- names(cog_types)

  if (!is.null(type)) {
    if (!type %in% types)
      stop_nice("Invalid cognostics type:", type)

    val <- try(cog_types[[type]](val))
    if (inherits(val, "try-error"))
      val <- NA
  } else {
    # TODO: if type is not specified, set type to NA and wait until final
    # call to as_cognostics() to infer the type (to make sure we have them all)

    # try to infer type
    if (is.factor(val))
      val <- as.character(val)
    type <- infer_cog_type(val)
    if (is.na(type))
      val <- NA
  }

  if (is.null(log))
    log <- NA

  if (is.logical(log)) {
    log <- ifelse(log, 10, NA)
  }
  if (is.numeric(log)) {
    if (log <= 0)
      log <- NA
  }

  cog_attrs <- list(
    desc = desc,
    type = type,
    group = group,
    defLabel = default_label,
    defActive = default_active,
    filterable = filterable,
    log = log
  )
  attr(val, "cog_attrs") <- cog_attrs

  class(val) <- c("cog", class(val))
  val
}

infer_cog_type <- function(val) {
  if (is.factor(val) || is.character(val)) {
    type <- "factor"
  } else if (is.numeric(val)) {
    type <- "numeric"
  } else if (inherits(val, "Date")) {
    type <- "date"
  } else if (inherits(val, "POSIXct")) {
    type <- "time"
  } else {
    type <- NA
  }
  type
}

#' Href Cognostic
#'
#' Create href to be used as cognostics in a trelliscope display
#'
#' @param x URL to link to
#' @param desc,group,default_label,default_active,filterable,sortable,log arguments passed to \code{\link{cog}}
#'
#' @seealso \code{\link{cog}}
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(rbokeh)
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(
#'     wiki_link = cog_href(paste0("https://en.wikipedia.org/wiki/Iris_",
#'       tolower(Species))[1], default_label = TRUE,
#'       desc = "link to species on wikipedia"),
#'     panel = panel(figure(xlab = "Sepal Length", ylab = "Sepal Width") %>%
#'       ly_points(Sepal.Length, Sepal.Width))) %>%
#'   trelliscope(name = "iris_species", ncol = 3)
#' }
#' @export
cog_href <- function(x, desc = "link", group = "common",
  default_label = FALSE, default_active = FALSE, filterable = FALSE,
  sortable = FALSE, log = FALSE) {

  cog(x, type = "href", desc = desc, group = group, default_label = default_label,
    default_active = default_active, filterable = filterable, sortable = sortable,
    log = log)
}

#' Cast a data frame as a cognostics data frame
#'
#' @param x a data frame
#' @param cond_cols the column name(s) that comprise the conditioning variables
#' @param key_col the column name that indicates the panel key
#' @param cog_desc an optional named list of descriptions for the cognostics columns
#' @param needs_key does the result need to have a "key" column?
#' @param needs_cond does the result need to have conditioning variable columns?
#' @export
as_cognostics <- function(x, cond_cols, key_col = NULL, cog_desc = NULL,
  needs_key = TRUE, needs_cond = TRUE, group = "common") {
  # make each column a true cognostic so things are consistent downstream

  if (needs_key) {
    if (is.null(key_col))
      key_col <- "panelKey"
    if (! key_col %in% names(x)) {
      x$panelKey <- cog(sanitize( # nolint
        apply(x[cond_cols], 1, paste, collapse = "_")),
        desc = "panel key", type = "key", group = "panelKey",
        default_active = TRUE, filterable = FALSE)
    }
  }

  if (needs_cond) {
    if (! all(cond_cols %in% names(x)))
      stop_nice("The cognostics data frame must have all specified cond_cols:",
        paste(cond_cols, collapse = ", "))

    for (cl in cond_cols) {
      x[[cl]] <- cog(x[[cl]], desc = "conditioning variable",
        type = ifelse(is.numeric(x[[cl]]), "numeric", "factor"),
        group = "condVar", default_label = TRUE)
    }
  }

  # TODO: make sure cond_cols are unique and key_col is unique

  # any variables that aren't cogs, fill them in...
  has_no_cog <- which(!sapply(x, function(x) inherits(x, "cog")))
  nms <- names(x)
  if (length(has_no_cog) > 0) {
    for (idx in has_no_cog) {
      desc <- cog_desc[[nms[idx]]]
      if (!is.character(desc))
        desc <- nms[idx]

      #  TODO keep until groups are handled in viewer
      if (!identical(group, "common")) {
        desc <- paste(group, ": ", desc, sep = "")
      }
      
      if (all(grepl("https*://", x[[idx]]))) {
        x[[idx]] <- cog_href(x[[idx]], desc = paste(desc, "(link)"), group = group)
      } else {
        x[[idx]] <- cog(x[[idx]], desc = desc, group = group)
      }
    }
  }

  # get rid of cogs that are all NA
  na_cogs <- which(sapply(x, function(a) all(is.na(a))))
  if (length(na_cogs) > 0) {
    message("Removing the following cognostics that are all NA: ",
      paste(nms[na_cogs], collapse = ", "))
    x[na_cogs] <- NULL
  }

  class(x) <- c(class(x), "cognostics")
  x
}
