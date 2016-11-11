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
    key     = as.character,
    integer = as.integer,
    numeric = as.numeric,
    factor  = as.character,
    date    = as.Date,
    time    = as.POSIXct,
    # geo     = as.cogGeo,
    # rel     = as.cogRel,
    # hier    = as.cogHier,
    href    = as.character
  )

  types <- names(cog_types)

  if (!is.null(type)) {
    if (!type %in% types)
      stop("Invalid cognostics type: ", type)

    val <- try(cog_types[[type]](val))
    if (inherits(val, "try-error"))
      val <- NA
  } else {
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
  x$panelKey <- cog(x[[key_col]], desc = "panel key", type = "key", # nolint
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
  if (length(has_no_cog) > 0) {
    for (idx in has_no_cog) {
      desc <- attr(x[[idx]], "label")
      if (!is.character(desc))
        desc <- ""
      x[[idx]] <- cog(x[[idx]], desc = desc)
    }
  }

  # get rid of cogs that are all NA
  na_cogs <- which(sapply(x, function(a) all(is.na(a))))
  if (length(na_cogs) > 0)
    x[na_cogs] <- NULL

  class(x) <- c(class(x), "cognostics")
  x
}
