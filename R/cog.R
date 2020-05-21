#' Cast Column as a Cognostic
#'
#' Cast a column of a cognostics data frame as a cognostic object
#'
#' @param val a scalar value (numeric, character, date, etc.)
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
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#' library(ggplot2)
#' library(rbokeh)
#' 
#' mpg_cog <- mpg %>%
#'   group_by(manufacturer, class) %>%
#'   nest() %>%
#'   mutate(
#'     cogs = map_cog(data, ~ tibble(
#'       mean_city_mpg = cog(mean(.$cty), desc = "Mean city mpg"),
#'       mean_hwy_mpg = cog(mean(.$hwy), desc = "Mean highway mpg"),
#'       most_common_drv = cog(tail(names(table(.$drv)), 1), desc = "Most common drive type")
#'     )),
#'     panel = map_plot(data, ~
#'       figure(., xlab = "City mpg", ylab = "Highway mpg",
#'         xlim = c(9, 47), ylim = c(7, 37)) %>%
#'         ly_points(cty, hwy,
#'           hover = list(year, model))
#'     )
#'   )
#' 
#' trelliscope(mpg_cog, name = "city_vs_highway_mpg", nrow = 1, ncol = 2)
cog <- function(val = NULL, desc = "", group = "common",
  type = NULL, default_label = FALSE, default_active = TRUE,
  filterable = TRUE, sortable = TRUE, log = NULL) {

  cog_types <- list(
    key           = as.character,
    integer       = as.integer,
    numeric       = as.numeric,
    factor        = as.character,
    date          = as.Date,
    time          = as.POSIXct,
    panelSrc      = as.character,
    panelSrcLocal = as.character,
    # color       = as.character,
    # geo         = as.cogGeo,
    # rel         = as.cogRel,
    # hier        = as.cogHier,
    href          = as.character
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
    if (all(grepl("^http://|^https://", val))) {
      type <- "href"
    } else {
      type <- "factor"
    }
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

#' Helper function for creating a cognostic for a link to another display in a filtered state
#' @param display A string indicating the name of the display to link to.
#' @param var A string indicating the variable name to filter on.
#' @param val A string indicating the value of the filter.
#' @param desc a description for this cognostic value
#' @param group optional categorization of the cognostic for organizational purposes in the viewer (currently not implemented in the viewer)
#' @param default_label should this cognostic be used as a panel label in the viewer by default?
#' @param default_active should this cognostic be active (available for sort / filter / sample) by default?
#' @param filterable should this cognostic be filterable?  Default is \code{TRUE}.  It can be useful to set this to \code{FALSE} if the cognostic is categorical with many unique values and is only desired to be used as a panel label.
#' @param sortable should this cognostic be sortable?
#' @export
cog_disp_filter <- function(display, var, val,
  desc = "link", group = "common",
  default_label = FALSE, default_active = FALSE,
  filterable = FALSE, sortable = FALSE) {
  x <- paste0("#display=", display, "&filter=var:",
    var, ";type:select;val:", val)

  cog(x, type = "href", desc = desc, group = group, 
    default_label = default_label,
    default_active = default_active,
    filterable = filterable, sortable = sortable,
    log = FALSE)
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
#' \donttest{
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
#' @param group value to be used in the \code{\link{cog}} group
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

  class(x) <- c("cognostics", class(x))
  x
}



bind_cog_list_and_descs <- function(cog_list) {
  # retrieve autocog description (or any other desc)
  non_null_pos <- ! unlist(lapply(cog_list, is.null))

  has_factor <- any(unlist(lapply(cog_list[[1]], is.factor)))
  if (!inherits(cog_list[[1]], "tibble") && has_factor)
    message(
      "Note: it is advised to use tibble() when creating cognostic columns, ",
      "to avoid issues that arise with data.frame and stringsAsFactors = TRUE.")

  res <- suppressWarnings(dplyr::bind_rows(cog_list))

  # retrieve the first non null cognostic descriptions
  #   from each nested cog data
  cog_desc <- list()
  if (sum(non_null_pos) > 0) {
    # get first non null attr
    non_null_row_dt <- cog_list[non_null_pos][[1]]

    # get attributes
    one_row_attrs <- lapply(non_null_row_dt, function(x) attr(x, "cog_attrs"))
    one_row_class <- lapply(non_null_row_dt, function(x) {
      res <- class(x)
      res[res == "factor"] <- "character"
      res
    })

    # extract description attrs
    cog_desc <- lapply(one_row_attrs, `[[`, "desc")

    # store attributes of each column of first non null info
    for (nm in names(res)) {
      attr(res[[nm]], "cog_attrs") <- one_row_attrs[[nm]]
      class(res[[nm]]) <- one_row_class[[nm]]
    }
  }

  list(
    cog_df = res,
    cog_desc = cog_desc
  )
}

#' @importFrom autocogs panel_cogs
cog_df_info <- function(x, panel_col, state, auto_cog = FALSE, nested_data_list = NULL,
  nested_cog_attrs = NULL) {

  atomic_cols <- names(x)[sapply(x, is.atomic)]
  non_atomic_cols <- setdiff(names(x), c(atomic_cols, panel_col))
  is_nested <- length(non_atomic_cols) > 0

  if (length(atomic_cols) == 0)
    stop_nice("There must be at least one atomic column in the data frame passed in",
      "to trelliscope.data.frame")

  cond_cols <- find_cond_cols(x[atomic_cols], is_nested)

  # if we are no longer sorted by a cond_col but are sorted by something else
  # and if sort state is not already specified, then set that as state
  if (is.unsorted(x[[cond_cols[1]]])) {
    sort_cols <- find_sort_cols(x[setdiff(atomic_cols, cond_cols)])

    if (nrow(sort_cols) > 0) {
      cond_not_sorted <- !sort_cols$name %in% cond_cols
      other_sorted <- setdiff(sort_cols$name, cond_cols)
      if (is.null(state$sort) && cond_not_sorted && length(other_sorted) > 0) {
        if (is.null(state))
          state <- list()
        state$sort <- lapply(other_sorted, function(a) {
          list(name = a, dir = sort_cols$dir[sort_cols$name == a])
        })
        if (is.null(state$labels)) {
          state$labels <- c(cond_cols, other_sorted)
        }
      }
    }
  }

  cogs <- list(as_cognostics(x[atomic_cols], cond_cols))

  if (!is.null(nested_data_list)) {
    # add unique data within nested data
    distinct_counts <- nested_data_list %>% purrr::map_df(. %>% summarise_all(n_distinct))
    unique_cols <- names(distinct_counts)[sapply(distinct_counts, function(x) all(x == 1))]
    if (length(unique_cols) > 0) {
      tmp <- nested_data_list %>%
        lapply(function(sub_dt) {
          sub_dt[1, unique_cols]
        }) %>%
        bind_rows()

      # add nested cog attrs back in, if specified
      for (nm in names(tmp)) {
        ca <- nested_cog_attrs[[nm]]
        if (!is.null(ca)) {
          attr(tmp[[nm]], "cog_attrs") <- ca
          class(tmp[[nm]]) <- c(class(tmp[[nm]]), "cog")
        }
      }

      cogs[[length(cogs) + 1]] <-  as_cognostics(
        tmp,
        needs_key = FALSE, needs_cond = FALSE,
        group = "_data",
        cog_desc = NULL
      )
    }

    # calculate non unique cognostics
    non_unique_cols <- setdiff(names(distinct_counts), c(unique_cols, ".id"))
    if (length(non_unique_cols) > 1) {

      # run a loop over all non_unique_cols
      for (i in seq_along(non_unique_cols)) {
        non_unique_col <- non_unique_cols[[i]]

        non_unique_cog_i <- lapply(nested_data_list, function(sub_dt) {
          column <- sub_dt[[non_unique_col]]
          if (is.character(column) || is.factor(column)) {
            autocogs::autocog_univariate_discrete(as.character(column))
          } else if (is.numeric(column)) {
            autocogs::autocog_univariate_continuous(column)
          } else {
            NULL
          }
        })

        tmp <- bind_cog_list_and_descs(non_unique_cog_i)
        non_unique_cog_df <- tmp$cog_df
        cog_desc <- tmp$cog_desc

        if (nrow(non_unique_cog_df) > 0) {
          # add the name to make it extra descriptive
          # TODO remove once visual grouping is done
          names(cog_desc) <- paste0(non_unique_col, "_", names(cog_desc))
          colnames(non_unique_cog_df) <- paste0(non_unique_col, "_", colnames(non_unique_cog_df))

          cogs[[length(cogs) + 1]] <- as_cognostics(
            non_unique_cog_df,
            needs_key = FALSE, needs_cond = FALSE,
            group = non_unique_col,
            cog_desc = cog_desc
          )
        }
      }
    }
  }

  if (length(non_atomic_cols) > 0) {
    usable <- non_atomic_cols[sapply(x[non_atomic_cols],
      function(a) is.data.frame(a[[1]]))]
    needs_auto <- usable[sapply(x[usable], function(a) {
      any(sapply(a, nrow) > 1)
    })]

    no_needs_auto <- setdiff(usable, needs_auto)
    for (a in no_needs_auto) {
      to_auto_list <- x[[a]]
      if (inherits(to_auto_list, "trelliscope_cogs")) {
        class(to_auto_list) <- "list"
      }

      tmp <- bind_cog_list_and_descs(to_auto_list)
      auto_df <- tmp$cog_df
      cog_desc <- tmp$cog_desc

      cogs[[length(cogs) + 1]] <- auto_df %>%
        as_cognostics(
          needs_key = FALSE, needs_cond = FALSE,
          group = a,
          cog_desc = cog_desc
      )
    }
  }

  # add automatic cognostics from autocogs
  if (!(identical(auto_cog, FALSE) || is.null(auto_cog))) {
    panel_cog_list <- panel_cogs(x, panel_col = panel_col, layers = auto_cog)
    for (nm in names(panel_cog_list)) {
      tmp <- bind_cog_list_and_descs(panel_cog_list[[nm]])
      panel_cog_dt <- tmp$cog_df
      cog_desc <- tmp$cog_desc
      names(cog_desc) <- paste0(nm, "_", names(cog_desc))
      colnames(panel_cog_dt) <- paste0(nm, "_", colnames(panel_cog_dt))
      cogs[[length(cogs) + 1]] <- as_cognostics(
          panel_cog_dt,
          needs_key = FALSE, needs_cond = FALSE,
          group = nm,
          cog_desc = cog_desc
      )

    }
  }

  cog_df <- bind_cols(cogs)

  list(
    cog_df = cog_df,
    cond_cols = cond_cols,
    atomic_cols = atomic_cols,
    non_atomic_cols = non_atomic_cols,
    state = state
  )
}
