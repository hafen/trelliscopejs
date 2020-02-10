#
# get_label <- function(data, cols) {
#   sapply(cols, function(col) {
#     lbl <- attr(data[[col]], "label")
#     if (is.null(lbl))
#       lbl <- col
#     lbl
#   })
# }

stop_nice <- function(...) {
  stop(paste(strwrap(paste(...), exdent = 7), collapse = "\n"), call. = FALSE)
}


# #' Compute automatic cognostics
# #'
# #' @param data a list of data frames (one per subset), a grouped data frame, or a nested data frame
# #' @return If the input is a list of data frames, the return value is a list of data frames containing the cognostics. If the input is a grouped or nested df, the result will be a nested df with a new column containing the cognostics.
# #' @importFrom purrr map map_df
# #' @export
# #' @seealso \code{\link{trelliscope}}
# auto_cogs <- function(data) {
#
#   # if a grouped df, nest it so we have a nested df
#   if (inherits(data, "grouped_df")) {
#     # nesting causes label attributes to be lost, so preserve them...
#     # (need to find a better way to deal with this)
#     labels <- lapply(data, function(x) attr(x, "label"))
#
#     data <- nest(data)
#
#     # set first subset label attributes (auto_cogs will look for them here)
#     for (nm in names(data$data[[1]]))
#       attr(data$data[[1]][[nm]], "label") <- labels[[nm]]
#   }
#
#   # in the case of nested df, there should be atomic columns indicating splitting variables
#   # and then a single "list" column of data frames
#   data_is_df <- FALSE
#   if (is.data.frame(data)) {
#     data_is_df <- TRUE
#
#     is_atomic <- sapply(data, is.atomic)
#     # at_least_one_atomic <- length(which(is_atomic)) > 0
#     exactly_one_non_atomic <- length(which(!is_atomic)) == 1
#     if (!exactly_one_non_atomic)
#       stop_nice("Data supplied to auto_cogs must be a data frame with a single",
#         "nested data frame column.")
#
#     nest_nm <- names(data)[which(!is_atomic)]
#
#     if (! inherits(data[[nest_nm]][[1]], "data.frame"))
#       stop_nice("Data in nested column supplied to auto_cogs must contain data frames.")
#
#     cog_data <- data[[nest_nm]]
#   } else {
#     cog_data <- data
#   }
#
#   # cog_spec is a list specifying the cognostics and their descriptions
#   # so that we can add these in later
#
#   ## determine which columns to compute what kind of cognostics for
#   cog_spec <- list(
#     count = tibble(col = NA, cogname = "count", desc = "number of observations")
#   )
#
#   # if any columns are unique per group, add them as an "identity" cognostic
#   tmp <- cog_data %>% purrr::map_df(. %>% summarise_all(n_distinct))
#   unique_cols <- names(tmp)[sapply(tmp, function(x) all(x == 1))]
#   if (length(unique_cols) > 0) {
#     cog_spec$unique <- tibble(
#       col = unique_cols,
#       cogname = sanitize(unique_cols),
#       desc = get_label(cog_data[[1]], unique_cols))
#   }
#
#   # if numeric and not unique, get the mean (TODO - other summary stats and group them)
#   num_cols <- names(cog_data[[1]])[sapply(cog_data[[1]], is.numeric)]
#   num_cols <- setdiff(num_cols, unique_cols)
#   if (length(num_cols) > 0)
#     cog_spec$num <- tibble(
#       col = num_cols,
#       cogname = paste0(sanitize(num_cols), "_mean"),
#       desc = paste("mean", get_label(cog_data[[1]], num_cols)))
#
#   tmp <- bind_rows(cog_spec)
#   cog_desc <- as.list(tmp$desc)
#   names(cog_desc) <- tmp$cogname
#
#   res <- map_cog(cog_data, function(x) {
#     res <- tibble(count = nrow(x))
#     for (ii in seq_along(cog_spec$unique$col))
#       res[[cog_spec$unique$cogname[ii]]] <- x[[cog_spec$unique$col[ii]]][1]
#     for (ii in seq_along(cog_spec$num$col))
#       res[[cog_spec$num$cogname[ii]]] <- mean(x[[cog_spec$num$col[ii]]])
#     res
#   })
#
#   if (data_is_df) {
#     return(
#       data %>%
#         mutate(auto_cogs = res)
#     )
#   } else {
#     return(res)
#   }
# }
