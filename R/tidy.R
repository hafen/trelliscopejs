
#' Panel Wrapper Function
#' Wrapper function to specify a plot object for a panel for use in dplyr summarise()
#'
#' @param x a plot object
#' @examples
#' \dontrun{
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


#' Panels Wrapper Function
#' Wrapper function to specify a plot command to be applied to a list-column for use tidy group/nest/mutate/map-like situations
#'
#' @param .x a list or atomic vector (see \code{\link[purrr]{map}} for details)
#' @param .f a function, formula, or atomic vector (see \code{\link[purrr]{map}} for details)
#' @param ... additional arguments passed on to .f (see \code{\link[purrr]{map}} for details)
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(tidyr)
#' ggplot2::mpg %>%
#'   group_by(manufacturer, class) %>%
#'   nest() %>%
#'   mutate(panel = panels(data,
#'     ~ figure(xlab = "City mpg", ylab = "Highway mpg") %>%
#'         ly_points(cty, hwy, data = .x))) %>%
#'   trelliscope(name = "city_vs_highway_mpg")
#' }
#' @export
panels <- function(.x, .f, ...) {
  structure(
    purrr::map(.x, .f, ...),
    class = c("trelliscope_panels", "list")
  )
}

#' Panels Wrapper Function
#' Wrapper function to specify a plot command to be applied to a list-column for use tidy group/nest/mutate/map-like situations
#'
#' @param .d A data frame.
#' @param ..f A function to apply to each row. It should return a valid panel object (such as a ggplot2 / lattice / htmlwidget object).
#' @param ... Additional arguments passed on to ..f.
#' @examples
#' \dontrun{
#' }
#' @export
panels_by_row <- function(.d, ..f, .to = "panel") {
  res <- purrr::by_row(.d = .d, ..f = ..f, .to = .to)
  class(res[[.to]]) <- c("trelliscope_panels", "list")
  res
}

#' Cogs Wrapper Function
#' Wrapper function to specify a cognostics function to be applied to a list-column for use tidy group/nest/mutate/map-like situations
#'
#' @param .x a list or atomic vector (see \code{\link[purrr]{map}} for details)
#' @param .f a function, formula, or atomic vector (see \code{\link[purrr]{map}} for details)
#' @param ... additional arguments passed on to .f (see \code{\link[purrr]{map}} for details)
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(tidyr)
#' ggplot2::mpg %>%
#'   group_by(manufacturer, class) %>%
#'   nest() %>%
#'   mutate(
#'     additional_cogs = cogs(data,
#'       ~ data_frame(
#'         max_city_mpg = cog(max(.x$cty), desc = "Max city mpg"),
#'         min_city_mpg = cog(min(.x$cty), desc = "Min city mpg"))),
#'     panel = panels(data, ~ figure(xlab = "City mpg", ylab = "Highway mpg") %>%
#'       ly_points(cty, hwy, data = .x))) %>%
#'   trelliscope(name = "city_vs_highway_mpg", nrow = 1, ncol = 2)
#' }
cogs <- function(.x, .f, ...) {
  structure(
    purrr::map(.x, .f, ...),
    class = c("trelliscope_cogs", "list")
  )
}
