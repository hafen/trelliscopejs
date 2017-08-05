#' Apply a function to each element of a vector and return a vector of plot "promises"
#'
#' @param .x a list or atomic vector (see \code{\link[purrr]{map}} for details)
#' @param .f a function, formula, or atomic vector that returns a plot object (see \code{\link[purrr]{map}} for details)
#' @param ... additional arguments passed on to .f (see \code{\link[purrr]{map}} for details)
#' @details See \code{\link[purrr]{map}}
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#' library(rbokeh)
#' library(gapminder)
#'
#' # nest gapminder data by country
#' by_country <- gapminder %>%
#'   group_by(country, continent) %>%
#'   nest()
#'
#' # add in a plot column with map_plot
#' by_country <- by_country %>% mutate(
#'   panel = map_pplot(data,
#'     ~ figure(xlim = c(1948, 2011), ylim = c(10, 95), width = 300, tools = NULL) %>%
#'         ly_points(year, lifeExp, data = .x, hover = .x)
#'   ))
#'
#' # look at one panel
#' by_country$panel[[1]]
#' get_ppanel(by_country, row = 1)
#'
#' # plot it
#' trelliscope(by_country, "gapminder", nrow = 2, ncol = 7)
#' }
#' @export
map_pplot <- function(.x, .f, ...) {
  f <- function(x)
    structure(x, class = "panel_promise")
  expr <- rlang::enquo(.x)
  structure(
    purrr::map(seq_along(.x), f, ...),
    class = c("trelliscope_ppanels", "list"),
    fn = .f,
    expr = expr,
    map_type = "map"
  )
}

#' Map over multiple inputs simultaneously and return a vector of plots
#'
#' @param .x,.y Vectors of the same length. A vector of length 1 will be recycled.
#' @param .f A function, formula, or atomic vector (see \code{\link[purrr]{map2}} for details)
#' @param ... additional arguments passed on to .f.
#' @param .l A list of lists. The length of .l determines the number of arguments that .f will be called with. List names will be used if present.
#' @details See \code{\link[purrr]{map2}}
#' @examples
#' \dontrun{
#' library(tidyr)
#' library(purrr)
#' library(rbokeh)
#' library(dplyr)
#'
#' p <- iris %>%
#'   nest(-Species) %>%
#'   mutate(
#'     mod = map(data, ~ lm(Sepal.Length ~ Sepal.Width, data = .x)),
#'     panel = map2_pplot(data, mod, function(data, mod) {
#'       figure(xlab = "Sepal.Width", ylab = "Sepal.Length") %>%
#'         ly_points(data$Sepal.Width, data$Sepal.Length) %>%
#'         ly_abline(mod)
#'     }))
#'
#' # look at one panel
#' p$panel[[1]]
#' get_ppanel(p, row = 2)
#'
#' # plot it
#' trelliscope(p, name = "iris")
#'
#' # same plot but using pmap_pplot()
#' p <- iris %>%
#'   nest(-Species) %>%
#'   mutate(
#'     mod = map(data, ~ lm(Sepal.Length ~ Sepal.Width, data = .x)),
#'     panel = pmap_pplot(list(data = data, mod = mod), function(data, mod) {
#'       figure(xlab = "Sepal.Width", ylab = "Sepal.Length") %>%
#'         ly_points(data$Sepal.Width, data$Sepal.Length) %>%
#'         ly_abline(mod)
#'     }))
#'
#' # look at one panel
#' p$panel[[1]]
#' get_ppanel(p, row = 2)
#'
#' # plot it
#' trelliscope(p, name = "iris")
#' }
#' @export
map2_pplot <- function(.x, .y, .f, ...) {
  f <- function(x)
    structure(x, class = "panel_promise")
  x_expr <- rlang::enquo(.x)
  y_expr <- rlang::enquo(.y)
  structure(
    purrr::map(seq_along(.x), f, ...),
    class = c("trelliscope_ppanels", "list"),
    fn = .f,
    x_expr = x_expr,
    y_expr = y_expr,
    map_type = "map2"
  )
}

#' @export
#' @rdname map2_plot
pmap_pplot <- function(.l, .f, ...) {
  f <- function(x)
    structure(x, class = "panel_promise")
  expr <- rlang::enquo(.l)
  structure(
    purrr::map(seq_along(.l[[1]]), f, ...),
    class = c("trelliscope_ppanels", "list"),
    fn = .f,
    expr = expr,
    map_type = "pmap"
  )
}

#' Apply a function to each row of a data frame and return a data frame with a new column of plots
#'
#' @param .d A data frame.
#' @param ..f A function to apply to each row. It should return a valid panel object (such as a ggplot2 / lattice / htmlwidget object).
#' @param .to Name of output column (defaults to "panel").
#' @details See \code{\link[purrrlyr]{by_row}}
#' @examples
#' \dontrun{
#' p <- iris %>%
#'   nest(-Species) %>%
#'   mutate(mod = map(data, ~ lm(Sepal.Length ~ Sepal.Width, data = .x))) %>%
#'   by_row_pplot(function(x) {
#'     figure(xlab = "Sepal.Width", ylab = "Sepal.Length") %>%
#'       ly_points(x$data[[1]]$Sepal.Width, x$data[[1]]$Sepal.Length) %>%
#'       ly_abline(x$mod[[1]])
#'   })
#'
#' # look at one panel
#' p$panel[[1]]
#' get_ppanel(p, row = 2)
#'
#' # plot it
#' trelliscope(p, name = "iris")
#' }
#' @export
by_row_pplot <- function(.d, ..f, .to = "panel") {
  f <- function(x)
    structure(x, class = "panel_promise")

  .d[[.to]] <- purrr::map(seq_len(nrow(.d)), f)

  class(.d[[.to]]) <- c("trelliscope_ppanels", "list")
  attr(.d[[.to]], "fn") <- ..f
  attr(.d[[.to]], "map_type") <- "by_row"

  .d
}

#' @export
print.panel_promise <- function(x, ...) {
  message(
    "This is a panel 'promise'. Panels will be computed on demand.\n",
    "To the panel for this row of your data, call:\n",
    paste0("get_ppanel(_data_, _panel_col_name_, ", as.character(x), ")"))
}

#' Get an evaluated panel promise for a specified row index
#'
#' @param dat a data frame with a column containing a panel promise
#' @param col_name the name of the column containing the panel promise
#' @param row the row index to use to find the panel to evaluate
#' @export
get_ppanel <- function(dat, col_name = "panel", row = 1) {
  quo_col_name <- enquo(col_name)
  cur_row <- dplyr::filter_at(dat,
    vars(one_of(col_name)), any_vars(as.integer(.) == row))
  if (nrow(cur_row) == 0)
    stop("Couldn't find a panel promise with row index '", row, "'.", call. = FALSE)

  map_type <- attr(dat[[col_name]], "map_type")
  if (is.null(map_type))
    stop("It doesn't look like ", col_name, " is a valid panel promise column.", call. = FALSE)

  .f <- attr(dat[[col_name]], "fn")
  if (map_type == "map") {
    .x <- rlang::eval_tidy(attr(dat[[col_name]], "expr"), cur_row)[[1]]
    return(rlang::as_function(.f)(.x))
  } else if (map_type == "map2") {
    .x <- rlang::eval_tidy(attr(dat[[col_name]], "x_expr"), cur_row)[[1]]
    .y <- rlang::eval_tidy(attr(dat[[col_name]], "y_expr"), cur_row)[[1]]
    return(rlang::as_function(.f)(.x, .y))
  } else if (map_type == "pmap") {
    .l <- rlang::eval_tidy(attr(dat[[col_name]], "expr"), cur_row)
    return(do.call(rlang::as_function(.f), unlist(.l, recursive = FALSE)))
  } else if (map_type == "by_row") {
    rlang::as_function(.f)(cur_row)
  }
}

##
##---------------------------------------------------------

jsonp2json <- function(x) {
  x[1] <- gsub("^__.*\\((.*)", "\\1", x)
  n <- length(x)
  x[n] <- gsub("\\)$", "", x[n])
  x
}

read_json_p <- function(path) {
  a <- readLines(path, warn = FALSE)
  if (grepl("jsonp$", path))
    a <- jsonp2json(a)
  jsonlite::fromJSON(paste(a, collapse = "\n"))
}

trelliscope_dir <- function(p)
  attr(p, "trelliscope_pars")$www_dir

#' @importFrom jug jug cors get serve_it decorate
launch_panel_server <- function(p, port = NULL) {
  base_path <- file.path(trelliscope_dir(p), "appfiles")
  group <- p$x$latest_display$group
  name <- p$x$latest_display$name

  pth <- list.files(file.path(base_path, "displays", group, name),
    pattern = "displayObj", full.names = TRUE)

  display_obj <- read_json_p(pth)
  if (!is.null(port))
    display_obj$panelInterface$port <- port

  get_panel_jug <- function(idx) {
    pnl <- get_ppanel(p$x$data, row = idx)

    pnl_pth <- file.path(base_path, "displays", group, name,
      ifelse(display_obj$jsonp, "jsonp", "json"),
      paste0(idx, ".", ifelse(display_obj$jsonp, "jsonp", "json")))
    if (!file.exists(pnl_pth)) {
      write_panel(pnl, idx, base_path = base_path, name = name, group = group,
        width = display_obj$width, height = display_obj$height,
        jsonp = display_obj$jsonp,
        split_layout = display_obj$panelInterface$split_layout)
    }

    res <- readLines(pnl_pth, warn = FALSE)
    if (display_obj$jsonp)
      res <- jsonp2json(res)

    paste(res, collapse = "\n")
  }

  # browseURL(paste0("http://localhost:", port, "/index.html"))

  jug::jug() %>%
    # serve_static_files(root_path = ".") %>%
    jug::cors(allow_headers = "Content-Type") %>%
    jug::get("/PANEL", jug::decorate(get_panel_jug)) %>%
    jug::serve_it(port = display_obj$panelInterface$port)
}
