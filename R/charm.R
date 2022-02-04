#' Use fidelius to password protect a trelliscope display
#' @param x an object of class "facet_trelliscope" or 
#' "trelliscopejs_widget"
#' @param ... arguments passed to [fidelius::charm()]
#' @export
tr_charm <- function(x, ...) {
  if (!inherits(x, c("facet_trelliscope", "trelliscopejs_widget")))
    stop("Can only call tr_charm() on a trelliscope object")
  attr(x, "fidelius_pars") <- list(...)
  x
}
