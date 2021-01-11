# The following code was taken directly from the {xfun} package. It is
# licensed under the MIT license. The copyright holder is Yihui Xie.

is_abs_path <- function (x) {
    if (is_unix())
        grepl("^[/~]", x)
    else !same_path(x, file.path(".", x))
}

same_path <- function (p1, p2, ...) {
    normalize_path(p1, ...) == normalize_path(p2, ...)
}

normalize_path <- function (x, winslash = "/", must_work = FALSE) {
    res = normalizePath(x, winslash = winslash, mustWork = must_work)
    if (is_windows())
        res[is.na(x)] = NA
    res
}

is_windows <- function () {
  .Platform$OS.type == "windows"
}

is_unix <- function () {
  .Platform$OS.type == "unix"
}
