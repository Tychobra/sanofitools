#' Adds the contents of `inst/` to `tbio/`
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
#'
.onLoad <- function(...) {
  shiny::addResourcePath("tbio", system.file("assets", package = "tbiotools"))

  invisible(NULL)
}