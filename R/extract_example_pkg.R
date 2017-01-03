#' Extract the dummy package
#' 
#' Extract the dummy \code{translateme} package from its zip file.
#' @param extract_to A string denoted the path to extract the zip file to.
#' @return A string denoting a path to the root of the \code{translateme}
#' package. That is, the directroy returned by 
#' \code{file.path(extract_to, "translateme")}.
#' @examples 
#' extract_example_pkg()
#' @importFrom utils unzip
#' @export
extract_example_pkg <- function(extract_to = tempdir()) {
  zipfile <- system.file("extdata", "translateme.zip", package = "msgtools")
  unzip(zipfile, exdir = extract_to)
  file.path(extract_to, "translateme")
}
