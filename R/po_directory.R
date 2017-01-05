#' @rdname po_directory
#' @title Utilities for translation directory
#' @description Check for and create a /po directory
#' @template pkg
#' @template verbosity
#' @return The path to the directory.
#' @examples
#' # create example package
#' pkg <- dummy_pkg()
#' if (!check_for_po_dir(pkg)) {
#'   make_po_dir(pkg)
#' }
#' @export
make_po_dir <- function(pkg = ".", verbose = getOption("verbose")) {
    pkg <- as.package(pkg)
    po_dir <- file.path(pkg$path, "po")
    if (dir.exists(po_dir)) {
        return(po_dir)
    }
    if (isTRUE(verbose)) {
        message(sprintf("Creating the directory", "'po'"))
    }
    dir.create(po_dir, FALSE)
    return(po_dir)
}

template_path <- function(pkg = ".", domain = "R") {
    pkg <- as.package(pkg)
    file.path(pkg$path, "po", paste0(if(domain %in% c("r","R")) "R-" else NULL, pkg$package, ".pot"))
}

translation_path <- function(language, pkg = ".", domain = "R") {
    pkg <- as.package(pkg)
    file.path(pkg$path, "po", paste0(if(domain %in% c("R", "r")) "R-" else NULL, language, ".po"))
}
