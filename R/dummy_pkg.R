#' @title Dummy package creation
#' @description Create a simple example package in the temporary directory
#' @param location A character string containing a path to a directory in which to create the package, by default the location of \code{\link[base]{tempdir}}.
#' @return A string denoting a path to the root of the package (called \dQuote{translateme}).
#' @importFrom devtools create
#' @export
dummy_pkg <- function(location = tempdir()) {
    
    pkg_path <- file.path(location, "translateme")
    
    if (!dir.exists(location)) {
        dir.create(location, recursive = TRUE)
    }
    if (dir.exists(file.path(location, "translateme"))) {
        unlink(pkg_path, recursive = TRUE)
    }
    
    description <- list(
      Title = "An Example Package",
      Description = "Demonstrates 'msgtools' functionality",
      BugReports = "https://github.com/RL10N/msgtools/issues",
      License = "Unlimited"
    )
    
    create(pkg_path, description = description, rstudio = FALSE)

    # initialize some messages
    dump("translatable_messages", file = file.path(pkg_path, "R", "fns.R"))
    pkg_path
}
