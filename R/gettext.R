#' @rdname gettext
#' @title \samp{gettext} availability
#' @description Check whether \samp{gettext} is available on your system; and install for Windows.
#' @param location A character string specifying a file path where the \samp{gettext} binaries should be installed.
#' @return A logical.
#' @details \code{install_gettext} provides a Windows-only installer into the specified directory. That directory should be one included in \env{PATH} so that the binaries are available.
#' @note Windows binaries for \samp{gettext} can also be installed from \url{http://www.stats.ox.ac.uk/pub/Rtools/goodies/gettext-tools.zip} or \url{https://github.com/mlocati/gettext-iconv-windows}.
#' @seealso \code{\link{use_localization}}
#' @examples
#' \dontrun{
#'   install_gettext()
#' }
#' check_for_gettext()
#' @export
check_for_gettext <- function() {
    e <- system("xgettext -h", show.output.on.console = FALSE, ignore.stdout = TRUE)
    if (e != 0L) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

#' @rdname gettext
#' @importFrom utils unzip download.file
#' @export
install_gettext <- function(location) {
    if (Sys.info()["sysname"] == "Windows") {
        uri <- "http://www.stats.ox.ac.uk/pub/Rtools/goodies/gettext-tools.zip"
        f <- tempfile()
        on.exit(unlink(f))
        download.file(uri, destfile = f, mode = "wb")
        unzip(f, exdir = location)
        return(location)
    } else {
        stop("Installation is currently only supported for Windows")
    }
}
