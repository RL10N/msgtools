#' @rdname check_for_gettext
#' @title Check for \samp{gettext} availability
#' @description A simple function to check whether \samp{gettext} is available on your system.
#' @return A logical.
#' @note Windows binaries for \samp{gettext} can be installed from \url{http://www.stats.ox.ac.uk/pub/Rtools/goodies/gettext-tools.zip} or \url{https://github.com/mlocati/gettext-iconv-windows}.
#' @seealso \code{\link{use_localization}}
#' @examples
#' check_for_gettext()
#' @export
check_for_gettext <- function() {
    e <- system("xgettext --version")
    if (e != 0L) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}
