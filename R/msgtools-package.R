#' @rdname msgtools-package
#' @name msgtools
#' @docType package
#' @title Tools for developing diagnostic messages
#' @description This package implements a number utilities for developing and maintaining error, warning, and other diagnostic messages in R packages, including checking for consistency across messages, spell-checking messages, and managing internationalization and location of messages (i.e., translations into various languages).
#' @template pkg
#' @template domain
#' @param verbose Logical. Should the function be chatty?
#' 
#' \code{use_localization} (alias: \code{use_l10n}) is the high-level, devtools-style function to create a \samp{/po} directory and initialize a translation template (.pot) file. 
#' 
#' \code{\link{make_template}} provides a lower-level interface for this internationalization step and \code{\link{make_translation}} handles localization. \code{\link{install_translations}} installs the translations for use.
#' 
#' \code{\link{get_messages}} returns a tibble (data frame) of messages and their file locations. \code{\link{spell_check_msgs}} returns a subset of messages with possible misspelled words and suggested replacements.
#' 
#' @note Most of the functionality of this package requires \samp{gettext}. Use \code{\link{check_for_gettext}} to see if it is available on your system.
#' @author Thomas J. Leeper
#' @examples
#' \dontrun{
#'   # get messages in memory
#'   get_messages()
#'   # spell check messages
#'   spell_check_msgs()
#' 
#'   # setup pkg for localization
#'   use_localization()
#'   
#'   # make a translation
#'   tran <- make_translation("es", translator = "Some Person <example@examle.com>")
#'   write_translation(tran)
#'
#'   # install translation(s)
#'   check_translations() # check for errors before install
#'   install_translations()
#' }
#' @seealso \code{\link{make_template}}, \code{\link{make_translation}}
#' @references \href{https://www.gnu.org/software/gettext/manual/gettext.html}{GNU gettext Manual}
#' @keywords package
NULL

#' @rdname msgtools-package
#' @importFrom devtools as.package
#' @export
use_localization <- use_l10n <- function(pkg = ".", domain = "R", 
    verbose = getOption("verbose")) {
    sync_template(pkg = pkg, domain = domain, verbose = verbose)
    return(invisible(TRUE))
}
