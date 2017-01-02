#' @rdname msgtools-package
#' @name msgtools
#' @docType package
#' @title Tools for developing diagnostic messages
#' @description This package implements a number utilities for developing and maintaining error, warning, and other diagnostic messages in R packages, including checking for consistency across messages, spell-checking messages, and managing internationalization and location of messages (i.e., translations into various languages).
#' @template pkg
#' @template domain
#' @details R provides built-in message internationalization. All calls to \code{\link[base]{stop}}, \code{\link[base]{warning}}, \code{\link[base]{message}}, \code{\link[base]{gettext}}, and \code{\link[base]{ngettext}} are automatically configured for internationalization. Creating-language specific translations of these messages thus simply requires setting up a package for localization, performing the translations (likely by hand), and installing the translations into the package so they are available to users. msgtools provides utilities for handling each of these steps.
#' 
#' \code{use_localization} (alias: \code{use_l10n}) is the high-level, devtools-style function to create a \samp{/po} directory and initialize a translation template (.pot) file. 
#' 
#' \code{\link{make_template}} provides a lower-level interface for this internationalization step and \code{\link{make_translation}} handles localization. \code{\link{install_translations}} installs the translations for use.
#' 
#' \code{\link{get_messages}} returns a tibble (data frame) of messages and their file locations. \code{\link{spell_check_msgs}} returns a subset of messages with possible misspelled words and suggested replacements.
#' 
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
#'   install_translations()
#' }
#' @seealso \code{\link{make_template}}, \code{\link{make_translation}}
#' @keywords package
NULL

#' @rdname msgtools-package
#' @importFrom devtools as.package
#' @export
use_localization <- use_l10n <- function(pkg = ".", domain = "R") {
    pkg <- as.package(pkg)
    po_dir <- make_po_dir(pkg = pkg)
    template_file <- template_path(pkg = pkg, domain = domain)
    template <- make_template(pkg = pkg, domain = domain)
    write_template(template = template, pkg = pkg)
    return(invisible(TRUE))
}
