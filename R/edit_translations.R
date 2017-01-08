#' @title Interactive translation editing
#' @description Edit a translation object in-memory using a simple interface
#' @param translation An object of class \code{"po"}, such as returned by \code{\link{make_translation}}. In lieu of \code{translation}, \code{language} and \code{pkg} and \code{domain} can be specified, which will load the translation from file using \code{\link{read_translation}}.
#' @template language
#' @template pkg
#' @template domain
#' @return An object of class \code{"po"}.
#' @author Thomas J. Leeper
#' @note Emacs users may find the gettext PO file editor more comfortable: \url{https://www.gnu.org/software/gettext/manual/gettext.html#PO-Mode}.
#' @examples
#' \dontrun{
#'   # setup pkg for localization
#'   pkg <- dummy_pkg()
#'   use_localization(pkg = pkg)
#'   
#'   # generate translation in memory
#'   (tran <- make_translation("es", translator = "Some Person <example@examle.com>", pkg = pkg))
#'   
#'   # edit translations
#'   tran2 <- edit_translation(tran, pkg = pkg)
#'   # write to disk
#'   write_translation(tran2, pkg = pkg)
#' }
#' @seealso \code{\link{make_translation}}
#' @importFrom utils select.list
#' @export
edit_translation <- function(translation, language, pkg = ".", domain = "R") {
    
    if (missing(translation)) {
        translation <- read_translation(language = language, pkg = pkg, domain = domain)
    }
    
    # command-line interface to update po translation
    esingle <- function(msgid, msgstr) {
        message(paste0(gettext("English message is: "), msgid))
        if (length(msgstr) && !is.na(msgstr) && msgstr != "") {
            message(paste0(gettext("Current translation is: "), msgstr))
        } else {
            message("No current translation!\n")
        }
        
        readline(gettext("Translation: "))
    }
    eplural <- function(msgid, msgid_plural, msgstr) {
        message(paste0(gettext("English single message is: "), msgid))
        message(paste0(gettext("English plural message is: "), msgid_plural))
        if (!length(msgstr)) {
            message("No current translations!\n")
        }
        plurals <- list()
        for (i in 0:6) {
            message(paste0(gettext("Current plural [", i, "] translation is: "), msgstr[[i+1]]))
            plurals[[i + 1L]] <- readline(gettext("Translation (n == ", i, "): "))
        }
        plurals
    }
    
    msgs1 <- translation[["direct"]][["msgid"]]
    mlist1 <- select.list(choices = msgs1, multiple = TRUE, title = gettext("Select 'Direct' Messages to Translate"))
    mlist1 <- match(mlist1, msgs1)
    
    if (length(mlist1)) {
        for (i in mlist1) {
            translation[["direct"]][["msgstr"]][i] <- esingle(translation[["direct"]][["msgid"]][i], translation[["direct"]][["msgstr"]][i])
        }
    }
    
    msgs2 <- translation[["countable"]][["msgid"]]
    mlist2 <- select.list(choices = msgs2, multiple = TRUE, title = gettext("Select 'Pluralized' Messages to Translate"))
    mlist2 <- match(mlist2, msgs2)
    
    if (length(mlist2)) {
        for (i in mlist2) {
            translation[["countable"]][["msgstr"]][[i]] <- eplural(translation[["countable"]][["msgid"]][i], translation[["countable"]][["msgid_plural"]][i], translation[["countable"]][["msgstr"]][[i]])
        }
    }
    return(translation)
}

