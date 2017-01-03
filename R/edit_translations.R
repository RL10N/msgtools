#' @title Interactive translation editing
#' @description Edit a translation object in-memory using a simple interface
#' @param translation An object of class \code{"po"}, such as returned by \code{\link{make_translation}} or \code{\link{read_translation}}.
#' @return \code{make_translation} and \code{read_translation} reutrn an object of class \code{"po"}.
#' @author Thomas J. Leeper
#' @note Emacs users may find the gettext PO file editor more comfortable: \url{https://www.gnu.org/software/gettext/manual/gettext.html#PO-Mode}.
#' @examples
#' \dontrun{
#'   # setup pkg for localization
#'   use_localization()
#'   
#'   # generate translation in memory
#'   (tran <- make_translation("es", translator = "Some Person <example@examle.com>"))
#'   
#'   # edit translations
#'   tran2 <- edit_translation(tran)
#'   # write to disk
#'   write_translation(tran2)
#' }
#' @seealso \code{\link{make_translation}}
#' @importFrom utils select.list
#' @export
edit_translation <- function(translation) {
    
    # command-line interface to update po translation
    esingle <- function(msgid, msgstr) {
        message(paste0(gettext("Original message is: "), msgid))
        if (length(msgstr) && !is.na(msgstr) && msgstr != "") {
            message(paste0(gettext("Current translation is: "), msgstr))
        } else {
            message("Current translation is empty")
        }
        
        readline(gettext("Translation: "))
    }
    eplural <- function(msgid, msgid_plural, msgstr) {
        message(paste0(gettext("Original message is: "), msgid))
        message(paste0(gettext("Original plural message is: "), msgid_plural))
        if (length(msgstr)) {
            message(paste0(gettext("Current translations are: "), paste0(msgstr, collapse = "\n")))
        } else {
            message("Current translation is empty")
        }
        
        list(readline(gettext("Translation (n == 0): ")),
             readline(gettext("Translation (n == 1): ")),
             readline(gettext("Translation (n == 2): ")),
             readline(gettext("Translation (n == 3): ")),
             readline(gettext("Translation (n >= 4): "))
             )
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

