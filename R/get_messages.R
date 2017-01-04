#' @title Extract diagnostic messages 
#' @description Extracts diagnostic messages from a package using \code{\link[tools]{xgettext}}
#' @template pkg
#' @details Extracts diagnostic messages from a package.
#' @return A \dQuote{tibble} (data frame) containing messages, pluralized messages, and the file location of each message.
#' @author Thomas J. Leeper
#' @examples
#' # get tidy data frame of messages
#' pkg <- extract_example_pkg()
#' get_messages(pkg)
#' @seealso \code{\link{spell_check_msgs}}, \code{\link{get_message_distances}}
#' @importFrom tibble tibble
#' @importFrom tools xgettext xngettext
#' @export
get_messages <- function(pkg = ".") {
    pkg <- as.package(pkg)
    
    gettextmsgs <- xgettext(pkg$path, asCall = FALSE)
    msgsA1 <- unlist(gettextmsgs)
    msgsA2 <- rep(NA_character_, length(msgsA1))
    filesA <- rep(names(gettextmsgs), lengths(gettextmsgs))
    filesA <- gsub(paste0(pkg$path, "/"), "", filesA)
    
    ngettextmsgs <- xngettext(pkg$path)
    msgsB <- do.call("rbind.data.frame", lapply(ngettextmsgs, function(x) do.call("rbind", x)))
    filesB <- rep(names(ngettextmsgs), lengths(ngettextmsgs))
    filesB <- gsub(paste0(pkg$path, "/"), "", filesB)
    
    tibble(msgid = c(msgsA1, as.character(msgsB[["msg1"]])),
           msgid_plural = c(msgsA2, as.character(msgsB[["msg2"]])),
           type = c(rep("direct", length(filesA)), rep("countable", length(filesB))),
           file = c(filesA, filesB))
}

some_nonexported_messages <- function() {
    gettext("This is a single test message")
    ngettext(2, "This is a test message", "These are two test messages")
    ngettext(2, "There is another test message", "These are another two test messages")
}
