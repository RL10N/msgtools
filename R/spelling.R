#' @title Spell check diagnostic messages
#' @description Extracts diagnostic messages from a package and checks them for spelling errors.
# @param language A character string specifying a translation language.
#' @param dict A \code{\link[hunspell]{dictionary}} object.
#' @template pkg 
#' @details Extracts diagnostic messages from a package and processes them using \code{\link[hunspell]{hunspell}} to check for possible spelling mistakes.
#' @return A data frame containing each message with a potentially misspelled word, the misspelled word, and the source code file in which the message is located, and (for each word) a list of suggested corrections. If no words are misspelled, a data frame with zero rows.
#' @author Thomas J. Leeper
#' @examples
#' \dontrun{
#'   # Not run since it takes too long
#'   pkg <- extract_example_pkg()
#'   spell_check_msgs(pkg)
#' }
#' @seealso \code{\link{get_messages}}, \code{\link{get_message_distances}}
#' @importFrom tibble as_tibble
#' @importFrom hunspell hunspell dictionary hunspell_suggest
#' @export
spell_check_msgs <- function(dict = dictionary("en_US"), pkg = ".") {
    pkg <- as.package(pkg)
    all_msgs <- get_messages(pkg = pkg)
    
    msgs1 <- all_msgs[["msgid"]]
    out1 <- hunspell(msgs1, dict = dict)
    ret1 <- 
    structure(list(message = rep(msgs1, lengths(out1)), 
                   misspelling = unlist(out1),
                   file = rep(all_msgs[["file"]], lengths(out1))), 
              row.names = seq_len(sum(lengths(out1))), 
              class = "data.frame")
    
    msgs2 <- all_msgs[["msgid_plural"]][!is.na(all_msgs[["msgid_plural"]])]
    out2 <- hunspell(msgs2, dict = dict)
    ret2 <- 
    structure(list(message = rep(msgs2, lengths(out2)), 
                   misspelling = unlist(out2),
                   file = rep(all_msgs[["file"]][!is.na(all_msgs[["msgid_plural"]])], lengths(out2))), 
              row.names = seq_len(sum(lengths(out2))), 
              class = "data.frame")
    
    out <- rbind(ret1, ret2)
    out <- as_tibble(out[!is.na(out[["misspelling"]]), ])
    out[["suggestions"]] <- hunspell_suggest(out[["misspelling"]])
    return(out)
}

spell_check_translation <- function(language, dict = dictionary(language), domain = "R", pkg = ".") {
    
    # function to spell check translations in a given language
    
    pkg <- as.package(pkg)
    
    translation <- read_translation(language = language, domain = domain, pkg = pkg)
    
    translation[["direct"]]
    translation[["countable"]]
    
    # do stuff

}
