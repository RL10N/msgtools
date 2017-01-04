#' @title Compare message distance
#' @description Compare the string distance of messages to check for near-duplicates
#' @template pkg
#' @details Compares the generalized Levenshtein (edit) distance between pairs of messages using \code{\link[utils]{adist}}, returning the data frame of messages from \code{\link{get_messages}} with additional columns corresponding to the pairwise distance
#' @return A \dQuote{tibble} (data frame) containing messages, pluralized messages, and the file location of each message.
#' @author Thomas J. Leeper
#' @examples
#' pkg <- dummy_pkg()
#' 
#' # get message distances
#' dist <- get_message_distances(pkg = pkg)
#' @seealso \code{\link{get_messages}}, \code{\link{spell_check_msgs}}
#' @importFrom tibble as_tibble
#' @importFrom utils adist
#' @export
get_message_distances <- function(pkg = ".") {
    pkg <- as.package(pkg)
    msgs <- get_messages(pkg = pkg)
    
    d <- as_tibble(sapply(msgs[["msgid"]], adist, y = msgs[["msgid"]], fixed = TRUE))
    names(d) <- paste0("msg_", seq_along(msgs[["msgid"]]))
    structure(cbind(msgs["msgid"], d), class = c("tbl_df", "tbl", "data.frame"))
}
