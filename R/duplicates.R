find_msg_duplicates <- function(pkg = ".") {
    pkg <- as.package(pkg)
    msgs <- unlist(get_messages(pkg = pkg))
    locations <- names(msgs) <- basename(names(msgs))
    locations <- gsub("[[:digit:]]+$", "", locations)
    distance <- sapply(msgs, adist, x = msgs)
    colnames(distance) <- NULL
    matches <- sapply(msgs, function(z) agrep(z, x = msgs[msgs != z]))
    structure(list(Message = unname(msgs), Location = locations, Matches = matches, Distance = distance), class = "msgduplicates")
}

print.msgduplicates <- function(x, ...) {
    d <- numeric(nrow(x$Distance))
    for(i in 1:nrow(x$Distance))
        d[i] <- min(x$Distance[i,-i])
    print(data.frame(Message = x$Message, Location = x$Location, 
                     Matches = sapply(x$Matches, function(z) if(length(z)) paste(z, collapse = ",") else "None"),
                     MinDistance = d,
                     stringsAsFactors = FALSE), right = FALSE, row.names = FALSE)
    invisible(x)
}
