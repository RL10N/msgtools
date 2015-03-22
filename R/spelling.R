spell_check <- function(pkg = ".", control = list(), program = NULL) {
    pkg <- as.package(pkg)
    msgs <- get_messages(pkg = pkg)
    d <- tempdir()
    f <- character(length(msgs))
    for(i in seq_along(msgs)) {
        f[i] <- tempfile(tmpdir = d, fileext = paste0("__",basename(names(msgs)[i])))
        writeLines(msgs[[i]], con = f[i])
        on.exit(unlink(f[i]))
    }
    out <- aspell(f, control = control, program = program)
    out$Message <- character(nrow(out))
    for(i in 1:nrow(out)) {
        out$Message[i] <- readLines(out$File[i], n = out$Line[i])[out$Line[i]]
    }
    names(out)[names(out) == "Original"] <- "Word"
    out$File <- sapply(strsplit(out$File, "__"),`[`, 2)
    structure(out, class = c("spell_check","data.frame"))
}

print.spell_check <- function(x, ...) {
    if(nrow(x) > 0) {
        out <- data.frame(Word = x$Word,
                          Message = x$Message,
                          Suggestions = sapply(s$Suggestions, function(x) {
                              paste(c(head(x, 5), if(length(x) > 5) "..." else ""), collapse = ", ")
                          }),
                          File = x$File, 
                          Line = x$Line,
                          Column = x$Column)
        print(out, right = FALSE)
    } else {
        message("No spelling errors found")
    }
    invisible(x)
}
