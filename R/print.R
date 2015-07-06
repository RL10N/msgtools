print.msgtemplate <- function(x, ...){
    m <- x$msgids
    x$msgids <- NULL
    cat(sprintf("%s-level message template (.pot) for %s\n", 
                attributes(x)$domain, 
                gsub("\\\\n", "", x$"Project-Id-Version")), 
        "\n")
    x$"Project-Id-Version" <- NULL
    print(data.frame("Template Metadata" = names(x), 
                     Value = gsub("\\\\n", "", unname(unlist(x))), 
                     check.names = FALSE), 
          right = FALSE, row.names = FALSE)
    cat(ngettext(length(m), sprintf("\nThere is 1 msgid in the template."),
                 sprintf("\nThere are %i msgid's in the template.", length(m))),
        "\n\n")
    invisible(x)
}

print.msgtranslation <- function(x, ...){
    m <- x$msgids
    x$msgids <- NULL
    cat(sprintf("%s-level %s-language message translation (.po) for %s\n", 
                attributes(x)$domain, 
                toupper(gsub("\\\\n", "", x$"Language")),
                gsub("\\\\n", "", x$"Project-Id-Version")), 
        "\n")
    x$"Project-Id-Version" <- NULL
    print(data.frame("Translation Metadata" = names(x), 
                     Value = gsub("\\\\n", "", unname(unlist(x))),
                     check.names = FALSE), 
          right = FALSE, row.names = FALSE)
    cat(ngettext(length(m), sprintf("\nThere is 1 msgid in the translation."),
                 sprintf("\nThere are %i msgid's in the translation.", length(m))),
        "\n\n")
    invisible(x)
}

print.gettext_msg <- function(x, ...) {
    cat("msgid: ", x$msgid, "\n")
    cat("msgstr: ", x$msgstr, "\n\n")
    invisible(x)
}

print.ngettext_msg <- function(x, ...) {
    cat("msgid: ", x$msgid, "\n")
    cat("msgid_plural: ", x$msgid_plural, "\n")
    cat("msgstr[0]: ", x$msgstr0, "\n")
    cat("msgstr[1]: ", x$msgstr1, "\n\n")
    invisible(x)
}
