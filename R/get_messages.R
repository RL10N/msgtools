get_messages <- function(pkg = ".", type = "gettext") {
    pkg <- as.package(pkg)
    type <- match.arg(type, c("gettext", "ngettext"))
    if(type == "gettext") {
        xgettext(pkg$path, asCall = FALSE)
    } else {
        xngettext(pkg$path)
    }
}

gettext("This is a single test message")
ngettext(2, "This is a test message", "These are two test messages")
