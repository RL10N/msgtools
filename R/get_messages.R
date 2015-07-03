get_messages <- function(pkg = ".", type = "xgettext") {
    pkg <- as.package(pkg)
    type <- match.arg(type, c("xgettext", "xngettext"))
    if(type == "xgettext") {
        xgettext(pkg$path, asCall = FALSE)
    } else if (type == "xngettext") {
        xngettext(pkg$path)
    }
}
