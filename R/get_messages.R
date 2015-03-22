get_messages <- function(pkg = ".", type = "xgettext") {
    pkg <- as.package(pkg)
    if(type == "xgettext") {
        xgettext(pkg$path, asCall = FALSE)
    } else if (type == "xngettext") {
        xngettext(pkg$path)
    }
}
