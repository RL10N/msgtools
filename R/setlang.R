getlang <- function() {
    Sys.getenv("LANG")
}

setlang <- function(language) {
    options("msgtools_LANG" = getlang())
    Sys.setenv(LANG = language)
}

resetlang <- function(language) {
    if(missing(language))
        Sys.setenv(LANG = getOption("msgtools_LANG", "EN"))
    else
        Sys.setenv(LANG = language)
}
