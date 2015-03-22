# toggles to check
setlang <- function(language) {
    options("msgtools_LANG" = Sys.getenv("LANG"))
    Sys.setenv(LANG = language)
}

resetlang <- function(language) {
    if(missing(language))
        Sys.setenv(LANG = getOption("msgtools_LANG", "EN"))
    else
        Sys.setenv(LANG = language)
}
