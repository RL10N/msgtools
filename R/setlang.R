# @rdname setlang
# @title Set or reset locale language
# @description Set or reset a locale language
# @param language A character string containing a language.
# @details \code{getlang} retrieves the current value of \samp{LANG} environment variables. \code{setlang} sets the \samp{LANG} environment variable and stores the supplied value of \code{language} in \code{options("msgtools_LANG")}. \code{resetlang} resets the language to the value specified in that option or, if that is missing, to English (\dQuote{EN}). This can be useful for testing message translations in various languages.
# @return Invisibly, a logical indicating with the language setting was successful.
# @author Thomas J. Leeper
# @seealso \code{\link[poio]{language_codes}}
# @import poio
# @export
setlang <- function(language) {
    check_language_regex(language)
    options("msgtools_LANG" = getlang())
    Sys.setenv(LANG = language)
}

# @rdname setlang
# @export
getlang <- function() {
    Sys.getenv("LANG")
}

# @rdname setlang
# @export
resetlang <- function(language) {
    if (missing(language)) {
        Sys.setenv(LANG = getOption("msgtools_LANG", "EN"))
    } else {
        check_language_regex(language)
        Sys.setenv(LANG = language)
    }
}

check_language_regex <- function(language) {
    if ((!grepl(ALLOWED_LANGUAGE_REGEX, language)) && language != "en@quot") {
        stop("'language' does not matched any allowed value")
    }
    return(TRUE)
}
