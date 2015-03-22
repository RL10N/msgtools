language_code <- function(language) {
    e <- new.env()
    data("ISO_639_2", package = "ISOcodes", envir = e)
    n <- grep(tolower(language), tolower(e$ISO_639_2$Name))
    v <- e$ISO_639_2$Name[n]
    data.frame(Language = v, Code = e$ISO_639_2$Alpha_2[n], stringsAsFactors = FALSE)
}

country_code <- function(country) {
    e <- new.env()
    data("ISO_3166_1", package = "ISOcodes", envir = e)
    n1 <- grep(tolower(country), tolower(e$ISO_3166_1$Name))
    n2 <- grep(tolower(country), tolower(e$ISO_3166_1$Official_name))
    n3 <- grep(tolower(country), tolower(e$ISO_3166_1$Common_name))
    v <- unique(c(n1,n2,n3))
    setNames(e$ISO_3166_1[v,c("Alpha_2", "Name", "Official_name", "Common_name")], 
             c("Code", "Name", "Official_name", "Common_name"))
}
