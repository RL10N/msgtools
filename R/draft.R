xgettext2pot <- function(dir, potFile, name = "R", version, bugs) 
{
    dir <- file_path_as_absolute(dir)
    if (missing(potFile)) 
        potFile <- paste0("R-", basename(dir), ".pot")
    tmp <- unique(unlist(xgettext(dir, asCall = FALSE)))
    tmp <- tmp[nzchar(tmp)]
    if (length(tmp) > 0L) 
        tmp <- shQuote(encodeString(tmp), type = "cmd")
    con <- file(potFile, "wt")
    on.exit(close(con))
    if (missing(version)) 
        version <- paste(R.version$major, R.version$minor, sep = ".")
    if (missing(bugs)) 
        bugs <- "bugs.r-project.org"
    writeLines(con = con, c("msgid \"\"", "msgstr \"\"", sprintf("\"Project-Id-Version: %s %s\\n\"", 
        name, version), sprintf("\"Report-Msgid-Bugs-To: %s\\n\"", 
        bugs), paste0("\"POT-Creation-Date: ", format(Sys.time(), 
        "%Y-%m-%d %H:%M"), "\\n\""), "\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"", 
        "\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"", 
        "\"Language-Team: LANGUAGE <LL@li.org>\\n\"", "\"MIME-Version: 1.0\\n\"", 
        "\"Content-Type: text/plain; charset=CHARSET\\n\"", "\"Content-Transfer-Encoding: 8bit\\n\"", 
        ""))
    for (e in tmp) writeLines(con = con, c("", paste("msgid", 
        e), "msgstr \"\""))
    tmp <- xngettext(dir)
    un <- unique(unlist(tmp, recursive = TRUE))
    for (ee in tmp) for (e in ee) if (e[1L] %in% un) {
        writeLines(con = con, c("", paste("msgid       ", shQuote(encodeString(e[1L]), 
            type = "cmd")), paste("msgid_plural", shQuote(encodeString(e[2L]), 
            type = "cmd")), "msgstr[0]    \"\"", "msgstr[1]    \"\""))
        un <- un[-match(e, un)]
    }
}

as.pot.xgettext <- 
function(x,
         language,
         project.name = "R",
         project.version = paste(R.version$major, R.version$minor, sep = "."),
         creation.date = format(Sys.time(), "%Y-%m-%d %H:%M"),
         revision.date = format(Sys.time(), "%Y-%m-%d %H:%M"),
         translator.name = NULL,
         translator.email = NULL,
         team.name = NULL,
         team.email = NULL,
         bug.reports = NULL,
         ...
){
    
    # convert xgettext object to a pot object
    
    invisible(x)
}


read.pot <- function(file){
    con <- file(potFile, "r")
    on.exit(close(con))
    readLines(con)
    # convert to "xgetpot" class
}

write.pot <- function(x, file = NULL, replace){
    a <- attributes(x)
    
    # if is.null(file) try to write it into a logical place, otherwise fail
    pot <- read.pot(file)
    
    con <- file(file, "wt")
    on.exit(close(con))
    writeLines(con = con, 
               c("msgid \"\"", "msgstr \"\"", 
                 sprintf("\"Project-Id-Version: %s %s\\n\"", a$project.name, a$project.version), 
                 sprintf("\"Report-Msgid-Bugs-To: %s\\n\"", a$bug.reports), 
                 paste0("\"POT-Creation-Date: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\\n\""), 
                 "\"PO-Revision-Date: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\\n\"", 
                 "\"Last-Translator: ", a$translator.name, "<",a$translator.email,">", "\\n\"", 
                 "\"Language-Team: ", a$team.name, "<",a$team.email,">", "\\n\"", 
                 "\"MIME-Version: 1.0\\n\"", 
                 "\"Content-Type: text/plain; charset=CHARSET\\n\"", 
                 "\"Content-Transfer-Encoding: 8bit\\n\"", 
                "") )
    for (e in x)
        writeLines(con = con, c("", paste("msgid", e), "msgstr \"\""))
    tmp <- xngettext(dir)
    un <- unique(unlist(tmp, recursive = TRUE))
    for (ee in tmp) for (e in ee) if (e[1L] %in% un) {
        writeLines(con = con,
                   c("", 
                     paste("msgid       ", shQuote(encodeString(e[1L]), type = "cmd")),
                     paste("msgid_plural", shQuote(encodeString(e[2L]), type = "cmd")), 
                     "msgstr[0]    \"\"", "msgstr[1]    \"\"") )
        un <- un[-match(e, un)]
    }
}


update_pot_file <- function(x, file=NULL){
    xgettext
    read.pot()
    # compare read and write
    write.pot()
}

as.po.gettextpot <- function(x, ...){
    invisible(x)
}

print.xgettext <- function(x, ...){
    invisible(x)
}

print.xgetpot <- function(x, ...){
    invisible(x)
}

print.xgetpo <- function(x, ...){
    invisible(x)
}

document_msgs <- function(x, file, fmt = "Rd", ...){
    # document
    
    # format
    if(fmt == "Rd"){
        
    } else if (fmt == "roxygen"){
        
    } else {
        stop("'fmt' must be 'Rd' or 'roxygen'")
    }
    
    # write to file
    
    invisible(x)
}
