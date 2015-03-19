document_msgs <- 
function(pkg = ".",
         x, 
         file, 
         fmt = "Rd", 
         title = "Diagnostic messages",
         description = NULL,
         name = NULL,
         keywords = c("message", "warning", "error"),
         ...
){
    pkg <- as.package(pkg)
    if(missing(file)) {
        if(tolower(fmt) == "rd")
            file <- file.path(pkg$path, "man", paste0(pkg$package, "-messages.Rd"))
        else if(tolower(fmt) == "roxygen")
            file <- file.path(pkg$path, "R", paste0(pkg$package, "-messages.R"))
        else
            stop("'fmt' must be 'Rd' or 'roxygen'")
    }
    
    # document
    
    # format
    
    con <- file(file)
    on.exit(close(con))
    
    if(tolower(fmt) == "rd"){
        writeLines(
        c(paste0("\\name{",name,"}"),
          paste0("\\alias{",name,"}"),
          paste0("\\title{",title,"}"),
          "\\description{This page documents common messages, warnings, and errors for package \\dQuote{package}.}",
          "\\section{Messages}{"), con = con)
        
        # this part will write the object to file:
        writeLines(paste0("\\item ", names(x), ":", unname(x), collapse = "\n"), con = con)
        
        writeLines(file,
        c("}",
          "}",
          paste0("\\keyword{",keywords,"}",collapse="\n")))
        
    } else if (tolower(fmt) == "roxygen"){
        writeLines(
        c("#' ", title,
          "#'",
          "#' ",description,
          "#'",
          "#' @section Messages:",
          "#' \\itemize{"), con = con)
        
        # this part will write the object to file:
        writeLines(paste0("#' \\item ", names(x), ":", unname(x), collapse = "\n"), con = con)
        
        writeLines(c("#' }",
          "#'",
          "#' @name ", name,
          "#' @keywords ",paste(keywords, collapse = " "),
          "NULL"), con = con)

    } else {
        stop("'fmt' must be 'Rd' or 'roxygen'")
    }
    
    # write to file
    
    invisible(x)
}
