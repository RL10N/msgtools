document_msgs <- 
function(x, 
         file, 
         fmt = "Rd", 
         title = "Diagnostic messages",
         description = NULL,
         name = NULL,
         keywords = c("message", "warning", "error"),
         ...
){
    # document
    
    # format
    if(fmt == "Rd"){
        writeLines(file,
        c(paste0("\\name{",name,"}"),
        paste0("\\alias{",name,"}"),
        paste0("\\title{",title,"}"),
        "\\description{This page documents common messages, warnings, and errors for package \\dQuote{package}.}",
        "\\section{Messages}{"))
        
        # this part will write the object to file:
        writeLines(file, paste0("\\item ", names(x), ":", unname(x), collapse = "\n"))
        
        writeLines(file,
        c("}",
        "}",
        paste0("\\keyword{",keywords,"}",collapse="\n")))
        
    } else if (fmt == "roxygen"){
        writeLines(file,
        c("#' ", title,
        "#'",
        "#' ",description,
        "#'",
        "#' @section Messages:",
        "#' \\itemize{"))
        
        # this part will write the object to file:
        writeLines(file, paste0("#' \\item ", names(x), ":", unname(x), collapse = "\n"))
        
        writeLines(file,
        c("#' }",
        "#'",
        "#' @name ", name,
        "#' @keywords ",paste(keywords, collapse = " "),
        "NULL"))

    } else {
        stop("'fmt' must be 'Rd' or 'roxygen'")
    }
    
    # write to file
    
    invisible(x)
}
