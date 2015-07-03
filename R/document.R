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
    fmt <- match.arg(tolower(fmt), c("rd", "roxygen"))
    if(missing(file)) {
        if(fmt == "rd") {
            file <- file.path(pkg$path, "man", paste0(pkg$package, "-messages.Rd"))
        } else {
            file <- file.path(pkg$path, "R", paste0(pkg$package, "-messages.R"))
        }
    }
    
    # extract messages
    m1 <- get_messages(pkg = pkg, type = "xgettext")
    m2 <- get_messages(pkg = pkg, type = "xngettext")
    
    
    # check if documentation file already exists
    # if file exists:
    
        # parse documentation file
    
        # compare file to messages
        
        # update file
    
    # if file doesn't exist:
    
        # document messages
        
        # write to file
    
    con <- file(file)
    on.exit(close(con))
    
    if(fmt == "rd"){
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
        
    } else {
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

    }
    
    # write to file
    
    invisible(x)
}
