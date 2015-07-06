parse_translation <- function(translation) {
    structure(parse_template(translation), class = "msgtranslation")
}

read_translation <- function(language, file, pkg = ".", domain = "R") {
    if(!missing(language)) {
        pkg <- as.package(pkg)
        file <- file.path(pkg$path, "po", paste0(if(domain == "R") "R-" else NULL, language, ".po"))
    } else if(!missing(file)) {
        language <- gsub(".po", "", basename(file))
        if(domain == "R")
            language <- gsub("R-", "", language)
    } else {
        stop("Either 'file' or 'language' is required")
    }
    con <- file(file, "r")
    on.exit(close(con))
    structure(parse_translation(readLines(con)), file = file)
}

write_translation <- function(translation, pkg = ".", domain = "R") {
    pkg <- as.package(pkg)
    language <- strsplit(tolower(translation$Language),":",fixed = TRUE)[[1]][1]
    language <- gsub("\\n", "", language, fixed = TRUE)
    po_dir <- file.path(pkg$path, "po")
    po_file <- file.path(po_dir, paste0(if(domain == "R") "R-" else NULL, language, ".po"))
    out <- c("msgid \"\"\nmsgstr \"\"")
    msgids <- translation$msgids
    translation$msgids <- NULL
    out <- c(out, paste0("\n\"", names(translation),": ", unname(unlist(translation)), "\\n\""))
    
    for(i in seq_along(msgids)) {
        if(inherits(msgids[[i]], "gettext_msg")) {
            out <- c(out, paste0("\n\nmsgid \"", unlist(msgids[[i]]$msgid), "\"\nmsgstr \"\""))
        } else {
            out <- c(out, paste0("\n\nmsgid \"", unlist(msgids[[i]]$msgid), 
                                 "\"\nmsgid_plural \"", unlist(msgids[[i]]$msgid_plural), 
                                 "\"\nmsgstr[0] ", "\"\"",
                                 "\nmsgstr[1] ", "\"\""))
        }
    }
    con <- file(po_file, "w")
    on.exit(close(con))
    writeLines(c(out, "\n"), con = con, sep = "")
}

translation_current <- function(language, file, template, pkg = ".", domain = "R") {
    if(missing(template)) {
        template <- read_template(pkg = pkg, domain = domain)
    }
    if(missing(language)) {
        translation <- read_translation(file = file)
    } else if(missing(file)) {
        translation <- read_translation(language = language)
    } else {
        stop("Either 'file' or 'language' is required")
    }
    
    # do not compare creation or revision dates
    translation2 <- translation
    translation2$"PO-Revision-Date" <- template$"PO-Revision-Date"
    translation2$"POT-Creation-Date" <- template$"POT-Creation-Date"
    structure(identical(translation2, template), 
              template = template, 
              translation = translation,
              comparison = all.equal(translation2, template))
}

make_translation <- 
function(language, 
         pkg = ".", 
         translator,
         team,
         domain = "R", 
         write = TRUE) {
    pkg <- as.package(pkg)
    po_dir <- file.path(pkg$path, "po")
    if(domain == "R") {
        template_file <- file.path(po_dir, paste0("R-",pkg$package, ".pot"))
        po_file <- file.path(po_dir, paste0("R-", language, ".po"))
    } else {
        template_file <- file.path(po_dir, paste0(pkg$package, ".pot"))
        po_file <- file.path(po_dir, paste0(language, ".po"))
    }
    if(file.exists(template_file)) {
        template <- read_template(template_file, pkg = pkg, domain = domain)
    } else {
        template <- use_translations(pkg = pkg, domain = domain)
    }
    
    # check current translation
    if(!missing(translator))
        template$"Last-Translator" <- translator
    if(!missing(team))
        template$"Language-Team" <- team
    
    template$Language <- paste(language, sep = ":")
    if(file.exists(po_file)) {
        current <- translation_current(file = po_file, template = template)
        translation <- attributes(current)$translation
        if(current) {
            message(sprintf("%s translation already up-to-date", language))
            return(attributes(current)$translation, file = po_file, domain = domain)
        } else {
            # update existing translation, leaving unchanged messages unchanged
            m_old <- translation$msgids
            m_new <- template$msgids
            m_old_m1 <- sapply(m_old, `[`, "msgid")
            m_old_m2 <- sapply(m_old, `[`, "msgid_plural")
            m_new_m1 <- sapply(m_new, `[`, "msgid")
            m_new_m2 <- sapply(m_new, `[`, "msgid_plural")
            
            # add new messages
            if(!setequal(m_old_m1, m_new_m1)) {
                
            }
            if(!setequal(m_old_m2, m_new_m2)) {
            
            }
            
            if(write) write_translation(translation)
            return(structure(`class<-`(template, "msgtranslation"), file = po_file, domain = domain))
        }
    }
    if(write) write_translation(template)
    structure(`class<-`(template, "msgtranslation"), file = po_file, domain = domain)
}

