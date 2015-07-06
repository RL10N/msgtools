edit_translation <- function(language, file, pkg = ".", domain = "R", write = TRUE) {
    pkg <- as.package(pkg)
    po_dir <- file.path(pkg$path, "po")
    if(!file.exists(po_dir)) {
        make_po_dir(pkg = pkg)
    }
    template <- use_translations(pkg = pkg, domain = domain)
    
    if(missing(file)) {
        file <- file.path(pkg$path, "po", paste0(if(domain == "R") "R-" else NULL, language, ".po"))
    } else if(missing(language)) {
        language <- gsub(".po", "", basename(file))
        if(domain == "R")
            language <- gsub("R-", "", language)
    } else {
        stop("Either 'file' or 'language' is required")
    }
    if(file.exists(file)) {
        if(!translation_current(file = file)) {
            tmp <- make_translation(language = language, pkg = pkg, domain = domain, write = FALSE)
            po <- attributes(tmp)$po
        } else {
            po <- read_translation(file = file)
        }
    } else {
        po <- make_translation(language = language, pkg = pkg, domain = domain, write = FALSE)
    }
    
    # command-line interface to update po translation
    esingle <- function(msgid, msgstr) {
        message(paste0(gettext("Original message is: "), msgid))
        message(paste0(gettext("Current translation is: "), msgstr))
        readline(gettext("Translation: "))
    }
    eplural <- function(x) {
        message(paste0(gettext("Original message is: "), x))
        
        # generalize to enter values of plural and respective message
        readline(gettext("Translation: "))
    }
    
    m <- sapply(po$msgids, `[`, "msgid")
    mlist <- select.list(choices = m, title = gettext("Select Messages to Translate"))
    
    # convert this to an interactive menu    
    for(i in mlist) {
        if(inherits(msgids[[i]], "gettext_msg")) {
            po$msgids[[i]]$msgstr <- esingle(po$msgids[[i]]$msgid, po$msgids[[i]]$msgstr)
        } else {
            # generalize to enter multiple values for ngettext_msg's
        }
    }
    
    po_file <- if(write) write_translation(po) else ""
    structure(translation, file = po_file)
}

