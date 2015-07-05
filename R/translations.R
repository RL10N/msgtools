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
    out <- c(out, paste0("\n\"", names(translation),": ", unname(unlist(translation)), "\""))
    
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
        template <- use_translations(pkg = pkg, domain = "R")
    }
    
    # check current translation
    if(!missing(translator))
        template$"Last-Translator" <- paste0(translator, "\\n")
    if(!missing(team))
        template$"Language-Team" <- paste0(team, "\\n")
    
    template$Language <- paste0(paste(language, sep = ":"), "\\n")
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

check_translations <- function(pkg = ".") {
    pkg <- as.package(pkg)
    po_dir <- file.path(pkg$path, "po")
    if(!file.exists(po_dir)) {
        stop("No /po directory found for package")
    }
    checkPoFiles(po_dir)
}

install_translations <- function(pkg = ".", which, check = TRUE) {
    pkg <- as.package(pkg)
    template_file <- file.path("po", paste0("R-", pkg$package, ".pot"))
    
    if(check & missing(which))
        check_translations(pkg = pkg)
    
    # R-level messages
    po_files <- dir("po", pattern = "R-.*[.]po$", full.names = TRUE)
    po_files <- po_files[po_files != "po/R-en@quot.po"]
    
    for (f in po_files) {
        lang <- sub("^R-(.*)[.]po$", "\\1", basename(f))
        message("  R-", lang, ":", appendLF = FALSE, domain = NA)
        if (system(paste("msgmerge --update", f, shQuote(template_file))) != 0L) {
            warning("running msgmerge on ", sQuote(f), " failed", domain = NA)
            next
        }
        if(check & !missing(which))
            res <- checkPoFile(f, TRUE)
        if (nrow(res)) {
            print(res)
            message("not installing", domain = NA)
            next
        }
        dest <- file.path(stem, lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("R-%s.mo", pkg$package))
        if (system(paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))) != 0L) 
            warning(sprintf("running msgfmt on %s failed", basename(f)), domain = NA, immediate. = TRUE)
    }
    if (l10n_info()[["UTF-8"]]) {
        lang <- "en@quot"
        message("  R-", lang, ":", domain = NA)
        f <- tempfile()
        #en_quote(template_file, f)
        dest <- file.path(stem, lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("R-%s.mo", pkg$package))
        cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))
        if (system(cmd) != 0L) 
            warning(sprintf("running msgfmt on %s failed", basename(f)), domain = NA, immediate. = TRUE)
    }
    
    # C-level messages
    po_files <- dir("po", pattern = "^[^R].*[.]po$", full.names = TRUE)
    po_files <- po_files[po_files != "po/en@quot.po"]
    if(!missing(which)) {
        # further select which translations to install from `pofiles`
    }
    
    # install C-level messages
    for (f in po_files) {
        lang <- sub("[.]po", "", basename(f))
        #message("  ", lang, ":", appendLF = FALSE, domain = NA)
        if (system(paste("msgmerge --update", shQuote(f), shQuote(template_file))) != 0L) {
            warning("running msgmerge on ", f, " failed", domain = NA)
            next
        }
        if(check & !missing(which))
            res <- checkPoFile(f, TRUE)
        if (nrow(res)) {
            print(res)
            message("not installing", domain = NA)
            next
        }
        dest <- file.path(stem, lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, paste0("R-", pkg$package, ".mo"))
        if (system(paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))) != 0L) 
            warning(sprintf("running msgfmt on %s failed", basename(f)), domain = NA)
    }
    if (l10n_info()[["UTF-8"]]) {
        lang <- "en@quot"
        message("  ", lang, ":", domain = NA)
        f <- tempfile()
        #en_quote(template, f)
        dest <- file.path(stem, lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("%s.mo", domain))
        cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))
        if (system(cmd) != 0L) 
            warning(sprintf("running msgfmt on %s failed", basename(f)), domain = NA)
    }
}

