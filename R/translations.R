make_po_dir <- function(pkg = ".") {
    pkg <- as.package(pkg)
    po_dir <- file.path(pkg$path, "po")
    dir.create(po_dir, FALSE)
    return(po_dir)
}

use_translations <- 
function(pkg = ".", 
         created = format(Sys.time(), "%Y-%m-%d %H:%M"),
         maintainer,
         translator,
         team,
         charset = "UTF-8",
         domain = "R") {
    pkg <- as.package(pkg)

    po_dir <- file.path(pkg$path, "po")
    if(!file.exists(po_dir))
        po_dir <- make_po_dir(pkg = pkg)
    if(domain == "R")
        domain <- paste0("R-", pkg$package)
    else
        domain <- pkg$package
    template_file <- file.path(pkg$path, "po", paste0(domain, ".pot"))
    
    # generate internal msgtemplate representation (calling `xgettext` and `xngettext`)
    template <- 
    c("msgid \"\"", 
      "msgstr \"\"", 
      sprintf("\"Project-Id-Version: %s %s\\n\"", pkg$package, pkg$version), 
      sprintf("\"Report-Msgid-Bugs-To: %s\\n\"", if(!missing(maintainer)) format(maintainer) else pkg$maintainer), 
      paste0("\"POT-Creation-Date: ", created, "\\n\""), 
      paste0("\"PO-Revision-Date: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\\n\""), 
      paste0("\"Last-Translator: ", if(!missing(translator)) format(translator) else NULL, "\\n\""), 
      paste0("\"Language-Team: ", if(!missing(team)) format(team) else NULL, "\\n\""), 
      "\"Language: LL \\n\"",
      "\"MIME-Version: 1.0\\n\"", 
      paste0("\"Content-Type: text/plain; charset=", charset, "\\n\""), 
      "\"Content-Transfer-Encoding: 8bit\\n\"")
    
    # run xgettext
    tmp <- unique(unlist(get_messages(pkg = pkg, type = "xgettext")))
    tmp <- tmp[nzchar(tmp)]
    if (length(tmp) > 0L) {
        tmp <- shQuote(encodeString(tmp), type = "cmd")
        template <- c(template, paste0("\nmsgid ",tmp,"\nmsgstr \"\""))
    }
    
    # run xngettext
    tmp <- get_messages(pkg = pkg, type = "xngettext")
    un <- unique(unlist(tmp, recursive = TRUE))
    for (ee in tmp) {
        for (e in ee) {
            if (e[1L] %in% un) {
               template <- c(template, paste0(paste0("\nmsgid        ", shQuote(encodeString(e[1L]), type = "cmd")),
                                    paste0("\nmsgid_plural ", shQuote(encodeString(e[2L]), type = "cmd")), 
                                    "\nmsgstr[0]    \"", "\nmsgstr[1]    \""))
                un <- un[-match(e, un)]
            }
        }
    }
    
    # parse template to internal representation
    template <- structure(parse_template(template), domain = domain)
    
    # check if an existing template matches internal template representation
    if(template_exists(pkg = pkg)) {
        if(template_current(pkg = pkg, template = template)) {
            message(".pot template file has not changed")
        } else {
            write_template(template, domain = attributes(template)$domain)
        }
    } else {
        write_template(template, domain = attributes(template)$domain)
    }
    return(invisible(template))
}

template_exists <- function(pkg = ".", domain = "R") {
    pkg <- as.package(pkg)
    if(domain == "R")
        domain <- paste0("R-", pkg$package)
    else
        domain <- pkg$package
    template_file <- file.path(pkg$path, "po", paste0(domain, ".pot"))
    file.exists(template_file)
}

parse_template <- function(template) {
    h <- gsub("[\"]$", "", gsub("^[\"]", "", template[!grepl("msgid", template)]))
    h <- read.dcf(textConnection(h[!grepl("msgstr", h)]))
    out <- setNames(as.list(h), colnames(h))
    
    msgids <- template[grep("msgid", template)]
    msgids <- gsub("^\n", "", msgids)
    msgids <- sapply(strsplit(msgids, "\nmsgstr"), `[`, 1)
    msgids <- gsub("^msgid ", "", msgids)
    msgids <- gsub("^[\"]", "", msgids)
    msgids <- gsub("[\"]$", "", msgids)
    
    # need to generalize this so that each message is itself a list with
    # a "msgid" element and maybe "msgid_plural" and multiple "msgstr"'s
    
    out$msgids <- sort(msgids[msgids != ""])
    structure(out, class = "msgtemplate")
}

write_template <- function(template, pkg = ".", domain = "R") {
    pkg <- as.package(pkg)
    if(domain == "R")
        domain <- paste0("R-", pkg$package)
    else
        domain <- pkg$package
    po_dir <- file.path(pkg$path, "po")
    if(!file.exists(po_dir))
        make_po_dir(pkg = pkg)
    template_file <- file.path(po_dir, paste0(domain, ".pot"))
    out <- c("msgid \"\"\nmsgstr \"\"")
    msgids <- template$msgids
    template$msgids <- NULL
    out <- c(out, paste0("\n\"", names(template),": ", unname(unlist(template)), "\""))
    
    # need to generalize this so that each message is itself a list with
    # a "msgid" element and maybe "msgid_plural" and multiple "msgstr"'s
    
    out <- c(out, paste0("\n\nmsgid \"", unlist(msgids), "\"\nmsgstr \"\""))
    writeLines(out, con = template_file)
}

read_template <- function(file, pkg = ".", domain = "R"){
    if(missing(file)) {
        pkg <- as.package(pkg)
        if(domain == "R")
            domain <- paste0("R-", pkg$package)
        else
            domain <- pkg$package
        file <- file.path(pkg$path, "po", paste0(domain, ".pot"))
    }
    if(!file.exists(file))
        stop(".pot template file not found")
    con <- file(file, "r")
    on.exit(close(con))
    template <- readLines(con)
    parse_template(template)
}


template_current <- function(template, pkg = ".", domain = "R") {
    template_file <- read_template(pkg = pkg, domain = domain)
    if(missing(template))
        template <- use_translations(pkg = pkg, domain = domain)
    structure(identical(template, template_file), comparison = all.equal(template, template_file), template = template, template_file = template_file)
}


parse_translation <- function(translation) {
    h <- gsub("[\"]$", "", gsub("^[\"]", "", translation[!grepl("msgid", translation)]))
    h <- read.dcf(textConnection(h[!grepl("msgstr", h)]))
    out <- setNames(as.list(h), colnames(h))
    
    msgids <- translation[grep("msgid", translation)]
    msgids <- gsub("^\n", "", msgids)
    msgids <- sapply(strsplit(msgids, "\nmsgstr"), `[`, 1)
    msgids <- gsub("^msgid ", "", msgids)
    msgids <- gsub("^[\"]", "", msgids)
    msgids <- gsub("[\"]$", "", msgids)
    
    # need to generalize this so that each message is itself a list with
    # a "msgid" element and maybe "msgid_plural" and multiple "msgstr"'s
    
    out$msgids <- sort(as.list(msgids[msgids != ""]))
    structure(out, class = "msgtranslation")
}


read_translation <- function(file, language, pkg = ".", domain = "R") {
    if(domain == "R")
        domain_prefix <- "R-"
    else
        domain_prefix <- ""
    if(missing(file)) {
        pkg <- as.package(pkg)
        file <- file.path(pkg$path, "po", paste0(domain_prefix, language, ".po"))
    } else if(missing(language)) {
        language <- gsub(".po", "", basename(file))
        if(domain == "R")
            language <- gsub("R-", "", language)
    } else {
        stop("Either 'file' or 'language' is required")
    }
    if(!file.exists(file))
        stop(paste0(domain_prefix, language, ".po file not found"))
    con <- file(file, "r")
    on.exit(close(con))
    parse_translation(readLines(con))
}

write_translation <- function(translation, pkg = ".", domain = "R") {
    pkg <- as.package(pkg)
    lang <- strsplit(tolower(translation$Language),":",fixed = TRUE)[[1]][1]
    if(domain == "R")
        domain <- paste0("R-", lang)
    else
        domain <- lang
    po_dir <- file.path(pkg$path, "po")
    po_file <- file.path(po_dir, paste0(domain, ".po"))
    out <- c("msgid \"\"\nmsgstr \"\"")
    msgids <- translation$msgids
    translation$msgids <- NULL
    out <- c(out, paste0("\n\"", names(translation),": ", unname(unlist(translation)), "\""))
    
    # need to generalize this so that each message is itself a list with
    # a "msgid" element and maybe "msgid_plural" and multiple "msgstr"'s
    
    out <- c(out, paste0("\n\nmsgid \"", unlist(msgids), "\"\nmsgstr \"\""))
    writeLines(out, con = po_file)
}

translation_current <- function(language, file, template, pkg = ".", domain = "R") {
    if(missing(template)) {
        template <- read_template(pkg = pkg, domain = domain)
    }
    if(domain == "R")
        domain_prefix <- "R-"
    else
        domain_prefix <- ""
    if(missing(file)) {
        pkg <- as.package(pkg)
        file <- file.path(pkg$path, "po", paste0(domain_prefix, language, ".po"))
    } else if(missing(language)) {
        language <- gsub(".po", "", basename(file))
        if(domain == "R")
            language <- gsub("R-", "", language)
    } else {
        stop("Either 'file' or 'language' is required")
    }
    if(!file.exists(file))
        stop(paste0(domain_prefix, language, ".po file not found"))
    
    gettextpo <- read_translation(file)
    tran <- gettextpo$msgid
    
    structure(identical(template$msgid, tran), template = template, translation = gettextpo)
}

make_translation <- function(language, pkg = ".", domain = "R", write = TRUE) {
    pkg <- as.package(pkg)
    po_dir <- file.path(pkg$path, "po")
    if(!file.exists(po_dir)) {
        stop("No /po directory found for package")
    }
    if(domain == "R") {
        template_file <- file.path(po_dir, paste0("R-",pkg$package, ".pot"))
        po_file <- file.path(po_dir, paste0("R-", language, ".po"))
    } else {
        template_file <- file.path(po_dir, paste0(pkg$package, ".pot"))
        po_file <- file.path(po_dir, paste0(language, ".po"))
    }
    
    # check current translation
    template <- read_template(template_file)
    if(file.exists(po_file)) {
        current <- translation_current(file = po_file, template = template)
        if(current) {
            message(sprintf("%s translation already up-to-date", language))
            return(po_file, po = attributes(current)$gettextpo)
        } else {
            # update existing translation, leaving unchanged messages unchanged
            # translation <- read_translation()
            # add new messages
            if(write)
                write_translation(translation)
            return(structure(po_file, translation = template))
        }
    }
    
    template$Language <- paste(language, sep = ":")
    if(write)
        write_translation(template)
    structure(po_file, translation = template)
}

edit_translation <- function(language, file, pkg = ".", domain = "R", write = TRUE) {
    pkg <- as.package(pkg)
    po_dir <- file.path(pkg$path, "po")
    if(!file.exists(po_dir)) {
        make_po_dir(pkg = pkg)
    }
    update_template(pkg = pkg, domain = domain)
    
    if(domain == "R")
        domain_prefix <- "R-"
    else
        domain_prefix <- ""
    if(missing(file)) {
        file <- file.path(pkg$path, "po", paste0(domain_prefix, language, ".po"))
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
    m <- po$msgids
    e <- function(x) {
        message(paste0("Original message is: ", x))
        readline(gettext("Translation: "))
    }
    for(i in m) {
        m$msgstr <- e(m$msgid[i])
    }
    
    po_file <- if(write) write_translation(po) else ""
    structure(po_file, translation = po)
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

# print methods
print.msgtemplate <- function(x, ...){
    invisible(x)
}

print.msgtranslation <- function(x, ...){
    invisible(x)
}
