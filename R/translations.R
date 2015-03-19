make_po_dir <- function(pkg = ".") {
    pkg <- as.package(pkg)
    po_dir <- file.path(pkg$path, "po")
    dir.create(po_dir, FALSE)
    return(po_dir)
}

get_messages <- function(pkg = ".", type = "xgettext") {
    pkg <- as.package(pkg)
    if(type == "xgettext") {
        xgettext(pkg$path, asCall = FALSE)
    } else if (type == "xngettext") {
        xngettext(pkg$path)
    }
}

generate_pot <- 
function(pkg = ".", 
         created = format(Sys.time(), "%Y-%m-%d %H:%M"),
         maintainer = "",
         translator = "",
         team = "",
         charset = "UTF-8",
         domain = "R") {
    pkg <- as.package(pkg)
    
    pot <- 
    c("msgid \"\"", 
      "msgstr \"\"", 
      sprintf("\"Project-Id-Version: %s %s\\n\"", pkg$package, pkg$version), 
      sprintf("\"Report-Msgid-Bugs-To: %s\\n\"", maintainer), 
      paste0("\"POT-Creation-Date: ", created, "\\n\""), 
      paste0("\"PO-Revision-Date: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\\n\""), 
      paste0("\"Last-Translator: ", translator, "\\n\""), 
      paste0("\"Language-Team: ", team, "\\n\""), 
      "\"Language: LL \\n\"",
      "\"MIME-Version: 1.0\\n\"", 
      paste0("\"Content-Type: text/plain; charset=", charset, "\\n\""), 
      "\"Content-Transfer-Encoding: 8bit\\n\"")
    
    # run xgettext
    tmp <- unique(unlist(get_messages(pkg = pkg, type = "xgettext")))
    tmp <- tmp[nzchar(tmp)]
    if (length(tmp) > 0L) {
        tmp <- shQuote(encodeString(tmp), type = "cmd")
        pot <- c(pot, paste0("\nmsgid ",tmp,"\nmsgstr \"\""))
    }
    
    # run xngettext
    tmp <- get_messages(pkg = pkg, type = "xngettext")
    un <- unique(unlist(tmp, recursive = TRUE))
    for (ee in tmp) {
        for (e in ee) {
            if (e[1L] %in% un) {
               pot <- c(pot, paste0(paste0("\nmsgid        ", shQuote(encodeString(e[1L]), type = "cmd")),
                                    paste0("\nmsgid_plural ", shQuote(encodeString(e[2L]), type = "cmd")), 
                                    "\nmsgstr[0]    \"", "\nmsgstr[1]    \""))
                un <- un[-match(e, un)]
            }
        }
    }
    structure(parse_pot(pot), domain = domain)
}

pot_exists <- function(pkg = ".", domain = "R") {
    pkg <- as.package(pkg)
    if(domain == "R")
        domain <- paste0("R-", pkg$package)
    else
        domain <- pkg$package
    pot_file <- file.path(pkg$path, "po", paste0(domain, ".pot"))
    file.exists(pot_file)
}

parse_pot <- function(pot) {
    h <- gsub("[\"]$", "", gsub("^[\"]", "", p[!grepl("msgid", p)]))
    h <- read.dcf(textConnection(h[!grepl("msgstr", h)]))
    out <- setNames(as.list(h), colnames(h))
    
    msgids <- p[grep("msgid", p)]
    msgids <- gsub("^\n", "", msgids)
    msgids <- sapply(strsplit(msgids, "\nmsgstr"), `[`, 1)
    msgids <- gsub("^msgid ", "", msgids)
    msgids <- gsub("^[\"]", "", msgids)
    msgids <- gsub("[\"]$", "", msgids)
    
    # need to generalize this so that each message is itself a list with
    # a "msgid" element and maybe "msgid_plural" and multiple "msgstr"'s
    
    out$msgids <- sort(as.list(msgids[msgids != ""]))
    structure(out, class = "gettextpot")
}

write_pot <- function(pot, pkg = ".", domain = "R") {
    pkg <- as.package(pkg)
    if(domain == "R")
        domain <- paste0("R-", pkg$package)
    else
        domain <- pkg$package
    po_dir <- file.path(pkg$path, "po")
    if(!file.exists(po_dir))
        make_po_dir(pkg = pkg)
    pot_file <- file.path(po_dir, paste0(domain, ".pot"))
    out <- c("msgid \"\"\nmsgstr \"\"")
    msgids <- pot$msgids
    pot$msgids <- NULL
    out <- c(out, paste0("\n\"", names(pot),": ", unname(unlist(pot)), "\""))
    
    # need to generalize this so that each message is itself a list with
    # a "msgid" element and maybe "msgid_plural" and multiple "msgstr"'s
    
    out <- c(out, paste0("\n\nmsgid \"", unlist(msgids), "\"\nmsgstr \"\""))
    writeLines(out, con = pot_file)
}

read_pot <- function(file, pkg = ".", domain = "R"){
    if(missing(file)) {
        pkg <- as.package(pkg)
        if(domain == "R")
            domain <- paste0("R-", pkg$package)
        else
            domain <- pkg$package
        file <- file.path(pkg$path, "po", paste0(domain, ".pot"))
    }
    if(!file.exists(file))
        stop(".pot file not found")
    con <- file(file, "r")
    on.exit(close(con))
    pot <- readLines(con)
    parse_pot(pot)
}

update_pot_file <- 
function(pkg = ".", 
         created = format(Sys.time(), "%Y-%m-%d %H:%M"),
         maintainer, # object of class "person"
         translator, # object of class "person"
         team, # object of class "person"
         charset = "UTF-8",
         domain = "R" # or "C"
         ) {
    pkg <- as.package(pkg)
    po_dir <- file.path(pkg$path, "po")
    if(!file.exists(po_dir))
        po_dir <- make_po_dir(pkg = pkg)
    if(domain == "R")
        domain <- paste0("R-", pkg$package)
    else
        domain <- pkg$package
    pot_file <- file.path(pkg$path, "po", paste0(domain, ".pot"))
    
    # generate internal gettextpot representation (calling `xgettext` and `xngettext`)
    pot <- generate_pot(pkg = pkg$package, 
                        created = created, 
                        maintainer = if(!missing(maintainer)) format(maintainer) else pkg$maintainer, 
                        translator = if(!missing(translator)) format(translator) else NULL,
                        if(!missing(team)) format(team) else NULL,
                        charset = charset,
                        domain = domain)
    
    if(pot_exists(pkg = pkg) && pot_current(pkg = pkg, pot = pot)) {
        message(".pot file has not changed")
    } else {
        write_pot(pot, domain = attributes(pot)$domain)
    }
    return(pot_file)
}

pot_current <- function(pot, pkg = ".", domain = "R") {
    pot_file <- read_pot(pkg = pkg, domain = domain)
    if(missing(pot))
        pot <- generate_pot(pkg = pkg, domain = domain)
    structure(identical(pot, pot_file), comparison = all.equal(pot, pot_file), pot = pot, pot_file = pot_file)
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
    structure(readLines(con), class = "gettextpo")
}

write_translation <- function(po, pkg = ".", domain = "R") {
    pkg <- as.package(pkg)
    lang <- strsplit(tolower(po$Language),":",fixed = TRUE)[[1]][1]
    if(domain == "R")
        domain <- paste0("R-", lang)
    else
        domain <- lang
    po_dir <- file.path(pkg$path, "po")
    po_file <- file.path(po_dir, paste0(domain, ".po"))
    out <- c("msgid \"\"\nmsgstr \"\"")
    msgids <- po$msgids
    po$msgids <- NULL
    out <- c(out, paste0("\n\"", names(po),": ", unname(unlist(po)), "\""))
    
    # need to generalize this so that each message is itself a list with
    # a "msgid" element and maybe "msgid_plural" and multiple "msgstr"'s
    
    out <- c(out, paste0("\n\nmsgid \"", unlist(msgids), "\"\nmsgstr \"\""))
    writeLines(out, con = po_file)
}

translation_current <- function(language, file, pot, pkg = ".", domain = "R") {
    if(missing(pot)) {
        pot <- read_pot(pkg = pkg, domain = domain)
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
    
    structure(identical(pot$msgid, tran), pot = pot, translation = gettextpo)
}

make_translation <- function(language, pkg = ".", domain = "R", write = TRUE) {
    pkg <- as.package(pkg)
    po_dir <- file.path(pkg$path, "po")
    if(!file.exists(po_dir)) {
        stop("No /po directory found for package")
    }
    if(domain == "R") {
        pot_file <- file.path(po_dir, paste0("R-",pkg$package, ".pot"))
        po_file <- file.path(po_dir, paste0("R-", language, ".po"))
    } else {
        pot_file <- file.path(po_dir, paste0(pkg$package, ".pot"))
        po_file <- file.path(po_dir, paste0(language, ".po"))
    }
    
    # check current translation
    pot <- read_pot(pot_file)
    if(file.exists(po_file)) {
        current <- translation_current(file = po_file, pot = pot)
        if(current) {
            message("Translation already up-to-date")
            return(po_file, po = attributes(current)$gettextpo)
        }
    }
    
    pot$Language <- paste(language, sep = ":")
    if(write) # optionally write to disk
        write_translation(pot)
    structure(po_file, po = pot)
}

edit_translation <- function(language, file, pkg = ".", domain = "R", write = TRUE) {
    pkg <- as.package(pkg)
    po_dir <- file.path(pkg$path, "po")
    if(!file.exists(po_dir)) {
        make_po_dir(pkg = pkg)
    }
    update_pot_file(pkg = pkg, domain = domain)
    
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
            tmp <- make_translation(language = lanugage, pkg = pkg, domain = domain, write = FALSE)
            po <- attributes(tmp)$po
        } else {
            po <- read_translation(file = file)
        }
    } else {
        po <- make_translation(language = language, pkg = pkg, domain = domain, write = FALSE)
    }
    
    # write a command-line interface to update po translation here
    stop("This feature is not yet supported. Sorry!")
    
    po_file <- if(write) write_translation(pot) else ""
    structure(po_file, po = po)
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
    pot_file <- file.path("po", paste0("R-", pkg$package, ".pot"))
    
    if(check & missing(which))
        check_translations(pkg = pkg)
    
    # R-level messages
    po_files <- dir("po", pattern = "R-.*[.]po$", full.names = TRUE)
    po_files <- po_files[po_files != "po/R-en@quot.po"]
    
    for (f in po_files) {
        lang <- sub("^R-(.*)[.]po$", "\\1", basename(f))
        message("  R-", lang, ":", appendLF = FALSE, domain = NA)
        if (system(paste("msgmerge --update", f, shQuote(pot_file))) != 0L) {
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
        en_quote(potfile, f)
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
        if (system(paste("msgmerge --update", shQuote(f), shQuote(pot_file))) != 0L) {
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
        en_quote(pot_file, f)
        dest <- file.path(stem, lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("%s.mo", dom))
        cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))
        if (system(cmd) != 0L) 
            warning(sprintf("running msgfmt on %s failed", basename(f)), domain = NA)
    }
}

# print methods
print.gettextpot <- function(x, ...){
    invisible(x)
}

print.gettextpo <- function(x, ...){
    invisible(x)
}


# toggles to check
setlang <- function(language) {
    options("msgtools_LANG" = Sys.getenv("LANG"))
    Sys.setenv(LANG = language)
}

resetlang <- function(language) {
    if(missing(language))
        Sys.setenv(LANG = getOptions("msgtools_LANG", "EN"))
    else
        Sys.setenv(LANG = language)
}
