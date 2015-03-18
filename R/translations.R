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

# function to build an internal xgettextpot representation
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
    if (length(tmp) > 0L) 
        tmp <- shQuote(encodeString(tmp), type = "cmd")
    pot <- c(pot, paste0("\nmsgid \"",tmp,"\"\nmsgstr \"\""))
    
    # run xngettext
    tmp <- get_messages(pkg = pkg, type = "xngettext")
    un <- unique(unlist(tmp, recursive = TRUE))
    for (ee in tmp) {
        for (e in ee) {
            if (e[1L] %in% un) {
               pot <- c(pot, paste0(paste0("\nmsgid        ", shQuote(encodeString(e[1L]), type = "cmd")),
                                    paste0("\nmsgid_plural ", shQuote(encodeString(e[2L]), type = "cmd")), 
                                    "\nmsgstr[0]    \"\"", "\nmsgstr[1]    \"\""))
                un <- un[-match(e, un)]
            }
        }
    }
    structure(pot, domain = "R", class = "gettextpot")
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
    con <- file(pot_file, "wt")
    on.exit(close(con))
    writeLines(pot, con = con)
}

pot_exists <- function(pkg = ".", domain = "R") {
    pkg <- as.package(pkg)
    if(domain == "R")
        domain <- paste0("R-", pkg$package)
    else
        domain <- pkg$package
    pot_file <- file.path(pkg$path, "po", paste0(domain, ".pot"))
    file.exists(file)
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
    structure(readLines(con), class = "gettextpot")
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
    
    if(pot_exists(pkg = pkg) && check_pot_changed(pkg = pkg, pot = pot)) {
        message(".pot file has not changed")
    } else {
        # write .pot from internal representation
        write_pot(pot, domain = attributes(pot)$domain)
    }
    return(pot_file)
}

check_pot_changed <- function(pot, pkg = ".", domain = "R") {
    # check existing .pot file
    pot_file <- read_pot(pkg = pkg, domain = domain)
    msg1 <- grep(pot_file, "msgid")[2]
    
    # check working gettextpot
    if(missing(pot))
        pot <- generate_pot(pkg = pkg, domain = domain)
    msg2 <- grep(pot, "msgid")[2]
    
    # compare (return TRUE if changed)
    !identical(pot_file[msg1:length(pot_file)], pot[msg2:length(pot)])
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

# check whether a .po file is up-to-date with the .pot
translation_current <- function(file, language, pot, pkg = ".", domain = "R") {
    # check existing .pot file
    if(missing(pot)) {
        pot <- read_pot(pkg = pkg, domain = domain)
    }
    pot <- sort(pot[grep("msgid", pot)])
    
    # check translation .po
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
    tran <- sort(gettextpo[grep("msgid", gettextpo)])
    
    # compare (return TRUE if identical)
    structure(identical(pot, tran), gettextpo = gettextpo)
}

make_translation <- function(language, pkg = ".", domain = "R") {
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
            return(attributes(current)$gettextpo)
        }
    }
    
    lang <- grep("language:", tolower(pot), fixed = TRUE)
    lpaste <- paste0("\"Language: ", paste(language, sep = ":"), "\\n\"")
    if(length(lang)) {
        po <- c(pot[1:(lang-1)], lpaste, pot[(lang+1):length(pot)])
    } else {
        lteam <- grep("language-team:", tolower(pot), fixed = TRUE)
        po <- c(pot[1:lteam], lpaste, pot[(lteam+1):length(pot)])
    }
        
    con <- file(po_file, "wt")
    on.exit(close(con))
    writeLines(text = po, con = con)
    structure(po, file = po_file, class = "gettextpo")
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
