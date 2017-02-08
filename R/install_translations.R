#' @rdname install
#' @title Install translations
#' @description Check and install message translations
#' @param language A character string specifying a language.
#' @template domain
#' @template pkg
#' @param strictPlural A logical indicating whether to compare formats of singular and plural forms in a strict way. See \code{\link[tools]{checkPoFile}}.
#' @template verbosity
#' @details \code{check_translation} checks a specific translation file. \code{check_translations} checks all translations in the /po directory. These are just wrappers around \code{\link[tools]{checkPoFile}}.
#' 
#' \code{install_translations} performs a reduced set of the functionality described in \code{\link[tools]{update_pkg_po}}.
#'
#' @return \code{check_translation} returns an object of class \dQuote{check_po_files}; \code{check_translations} returns a list of such objects. \code{install_translations} returns a logical TRUE, if successful.
#' @note These functions require that gettext is installed on your system.
#' @examples
#' \dontrun{
#'   # check translations
#'   check_translations()
#'   
#'   # install translations
#'   install_translations()
#' }
#' @seealso \code{\link{use_localization}}, \code{\link{make_translation}}
#' @importFrom tools checkPoFile
#' @export
check_translation <- 
function(language, pkg = ".", domain = "R", strictPlural = FALSE, verbose = getOption("verbose")) {
    pkg <- as.package(pkg)
    po_file <- translation_path(pkg = pkg, language = language, domain = domain)
    if (isTRUE(verbose)) {
        message(sprintf("Checking %s translation file", basename(po_file)))
    }
    checkPoFile(po_file, strictPlural = strictPlural)
}

#' @rdname install
#' @export
check_translations <- function(pkg = ".", strictPlural = FALSE, verbose = getOption("verbose")) {
    pkg <- as.package(pkg)
    languages <- dir(file.path(pkg$path, "po"), pattern = "\\.po$", full.names = FALSE)
    languages <- gsub("\\.po$", "", languages)
    rdomain <- grepl("^[R][-]", languages)
    languages2 <- gsub("^[R][-]", "", languages)
    out1 <- lapply(languages2[rdomain], check_translation, pkg = pkg, domain = "R", strictPlural = strictPlural, verbose = verbose)
    out2 <- lapply(languages2[!rdomain], check_translation, pkg = pkg, domain = "C", strictPlural = strictPlural, verbose = verbose)
    out <- c(out1, out2)
    names(out) <- c(languages[rdomain], languages[!rdomain])
    out
}

#' @rdname install
#' @export
install_translations <- function(pkg = ".", verbose = getOption("verbose")) {
    pkg <- as.package(pkg)
    po_dir <- file.path(pkg$path, "po")
    
    # setup inst/po directory
    inst_po_dir <- file.path(pkg$path, "inst", "po")
    if (!dir.exists(inst_po_dir)) {
        if (isTRUE(verbose)) {
            message(sprintf("Creating the directory", "'inst/po'"))
        }
        dir.create(inst_po_dir, FALSE, TRUE)
    }
    
    # R-level messages
    po_files <- dir(po_dir, pattern = "R-.*[.]po$", full.names = TRUE)
    po_files <- po_files[po_files != "po/R-en@quot.po"]
    
    ## install R-level messages
    for (f in po_files) {
        lang <- sub("^R-(.*)[.]po$", "\\1", basename(f))
        if (isTRUE(verbose)) {
            message("Installing R-", lang, " translation")
        }
        dest <- file.path(inst_po_dir, lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("R-%s.mo", pkg$package))
        if (system(paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))) != 0L) 
            warning(sprintf("running msgfmt on %s failed", basename(f)), immediate. = TRUE)

        if (l10n_info()[["UTF-8"]]) {
          lang <- "en@quot"
              dest <- file.path(inst_po_dir, lang, "LC_MESSAGES")
          dir.create(dest, FALSE, TRUE)
          dest <- file.path(dest, sprintf("R-%s.mo", pkg$package))
          cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))
          if (system(cmd) != 0L)
            warning(sprintf("running msgfmt on %s failed", basename(f)), immediate. = TRUE)
        }
      }
    
    # C-level messages
    po_files <- dir(po_dir, pattern = "^[^R].*[.]po$", full.names = TRUE)
    po_files <- po_files[po_files != "po/en@quot.po"]
    
    ## install C-level messages
    for (f in po_files) {
        lang <- sub("[.]po", "", basename(f))
        if (isTRUE(verbose)) {
            message("Installing ", lang, " translation")
        }
        dest <- file.path(inst_po_dir, lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, paste0("R-", pkg$package, ".mo"))
        if (system(paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))) != 0L) 
            warning(sprintf("running msgfmt on %s failed", basename(f)))

        if (l10n_info()[["UTF-8"]]) {
          lang <- "en@quot"
          dest <- file.path(inst_po_dir, lang, "LC_MESSAGES")
          dir.create(dest, FALSE, TRUE)
          dest <- file.path(dest, sprintf("%s.mo", pkg$package))
          cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))
          if (system(cmd) != 0L)
            warning(sprintf("running msgfmt on %s failed", basename(f)))
        }
      }
    return(invisible(TRUE))
}

