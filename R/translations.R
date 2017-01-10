#' @rdname translations
#' @title Handle message translations (.po files)
#' @description Read, write, and generate translations of diagnostic messages
#' @param translation An object of class \code{"po"} containing a message translation.
#' @template language
#' @param translator A character string the name and email of a translation of the form \code{First Last <email@example.com>}.
#' @param team Optionally, a character string specifying contact information for a \dQuote{translation team}.
#' @template pkg
#' @template domain
#' @template verbosity
#' @details \code{read_translation} and \code{write_translation} provide basic input and output functionality for translation (.po) files. If called from with an R package directory, the locations of these fies are identified automatically. \code{\link[poio]{read_po}} provides a lower-level interface for reading a specific file.
#' 
#' The behavior of \code{make_translation} depends on context. If the requested translation already exists, it is updated against the template (.pot) file and loaded into memory. If the translation does not already exist, the function creates a \code{"po"} translation object from the message template (.pot) file (if one does not exist, it is created). The \code{language} and \code{translator} arguments are mandatory in the latter case and only used to update values in an existing file if they differ from the existing values. \code{language} must be an allowed language code (see \code{\link[poio]{language_codes}}); the \dQuote{Plural-Forms} metadata field is generated automatically from the language value (see \code{\link[poio]{plural_forms}}).
#' 
#' \code{sync_translations()} updates the template file (via \code{\link{sync_template}} and then updates existing translation files against it.
#' 
#' \code{\link{edit_translation}} is a very simple interactive interface for editing a translation object in memory.
#' 
#' @return \code{make_translation} and \code{read_translation} reutrn an object of class \dQuote{po}. \code{write_translation} returns the path to the file, invisibly.
#' @note These functions require that gettext is installed on your system.
#' @author Thomas J. Leeper
#' @examples
#' \dontrun{
#'   # create example package
#'   pkg <- dummy_pkg()
#'   
#'   # setup pkg for localization
#'   use_localization(pkg)
#'   
#'   # generate Portugal Portugese translation in memory
#'   make_translation("pt_PT", translator = "Some Person <example@example.com>")
#'   # generate Spanish translation in memory
#'   (tran <- make_translation("es", translator = "Some Person <example@example.com>"))
#'   # write to disk
#'   write_translation(tran)
#' }
#' @seealso \code{\link{make_template}}, \code{\link{use_localization}}, \code{\link{edit_translation}}
#' @import poio
#' @export
read_translation <- function(language, pkg = ".", domain = "R") {
    pkg <- as.package(pkg)
    if (!translation_exists(language = language, pkg = pkg, domain = domain)) {
        stop("Translation (.po) file not found!")
    }
    po_file <- translation_path(pkg = pkg, language = language, domain = domain)
    translation <- read_po(po_file)
    translator <- translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "Last-Translator"]
    fix_metadata(translation, pkg = pkg, .dots = list("Last-Translator" = translator))
}

#' @rdname translations
#' @export
write_translation <- function(translation, pkg = ".", verbose = getOption("verbose")) {
    pkg <- as.package(pkg)
    make_po_dir(pkg = pkg)
    language <- translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "Language"]
    po_file <- translation_path(pkg = pkg, 
                                language = language, 
                                domain = translation[["source_type"]])
    if (isTRUE(verbose)) {
        if (file.exists(po_file)) {
            message("Overwriting existing translation (.po) file ", basename(po_file))
        } else {
            message("Writing translation (.po) file ", basename(po_file))
        }
    }
    
    # update revision date
    translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "PO-Revision-Date"] <- format(Sys.time(), "%Y-%m-%d %H:%M")
    
    write_po(translation, po_file)
    return(invisible(po_file))
}

#' @rdname translations
#' @importFrom utils data
#' @export
make_translation <- 
function(language, 
         translator,
         team = " ",
         pkg = ".", 
         domain = "R",
         verbose = getOption("verbose")) {
    
    pkg <- as.package(pkg)
    
    check_language_regex(language)
    
    template_file <- template_path(pkg = pkg, domain = domain)
    
    # check for template
    if (file.exists(template_file)) {
        template <- read_template(pkg = pkg, domain = domain)
    } else {
        sync_template(pkg = pkg, domain = domain, verbose = verbose)
        template <- read_template(pkg = pkg, domain = domain)
    }
    
    po_file <- translation_path(pkg = pkg, language = language, domain = template[["source_type"]])
    
    # check for translation
    if (file.exists(po_file)) {
        
        sync_translation(language = language, pkg = pkg, domain = domain, verbose = verbose)
        
        translation <- read_translation(language = language, domain = domain, pkg = pkg)
        oldtranslator <- translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "Last-Translator"]
        if (!missing(translator)) {
            if (oldtranslator != translator) {
                if (isTRUE(verbose)) {
                    message("Overwriting existing translator ", oldtranslator, " to ", translator)
                }
                translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "Last-Translator"] <- translator
            }
        }
        oldteam <- translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "Language-Team"]
        if (team != " ") {
            if (oldteam != team) {
                if (isTRUE(verbose)) {
                    message("Overwriting existing translation team ", oldteam, " to ", team)
                }
                translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "Language-Team"] <- team
            }
        }
    } else {
        
        if (isTRUE(verbose)) {
            message("Generating translation from template")
        }
        
        translation <- generate_po_from_pot(template, lang = language)
        
        ## translator
        translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "Last-Translator"] <- translator
        ## language team
        translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "Language-Team"] <- team
        
    }
    
    # update revision date
    translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "PO-Revision-Date"] <- format(Sys.time(), "%Y-%m-%d %H:%M")
    
    return(translation)
}

sync_translation <- 
function(language, 
         pkg = ".", 
         domain = "R",
         verbose = getOption("verbose")) {
    
    pkg <- as.package(pkg)
    
    check_language_regex(language)
    sync_template(pkg = pkg, domain = domain, verbose = verbose)
    
    template_file <- template_path(pkg = pkg, domain = domain)
    po_file <- translation_path(pkg = pkg, language = language, domain = domain)
    
    if (isTRUE(verbose)) {
        message("Updating existing translation (.po) file ", basename(po_file))
    }
    cmd <- paste("msgmerge --update", shQuote(po_file), shQuote(template_file))
    if (system(cmd) != 0L) {
        warning("running msgmerge on ", po_file, " failed", domain = NA)
    }
    return(invisible(TRUE))
}

#' @rdname translations
#' @export
sync_translations <- 
function(pkg = ".", 
         verbose = getOption("verbose")) {
    
    pkg <- as.package(pkg)
    
    sync_template(pkg = pkg, domain = "R", verbose = TRUE)
    #sync_template(pkg = pkg, domain = "C", verbose = TRUE)
    
    languages <- dir(file.path(pkg$path, "po"), pattern = "\\.po$", full.names = FALSE)
    languages <- gsub("\\.po$", "", languages)
    rdomain <- grepl("^[R][-]", languages)
    languages2 <- gsub("^[R][-]", "", languages)
    
    languages2 <- gsub("^[R][-]", "", languages)
    out1 <- lapply(languages2[rdomain], sync_translation, pkg = pkg, domain = "R", verbose = verbose)
    out2 <- lapply(languages2[!rdomain], sync_translation, pkg = pkg, domain = "C", verbose = verbose)
    
    return(invisible(TRUE))
}

translation_exists <- function(language, pkg = ".", domain = "R") {
    pkg <- as.package(pkg)
    po_file <- translation_path(language = language, pkg = pkg, domain = domain)
    if (!file.exists(po_file)) {
        return(FALSE)
    }
    return(TRUE)
}
