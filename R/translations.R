#' @rdname translations
#' @title Handle message translations (.po files)
#' @description Read, write, and generate translations of diagnostic messages
#' @param language A character string specifying a language.
#' @param translator A character string the name and email of a translation of the form \code{First Last <email@example.com>}.
#' @param team Optionally, a character string specifying contact information for a \dQuote{translation team}.
#' @template pkg
#' @template domain
#' @template template
#' @details \code{read_translation} and \code{write_translation} provide basic input and output functionality for translation (.po) files. If called from with an R package directory, the locations of these fies are identified automatically. \code{\link[poio]{read_po}} provides a lower-level interface for reading a specific file.
#' 
#' \code{make_translation} creates a \code{"po"} translation object from a message template (.pot) file (if one does not exist, it is created). The \code{language} and \code{translator} arguments are mandatory. \code{language} must be an allowed language code (see \code{\link[poio]{language_codes}}); the \dQuote{Plural-Forms} metadata field is generated automatically from the language value (see \code{\link[poio]{plural_forms}}).
#' 
#' \code{\link{edit_translation}} is a very simple interactive interface for editing a translation object in memory.
#' 
#' @return \code{make_translation} and \code{read_translation} reutrn an object of class \dQuote{po}. \code{write_translation} returns the path to the file, invisibly.
#' @author Thomas J. Leeper
#' @examples
#' \dontrun{
#'   # setup pkg for localization
#'   use_localization()
#'   
#'   # generate translation in memory
#'   (tran <- make_translation("es", translator = "Some Person <example@examle.com>"))
#'   # write to disk
#'   write_translation(tran)
#' }
#' @seealso \code{\link{make_template}}, \code{\link{use_localization}}, \code{\link{edit_translation}}
#' @import poio
#' @export
read_translation <- function(language, domain = "R", pkg = ".") {
    pkg <- as.package(pkg)
    file <- translation_path(pkg = pkg, language = language, domain = domain)
    fix_metadata(read_po(file))
}

#' @rdname translations
#' @export
write_translation <- function(translation, pkg = ".") {
    pkg <- as.package(pkg)
    make_po_dir(pkg = pkg)
    language <- translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "Language"]
    po_file <- translation_path(pkg = pkg, 
                                language = language, 
                                domain = translation[["source_type"]])
    write_po(translation, po_file)
    return(invisible(po_file))
}

#' @rdname translations
#' @export
make_translation <- 
function(language, 
         translator,
         team = " ",
         pkg = ".", 
         domain = "R") {
    pkg <- as.package(pkg)
    
    check_language_regex(language)
    
    template_file <- template_path(pkg = pkg, domain = domain)
    po_file <- translation_path(pkg = pkg, language = language, domain = template[["source_type"]])
    
    # check for template
    if (file.exists(template_file)) {
        template <- read_template(pkg = pkg, domain = domain)
    } else {
        template <- make_template(pkg = pkg, domain = domain)
        message("Writing template (.pot) file to disk")
        write_template(template, pkg = pkg)
    }
    
    # check for translation
    if (file.exists(po_file)) {
        translation <- read_translation(language = language, domain = domain, pkg = pkg)
        oldtranslator <- translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "Last-Translator"]
        if (oldtranslator != translator) {
            translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "Last-Translator"] <- translator
        }
        oldteam <- translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "Language-Team"]
        if (oldteam != team) {
            translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "Language-Team"] <- team
        }
    } else {
        translation <- template
        translation[["file_type"]] <- "po"
        ## translator
        translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "Last-Translator"] <- translator
        ## language team
        translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "Language-Team"] <- team
        ## language
        translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "Language"] <- language
        ## plural forms
        data(plural_forms)
        plural <- plural_forms[["PluralFormHeader"]][plural_forms[["ISO"]] == language]
        translation[["metadata"]] <- rbind(translation[["metadata"]], c("Plural-Forms", plural))
        
        # need to setup structure of `countable$msgstr` list to reflect plural forms
        
    }
    
    # update revision date
    translation[["metadata"]][["value"]][translation[["metadata"]][["name"]] == "PO-Revision-Date"] <- format(Sys.time(), "%Y-%m-%d %H:%M")
    
    return(translation)
}

#' @rdname translations
#' @export
translation_current <- function(language, template, pkg = ".", domain = "R") {
    translation <- read_translation(language = language, domain = domain, pkg = pkg)
    if (missing(template)) {
        template <- read_template(pkg = pkg, domain = domain)
    }
    structure(identical(translation, template))
}

translation_path <- function(pkg = ".", language, domain = "R") {
    pkg <- as.package(pkg)
    file.path(pkg$path, "po", paste0(if(domain %in% c("R", "r")) "R-" else NULL, language, ".po"))
}
