`%n%` <- function (x, y)  if (!is.null(x)) x else y

#' @name templates
#' @title Handle message templates (.pot files)
#' @description Read, write, and generate .pot diagnostic message templates
#' @param charset A character string specifying the character set of the translation template file.
#' @template pkg
#' @template template
#' @template verbosity
#' @template domain
#' @details \code{read_template} and \code{write_template} provide basic input and output functionality for translation template (.pot) files. If called from with an R package directory, the locations of these fies are identified automatically.
#' 
#' \code{make_template} generates a new template in memory, without writing it to disk. \code{sync_template} makes a new template and writes it to disk or, if a template file already exists, overwrites it.
#'
#' \code{sync_template()} updates the template file. \code{\link{sync_translations}} further updates translation files against that template.
#' 
#' @return \code{make_template} and \code{read_template} reutrn an R6 object of class \dQuote{po}. \code{write_template} returns the path to the file, invisibly.
#' @author Thomas J. Leeper
#' @examples
#' pkg <- dummy_pkg()
#'
#' # check for existing template
#' try(template_exists(pkg = pkg))
#' 
#' # generate an in-memory template
#' pot <- make_template(pkg = pkg)
#' write_template(pot, pkg = pkg)
#'   
#' @seealso \code{\link{get_messages}} to read messages into memory without creating a template file, \code{\link{use_localization}} to setup a package for localization (including generation of a template file)
#' @importFrom tibble tibble
#' @importFrom digest digest
#' @import poio
#' @export
make_template <- 
function(charset = "UTF-8",
         pkg = ".", 
         domain = "R") {
    
    pkg <- as.package(pkg)
    
    if (!domain %in% c("R", "r")) {
        stop("Currently only the 'R' domain is supported")
    }
    
    msgs <- get_messages(pkg = pkg)
    msgs <- msgs[msgs[["msgid"]] != "", ]
    direct <- msgs[msgs[["type"]] == "direct",]
    direct <- direct[!duplicated(direct[["msgid"]]), ]
    countable <- msgs[msgs[["type"]] == "countable",]
    countable <- countable[!duplicated(countable[["msgid"]]), ]
    
    # This is sometimes NULL, and we need it to have length 1 when
    # creating the tibble later in this function
    bugreports <- pkg[["bugreports"]] %n% " "
    
    out <- 
    po(source_type = tolower(domain),
       file_type = "pot",
       initial_comments = character(),
       metadata = tibble(name = c("Project-Id-Version",
                                  "Report-Msgid-Bugs-To", 
                                  "POT-Creation-Date", 
                                  "PO-Revision-Date",
                                  "Last-Translator",
                                  "Language-Team",
                                  "Language", 
                                  "MIME-Version",
                                  "Content-Type",
                                  "Content-Transfer-Encoding"),
                         value = c(paste(pkg[["package"]], pkg[["version"]]),
                                   bugreports, 
                                   format(Sys.time(), "%Y-%m-%d %H:%M"),
                                   format(Sys.time(), "%Y-%m-%d %H:%M"), 
                                   " ", 
                                   " ", 
                                   "LL", 
                                   "1.0", 
                                   paste0("text/plain; charset=", charset),
                                   "8bit")),
       direct = tibble(msgid = direct[["msgid"]],
                       msgstr = character(nrow(direct)),
                       is_obsolete = rep(FALSE, nrow(direct)),
                       msgctxt = rep(list(character(0)), nrow(direct)),
                       translator_comments = rep(list(character(0)), nrow(direct)),
                       source_reference_comments = rep(list(character(0)), nrow(direct)),
                       flags_comments = rep(list(character(0)), nrow(direct)),
                       previous_string_comments = rep(list(character(0)), nrow(direct)),
                       msgkey = character(nrow(direct))
                      ),
       countable = tibble(msgid = countable[["msgid"]],
                          msgid_plural = countable[["msgid_plural"]],
                          msgstr = rep(list(character(0)), nrow(countable)),
                          is_obsolete = rep(FALSE, nrow(countable)),
                          msgctxt = character(nrow(countable)),
                          translator_comments = rep(list(character(0)), nrow(countable)),
                          source_reference_comments = rep(list(character(0)), nrow(countable)),
                          flags_comments = rep(list(character(0)), nrow(countable)),
                          previous_string_comments = rep(list(character(0)), nrow(countable)),
                          msgkey = character(nrow(countable))
                         )
    )
    if (nrow(out[["direct"]]) > 0) {
        out[["direct"]][["msgkey"]] <- digest(paste(out[["direct"]][["msgid"]], out[["direct"]][["msgctext"]]), algo = "xxhash32")
    }
    if (nrow(out[["countable"]]) > 0) {
        out[["countable"]][["msgkey"]] <- digest(paste(out[["countable"]][["msgid"]], out[["countable"]][["msgctext"]]), algo = "xxhash32")
    }
    out
}

#' @rdname templates
#' @export
read_template <- function(pkg = ".", domain = "R"){
    if (!template_exists(pkg = pkg, domain = domain)) {
        stop("Template (.pot) file not found!")
    }
    pot_file <- template_path(pkg = pkg, domain = domain)
    pot <- read_po(pot_file)
    translator <- pot[["metadata"]][["value"]][pot[["metadata"]][["name"]] == "Last-Translator"]
    fix_metadata(pot, pkg = pkg, .dots = list("Last-Translator" = translator))
}

#' @rdname templates
#' @export
write_template <- function(template, pkg = ".", verbose = getOption("verbose")) {
    pkg <- as.package(pkg)
    make_po_dir(pkg = pkg, verbose = verbose)
    if (isTRUE(verbose)) {
       message("Writing the template (.pot) file")
    }
    pot_file <- template_path(pkg = pkg, domain = template[["source_type"]])
    write_po(template, pot_file)
    return(invisible(pot_file))
}

#' @rdname templates
#' @export
template_exists <- function(pkg = ".", domain = "R") {
    pkg <- as.package(pkg)
    pot_file <- template_path(pkg = pkg, domain = domain)
    if (!file.exists(pot_file)) {
        return(FALSE)
    }
    return(TRUE)
}

#' @rdname templates
#' @export
sync_template <- 
function(charset = "UTF-8", 
         pkg = ".", 
         domain = "R",
         verbose = getOption("verbose")) {
    no_template <- !template_exists(pkg = pkg, domain = domain)
    template <- make_template(charset = charset, pkg = pkg, domain = domain)
    if (isTRUE(no_template)) {
        write_template(template, pkg = pkg, verbose = verbose)
    } else if (!isTRUE(no_template)) {
        current <- template_current(template = template, pkg = pkg, domain = domain)
        if (isTRUE(current)) {
            invisible(template_path(pkg = pkg, domain = domain))
        } else {
            write_template(template, pkg = pkg, verbose = verbose)
        }
    }
}

#' @rdname templates
#' @export
template_current <- function(template, pkg = ".", domain = "R") {
    template_from_file <- read_template(pkg = pkg, domain = domain)
    identical(list(template[["direct"]], template[["countable"]]),
              list(template_from_file[["direct"]], template_from_file[["countable"]]))
}
