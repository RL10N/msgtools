`%n%` <- function (x, y)  if (!is.null(x)) x else y

#' @name templates
#' @title Handle message templates (.pot files)
#' @description Read, write, and generate .pot diagnostic message templates
#' @template pkg
#' @template template
#' @param charset A character string specifying the character set of the translation template file.
#' @param verbose Logical. Should the function be chatty?
#' @template domain
#' @details \code{read_template} and \code{write_template} provide basic input and output functionality for translation template (.pot) files. If called from with an R package directory, the locations of these fies are identified automatically (see \code{\link{make_po_dir}}).
#' 
#' \code{make_template} generates a new template in memory, without writing it to disk. \code{sync_template} makes a new template and writes it to disk or, if a template file already exists, overwrites it.
#' @return \code{make_template} and \code{read_template} reutrn an object of class \dQuote{po}. \code{write_template} returns the path to the file, invisibly.
#' @author Thomas J. Leeper
#' @examples
#' # check for existing template
#' pkg <- extract_example_pkg()
#' try(check_for_template(pkg))
#' 
#' # generate an in-memory template
#' pot <- make_template(pkg)
#' write_template(pot)
#'   
#' # setup a package for translating
#' use_localization(pkg)
#' }
#' @seealso \code{\link{get_messages}} to read messages into memory without creating a template file, \code{\link{use_localization}} to setup a package for localization (including generation of a template file)
#' @importFrom tibble tibble
#' @import poio
#' @export
make_template <- 
function(charset = "UTF-8",
         pkg = ".", 
         domain = "R") {
    
    pkg <- as.package(pkg)
    
    if (domain != "R") {
        stop("Currently only the R domain is implemented")
    }
    
    msgs <- get_messages(pkg = pkg)
    direct <- msgs[msgs[["type"]] == "direct",]
    countable <- msgs[msgs[["type"]] == "countable",]
    
    # This is sometimes NULL, and we need it to have length 1 when
    # creating the tibble later in this function
    bugreports <- pkg[["bugreports"]] %n% " "
    
    structure(list(
              source_type = tolower(domain),
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
                              msgctxt = character(nrow(direct)),
                              translator_comments = rep(NA_character_, nrow(direct)),
                              source_reference_comments = rep(list(list()), nrow(direct)),
                              flags_comments = rep(list(list()), nrow(direct)),
                              previous_string_comments = rep(list(list()), nrow(direct))
                              ),
              countable = tibble(msgid = countable[["msgid"]],
                                 msgid_plural = countable[["msgid_plural"]],
                                 msgstr = rep(list(list()), nrow(countable)),
                                 is_obsolete = rep(FALSE, nrow(countable)),
                                 msgctxt = character(nrow(countable)),
                                 translator_comments = rep(NA_character_, nrow(countable)),
                                 source_reference_comments = rep(list(list()), nrow(countable)),
                                 flags_comments = rep(list(list()), nrow(countable)),
                                 previous_string_comments = rep(list(list()), nrow(countable))
                                 )
              ), class = c("po", "list"))
}

#' @rdname templates
#' @export
sync_template <- function(charset = "UTF-8", pkg = ".", domain = "R",
                          verbose = getOption("verbose")) {
    template <- make_template(charset = charset, pkg = pkg, domain = domain)
    write_template(template, pkg = pkg, verbose = verbose)
}

#' @rdname templates
#' @export
read_template <- function(pkg = ".", domain = "R"){
    check_for_template(pkg = pkg, domain = domain)
    pot_file <- template_path(pkg = pkg, domain = domain)
    fix_metadata(read_po(pot_file))
}

#' @rdname templates
#' @export
write_template <- function(template, pkg = ".", verbose = getOption("verbose")) {
    pkg <- as.package(pkg)
    if(verbose) {
      message("Creating the 'po' directory")
    }
    make_po_dir(pkg = pkg)
    if(verbose) {
      message("Writing the 'pot' master translation file")
    }
    pot_file <- template_path(pkg = pkg, domain = template[["source_type"]])
    write_po(template, pot_file)
    return(invisible(pot_file))
}

#' @rdname templates
#' @export
check_for_template <- function(pkg = ".", domain = "R") {
    pkg <- as.package(pkg)
    pot_file <- template_path(pkg = pkg, domain = domain)
    if (file.exists(pot_file)) {
        return(TRUE)
    }
    stop("Template (.pot) file does not exist")
}

#' @rdname templates
#' @export
template_current <- function(template, pkg = ".", domain = "R") {
    template_from_file <- read_template(pkg = pkg, domain = domain)
    identical(template, template_from_file)
}

template_path <- function(pkg = ".", domain = "R") {
    pkg <- as.package(pkg)
    file.path(pkg$path, "po", paste0(if(domain %in% c("r","R")) "R-" else NULL, pkg$package, ".pot"))
}
