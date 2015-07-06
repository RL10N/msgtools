make_po_dir <- function(pkg = ".") {
    pkg <- as.package(pkg)
    po_dir <- file.path(pkg$path, "po")
    dir.create(po_dir, FALSE)
    return(po_dir)
}

make_template <- 
function(pkg = ".", 
         created = format(Sys.time(), "%Y-%m-%d %H:%M"),
         maintainer,
         translator,
         team,
         charset = "UTF-8",
         domain = "R") {
    # generate internal msgtemplate representation (calling `xgettext` and `xngettext`)
    pkg <- as.package(pkg)
    
    if(domain != "R")
        stop("Currently only the R domain is implemented")
    
    template <- 
        list("Project-Id-Version" = paste(pkg$package, pkg$version), 
             "Report-Msgid-Bugs-To" = if(!missing(maintainer)) format(maintainer) else pkg$maintainer, 
             "POT-Creation-Date" = created, 
             "PO-Revision-Date" = format(Sys.time(), "%Y-%m-%d %H:%M"), 
             "Last-Translator" = if(!missing(translator)) format(translator) else "", 
             "Language-Team" = if(!missing(team)) format(team) else "", 
             "Language" = "LL",
             "MIME-Version" = "1.0", 
             "Content-Type" = paste0("text/plain; charset=", charset), 
             "Content-Transfer-Encoding" = "8bit"
        )
    
    # run xgettext
    tmp <- unique(unlist(get_messages(pkg = pkg, type = "gettext")))
    msgids1 <- list()
    tmp <- tmp[nzchar(tmp)]
    if (length(tmp) > 0L) {
        msgids1 <- lapply(tmp, gettext_msg)
    }
    
    # run xngettext
    tmp <- get_messages(pkg = pkg, type = "ngettext")
    msgids2 <- list()
    i <- 1
    un <- unique(unlist(tmp, recursive = TRUE))
    for (ee in tmp) {
        for (e in ee) {
            if (e[1L] %in% un) {
                msgids2[[i]] <- ngettext_msg(e[1L], e[2L])
                i <- i + 1
                un <- un[-match(e, un)]
            }
        }
    }
    
    # parse template to internal representation
    structure(c(template, msgids = list(c(msgids1, msgids2))), class = "msgtemplate", domain = domain)
}

use_translations <- 
function(pkg = ".", 
         domain = "R",
         ...) {
    pkg <- as.package(pkg)

    po_dir <- file.path(pkg$path, "po")
    if(!file.exists(po_dir))
        po_dir <- make_po_dir(pkg = pkg)
    template_file <- file.path(pkg$path, "po", paste0(if(domain == "R") "R-" else NULL, pkg$package, ".pot"))
    
    template <- make_template(pkg = pkg, domain = domain, ...)
    if(template_exists(pkg = pkg, domain = domain)) {
        current <- template_current(pkg = pkg, template = template, domain = domain)
        if(current) {
            message("The .pot template file has not changed")
            return(attributes(current)$template)
        } else {
            template$"POT-Creation-Date" <- attributes(current)$"template_file"$"POT-Creation-Date"
            write_template(template, domain = domain)
        }
    } else {
        write_template(template, domain = domain)
    }
    structure(template, file = template_file, domain = domain)
}

template_exists <- function(pkg = ".", domain = "R") {
    pkg <- as.package(pkg)
    template_file <- file.path(pkg$path, "po", paste0(if(domain == "R") "R-" else NULL, pkg$package, ".pot"))
    file.exists(template_file)
}

gettext_msg <- function(msg, tr = "") {
    structure(list(msgid = msg, 
                   msgstr = tr), 
              class = "gettext_msg")
}

ngettext_msg <- function(msg, plural, ...) {
    a <- list(...)
    if(!length(a)) {
        # need to generalize this to arbitrary numbers of translation strings
        m0 <- ""
        m1 <- ""
    }
    structure(list(msgid = msg, 
                   msgid_plural = plural, 
                   msgstr0 = m0, 
                   msgstr1 = m1), 
              class = "ngettext_msg")
}

parse_template <- function(template) {
    h <- gsub("[\"]$", "", gsub("^[\"]", "", template[!grepl("msgid", template)]))
    h <- read.dcf(textConnection(h[!grepl("msgstr", h)]))
    h <- gsub("\\\\n", "", h)
    out <- setNames(as.list(h), colnames(h))
    out$msgids <- list()
    
    gx <- grep("msgid", template)
    gn <- grep("msgid_plural", template)
    
    # parse `xgettext` messages
    if(length(gx)) {
        gx <- gx[!gx %in% (gn-1)]
        msgids1 <- template[gx]
        msgids1 <- gsub("^\n", "", msgids1)
        msgids1 <- sapply(strsplit(msgids1, "\nmsgstr"), `[`, 1)
        msgids1 <- gsub("^msgid ", "", msgids1)
        msgids1 <- gsub("^[\"]", "", msgids1)
        msgids1 <- gsub("[\"]$", "", msgids1)
        msgids1 <- msgids1[msgids1 != ""]
        
        # for translations: handle translation strings
        
        msgids1out <- lapply(msgids1, gettext_msg)[sort.list(msgids1)]
        out$msgids <- msgids1out
    }
    
    # parse `nxgettext` messages
    if(length(gn)) {
        msgids2single <- template[gn-1]
        msgids2single <- gsub("^\n", "", msgids2single)
        msgids2single <- gsub("^msgid ", "", msgids2single)
        msgids2single <- gsub("^[\"]{1,2}", "", msgids2single)
        msgids2single <- gsub("[\"]+$", "", msgids2single)
        
        msgids2plural <- template[gn]
        msgids2plural <- gsub("^\n", "", msgids2plural)
        msgids2plural <- gsub("^msgid_plural ", "", msgids2plural)
        msgids2plural <- gsub("^[\"]+", "", msgids2plural)
        msgids2plural <- gsub("[\"]+$", "", msgids2plural)
        
        # for translations: handle arbitrary number of translation strings
        
        msgids2out <- unname(mapply(ngettext_msg, msgids2single, msgids2plural, SIMPLIFY = FALSE)[sort.list(msgids2single)])
        out$msgids <- c(out$msgids, msgids2out)
    }
    
    structure(out, class = "msgtemplate")
}

write_template <- function(template, pkg = ".", domain = "R") {
    pkg <- as.package(pkg)
    po_dir <- file.path(pkg$path, "po")
    if(!file.exists(po_dir))
        make_po_dir(pkg = pkg)
    template_file <- file.path(po_dir, paste0(if(domain == "R") "R-" else NULL, pkg$package, ".pot"))
    out <- c("msgid \"\"\nmsgstr \"\"")
    msgids <- template$msgids
    template$msgids <- NULL
    out <- c(out, paste0("\n\"", names(template),": ", unname(unlist(template)), "\\n\""))
    
    msgids <- msgids[order(unlist(lapply(msgids, `[`, "msgid")))]
    
    for(i in seq_along(msgids)) {
        if(inherits(msgids[[i]], "gettext_msg")) {
            out <- c(out, paste0("\n\nmsgid \"", msgids[[i]]$msgid, "\"\nmsgstr \"\""))
        } else {
            out <- c(out, paste0("\n\nmsgid \"", msgids[[i]]$msgid, 
                                 "\"\nmsgid_plural \"", msgids[[i]]$msgid_plural, 
                                 "\"\nmsgstr[0] \"\"\nmsgstr[1] \"\""))
        }
    }
    writeLines(c(out,"\n"), con = template_file, sep = "")
}

read_template <- function(file, pkg = ".", domain = "R"){
    if(missing(file)) {
        pkg <- as.package(pkg)
        file <- file.path(pkg$path, "po", paste0(if(domain == "R") "R-" else NULL, pkg$package, ".pot"))
    }
    if(!file.exists(file))
        stop("The .pot template file not found")
    con <- file(file, "r")
    on.exit(close(con))
    template <- readLines(con)
    structure(parse_template(template), file = file, domain = domain)
}

template_current <- function(template, pkg = ".", domain = "R") {
    template_file <- read_template(pkg = pkg, domain = domain)
    
    # do not compare creation or revision dates
    template2 <- template
    template2$"PO-Revision-Date" <- template_file$"PO-Revision-Date"
    template2$"POT-Creation-Date" <- template_file$"POT-Creation-Date"
    structure(identical(template2, template_file), 
              template = template, 
              template_file = template_file,
              comparison = all.equal(template2, template_file))
}
