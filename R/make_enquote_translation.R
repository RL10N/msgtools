make_enquote_translation <- function(pkg = ".", domain = "R", verbose = getOption("verbose")) {
    pkg <- as.package(pkg)
    
    inst_po_dir <- file.path(pkg$path, "inst", "po")
    dest <- file.path(inst_po_dir, "en@quot", "LC_MESSAGES")
    dir.create(dest, FALSE, TRUE)
    
    f1 <- tempfile()
    translation <- make_translation("en", translator = "", pkg = pkg, domain = domain, verbose = verbose)
    translation[["direct"]][["msgstr"]] <- translation[["direct"]][["msgid"]]
    translation[["countable"]][["msgstr"]] <- mapply(c, translation[["countable"]][["msgid"]], 
                                                        translation[["countable"]][["msgid_plural"]],
                                                     SIMPLIFY = FALSE)
    write_po(translation, po_file = f1)
    char <- rawToChar(readBin(f1, "raw", n = 1e8))
    char <- sub("Language: en\\\\n", "Language: en@quot\\\\n", char)
    writeBin(char, f1)
    return(f1)
}
