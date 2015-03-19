spell_check <- function(check = "all", pkg = ".", control = list(), program = NULL) {
    pkg <- as.package(pkg)
    
    if("all" %in% check) {
        all <- c("documentation", "vignettes", "r", "c", "messages")
    }
    all <- tolower(all)
    
    out <- list()
    if("documentation" %in% all)
        out$documentation <- aspell_package_Rd_files(dir = pkg$path, control = control, program = program)
    if("vignettes" %in% all)
        out$vignettes <- aspell_package_vignettes(dir = pkg$path, control = control, program = program)
    if("r" %in% all)
        out$R <- aspell_package_R_files(dir = pkg$path, control = control, program = program)
    if("c" %in% all)
        out$C <- aspell_package_C_files(dir = pkg$path, control = control, program = program)
    if("messages" %in% all) {
        msgs <- get_messages(dir = pkg$path)
        d <- tempdir()
        f <- character(length(msgs))
        for(i in seq_along(msgs)) {
            f[i] <- tempfile(dir = d, fileext = basename(names(msgs)[i]))
            writeLines(msgs[[i]], con = f[i])
            on.exit(unlink(f[i]))
        }
        out$messages <- aspell(f, control = control, program = program)
    }
    structure(out, class = "spellcheck")
}
