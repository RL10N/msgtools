library("testthat")
library("msgtools")

if (!grepl('SunOS',Sys.info()['sysname']) && check_for_gettext()) {
    test_check("msgtools", reporter = "summary")
}
