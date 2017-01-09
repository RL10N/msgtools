library("testthat")
library("msgtools")

if (check_for_gettext()) {
    test_check("msgtools", reporter = "summary")
}
