
test_that("check_for_gettext() works", {
    expect_true(check_for_gettext())
})

context("use_localization()  works")

suppressMessages(pkg_dir <- dummy_pkg(messages = FALSE))

test_that("use_localization() works without any messages", {
    use_localization(pkg = pkg_dir)
})

unlink(pkg_dir)
suppressMessages(pkg_dir <- dummy_pkg(messages = TRUE))

test_that("use_localization() works with messages", {
    use_localization(pkg = pkg_dir)
})


context("basic functions work")

test_that("get_messages() works", {
    expect_true(inherits(get_messages(pkg = pkg_dir), "data.frame"))
})

test_that("get_message_distances() works", {
    expect_true(inherits(get_message_distances(pkg = pkg_dir), "data.frame"))
})

test_that("spell_check_msgs() works", {
    expect_true(inherits(spell_check_msgs(pkg = pkg_dir), "data.frame"))
})

test_that("internal func translatable_messages() works", {
    expect_error(suppressWarnings(suppressMessages(translatable_messages())))
})

context("translation functions work")

test_that("make_translation() and write_translation() work", {
    es <- make_translation("es", translator = "Awesome Translator <translator@example.com", pkg = pkg_dir)
    expect_true(inherits(es, "po"), label = "make_translation() works")
    expect_true(is.character(write_translation(es, pkg = pkg_dir)), label = "write_translation() works")

    es2 <- make_translation("es", translator = "Awesome Translator <translator@example.com", pkg = pkg_dir)
    expect_true(inherits(es2, "po"), label = "make_translation() works from existing file")

    unlink(file.path(pkg_dir, "po", "R-translateme.pot"))
    es3 <- make_translation("es", translator = "Awesome Translator <translator@example.com", pkg = pkg_dir)
    expect_true(inherits(es3, "po"), label = "make_translation() works w/o existing template")
})


context("template functions work")

test_that("template_exists() works", {
    expect_true(template_exists(pkg = pkg_dir))
})

test_that("template_current() works", {
    expect_true(template_current(read_template(pkg = pkg_dir), pkg = pkg_dir))
})

test_that("read_template() works", {
    expect_true(inherits(read_template(pkg = pkg_dir), "po"))
})

test_that("sync_template() works", {
    expect_true(is.character(sync_template(pkg = pkg_dir)))
})

context("installation functions work")

test_that("check_translations() works", {
    expect_true(inherits(check_translations(pkg = pkg_dir), "list"))
})

test_that("check_translations() works", {
    expect_true(install_translations(pkg = pkg_dir))
})

unlink(pkg_dir, recursive = TRUE)
