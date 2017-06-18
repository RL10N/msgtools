# CHANGES TO msgtools 0.2.7

* Fix CRAN failures due to `en@quot` UTF-8 directional quote language. (#22)

# CHANGES TO msgtools 0.2.6

* Work on UTF-8 issue. (#22)
* Fixed bug in `use_localization()` (specifically in `make_template()` when when no messages are present. Added test of this. (#21)

# CHANGES TO msgtools 0.2.5

* Updated SystemRequirements to specify `GNU gettext`. (#20)
* Made tests conditional successful `check_for_gettext()`. (#20)
* Updated calls to `poio::fix_metadata()` to pass explicit `Last-Translator` metadata.
* Installed Spanish translation of some messages.

# CHANGES TO msgtools 0.2.4

* Updated package to work with new R6-based "po" objects from **poio**.

# CHANGES TO msgtools 0.2.3

* Added a vignette demonstrating package functionality and streamlined the README. (#3)
* Added simple test suite. (#18)

# CHANGES TO msgtools 0.2.2

* Streamlined the `install_translations()` function and separated `check_translation()` and `check_translations()` into separate functions.
* Added `sync_template()` function to update an existing template (or create a new template file if doesn't exist). This is now called internally by `use_localization()`.
* Added `check_for_gettext()` function to confirm availability of gettext.
* Added `get_message_distances()` function to compare message string edit distance (useful for duplicate checking).
* `make_translation()` now updates an existing translation against the template file before reading into memory.
* Updated README documentation.
* Fixed numerous `R CMD check` warnings and notes.

# CHANGES TO msgtools 0.2.1

* Use **poio** for .po and .pot file input and output. (#16)
* Use **hunspell** for spell checking.
* Exported many previously unexported functions and updated functionality considerably.

# CHANGES TO msgtools 0.1.0

* Initial package released.
