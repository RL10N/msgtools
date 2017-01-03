# CHANGES TO msgtools 0.1.2

* Streamlined the `install_translations()` function and separated `check_translation()` and `check_translations()` into separate functions.
* Added `sync_template()` function to update an existing template (or create a new template file if doesn't exist). This is now called internally by `use_localization()`.
* Added `check_for_gettext()` function to confirm availability of gettext.
* Added `get_message_distances()` function to compare message string edit distance (useful for duplicate checking).
* `make_translation()` now updates an existing translation against the template file before reading into memory.
* Updated README documentation.
* Fixed numerous `R CMD check` warnings and notes.

# CHANGES TO msgtools 0.1.1

* Use **poio** for .po and .pot file input and output. (#16)
* Use **hunspell** for spell checking.
* Exported many previously unexported functions and updated functionality considerably.

# CHANGES TO msgtools 0.1.0

* Initial package released.
