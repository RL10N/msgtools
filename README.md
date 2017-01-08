# Tools for Developing Diagnostic Messages

<img src="http://rl10n.github.io/images/logo.png" alt="RL10N Lion" height="150" />

**msgtools** implements a number utilities for developing and maintaining error, warning, and other messages in R packages, including checking for consistency across messages, spell-checking messages, and building message translations into various languages for purposes of localization. The package is designed to enhance **devtools**.

## Building and updating message translation files

**msgtools** provides a simplified workflow for creating, maintaining, and updating translations of messages in both R and C code that improve upon those available in the **tools** package. For some context on message localization in R, see ["R Administration"](https://cran.r-project.org/doc/manuals/r-devel/R-admin.html#Localization-of-messages) and ["Translating R Messages, R >=3.0.0"](https://developer.r-project.org/Translations30.html)). A list of "translation teams" is available from https://developer.r-project.org/TranslationTeams.html.

R has reasonably sophisticated built-in *internationalization* (i18n) features. All calls to `message()`, `warning()`, `stop()`, `gettext()`, `ngettext()`, and `gettextf()` are passed through a C-level functionality that translates the contained messages into the user's local language (if the translations are available).

To begin *localization* (l10n) of messages, a package developer simply needs to (1) write translations for these messages using a standardized file format and (2) install the translations into their package. Localization traditionally requires the manual creation of a `.pot` ("portable object template") file using the `xgettext` command line utility and the generation of language-specific .po ("portable object") translation files that specify the translation of each message string into the target language.

The .pot/.po file format is fairly straightforward, simply containing a series of messages and their translations, plus some metadata:

```
#: lib/error.c:116
msgid "Unknown system error"
msgstr "Error desconegut del sistema"
```

(The R source contains a number of examples of translations, for example in [the base package](https://svn.r-project.org/R/trunk/src/library/base/po/).)

**msgtools** negates the need to directly call the command-line utilities or interact with .pot/.po files by relying on R representations of templates and translations (provided by [poio](https://cran.r-project.org/package=poio)). This makes it possible to programmatically create, modify, and install translations, thus eliminating the need to manually open each translation file in a text editor.

A basic **msgtools** workflow is as follows:

```R
library("msgtools")

# setup package for localization
use_localization()

# create Spanish translation
es <- make_translation("es", translator = "Some Translator <translator@example.com")
write_translation(es)

## edit translations manually!

# check translation files for errors
check_translations()

# install translations
install_translations()
```

At this point, you still have to manually edit the the .po translation files created by `make_translation()` using either a text editor or the lightweight `edit_translation()` function. If you change your package code, you need to update the template file and each of the translation files to ensure they contain any new or modified messages. The easiest way to do this is:

```R
sync_translations()

# translate, again!

# re-install
install_translations()
```

A [vignette included in the package](https://github.com/RL10N/msgtools/blob/master/vignettes/Tutorial.Rmd) provides a fully documented workflow using a simple example package.

## Requirements and Installation

[![CRAN Version](https://www.r-pkg.org/badges/version/msgtools)](https://cran.r-project.org/package=msgtools)
![Downloads](https://cranlogs.r-pkg.org/badges/msgtools)
[![Travis-CI Build Status](https://travis-ci.org/RL10N/msgtools.png?branch=master)](https://travis-ci.org/RL10N/msgtools)
[![codecov.io](https://codecov.io/github/RL10N/msgtools/coverage.svg?branch=master)](https://codecov.io/github/RL10N/msgtools?branch=master)

The development version can be installed directly from GitHub using `ghit`:

```R
if (!require('ghit')) {
    install.packages('ghit')
    library('ghit')
}
install_github('RL10N/msgtools')
```

**msgtools** will eventually be available on [CRAN](https://cran.r-project.org/package=msgtools), so that it can be installed using:

```R
install.packages('msgtools')
```

Note that **msgtools** also has one system requirement (i.e., software that must be installed separately from R). Specifically, the package requires [GNU gettext-tools](https://www.gnu.org/software/gettext/). Windows binaries can be installed using the `install_gettext()` function or by downloading binaries directly from http://www.stats.ox.ac.uk/pub/Rtools/goodies/gettext-tools.zip or https://github.com/mlocati/gettext-iconv-windows.

## Acknowledgements

The development of **msgtools** was generously supported by [The R Consortium](https://www.r-consortium.org/) through an award to Richie Cotton and Thomas Leeper.
