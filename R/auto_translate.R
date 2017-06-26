#' Auto translate messages
#' 
#' Automaticlly translate messages using Google Translate or Microsoft 
#' Translator.
#' @param translation An object of class \code{"po"} containing a message 
#' translation.
#' @param overwrite Logical. If \code{TRUE}, translations will be found for all
#' messages. If \code{FALSE}, only the messages that do not have an existing
#' translation will be translated.
#' @param api_key A string describing an API key for Google Translate or Microsoft 
#' Translator.
#' @param parallelization_strategy A string naming a parallelization strategy,
#' passed to \code{\link[future]{plan}}.
#' @return An object of class \code{"po"} containing a message translation.
#' @note To get a Google Translate API KEY, you must create a Google Cloud
#' account. See \url{https://cloud.google.com/translate/docs/getting-started}
#' to get started.
#' Likewise, to use Microsfot Translator, you need a Microsoft Azure account.
#' See \url{https://www.microsoft.com/en-us/translator/getstarted.aspx} to get 
#' started.#' @examples
#' \dontrun{
#'   # create example package
#'   pkg <- dummy_pkg()
#'   
#'   # setup pkg for localization
#'   use_localization(pkg)
#'   
#'   # generate Spanish translation in memory
#'   (tran <- make_translation("es", translator = "Some Person <example@example.com>"))
#'   
#'   # You need a Google Translate API key before running this
#'   auto_tran_google <- google_translate(tran)
#'   View(auto_tran_google$direct)
#'   View(auto_tran_google$countable)
#'   
#'   # You need a Microsoft Cognitive Services Translator API 
#'   # key before running this
#'   auto_tran_microsoft <- microsoft_translate(tran)
#'   View(auto_tran_microsoft$direct)
#'   View(auto_tran_microsoft$countable)
#' }
#' @importFrom autotranslate get_translations
auto_translate <- function(translation, overwrite = FALSE, api_key,
    parallelization_strategy = c("sequential", "multicore", "cluster"),
    engine = c("google", "microsoft")) {
  engine <- match.arg(engine)
  
  to_translate <- if(overwrite) {
    rep.int(TRUE, nrow(translation$direct))
  } else {
    translation$direct$msgstr == ""
  }
  direct_translations <- get_translations(
    x = translation$direct$msgid[to_translate],
    lang_to = translation$lang,
    lang_from = "en",
    api_key = api_key,
    engine = engine
  )  
  translation$direct$msgstr[to_translate] <- direct_translations
  
  # There may be a more sophisticated way of getting countable translations
  # for languages with multiple plural forms. For now if there is 1 plural form
  # just pass the singular form, and return it for both values.
  # In other cases, use the plural form multiple times.
  to_translate <- if (overwrite) {
    rep.int(TRUE, nrow(translation$countable))
  } else {
    vapply(
      translation$countable$msgstr,
      function(msgstri) {
        all(msgstri == "")
      },
      logical(1L)
    )
  }
  singular_countable_translations <- get_translations(
    x = translation$countable$msgid[to_translate],
    lang_to = translation$lang,
    lang_from = "en",
    api_key = api_key,
    engine = engine
  )
  plural_countable_translations <- if (translation$n_plural_forms > 1L) {
    get_translations(
      x = translation$countable$msgid_plural[to_translate],
      lang_to = translation$lang,
      lang_from = "en",
      api_key = api_key,
      engine = engine
    )  
  } else {
    character(sum(to_translate))
  }
  m <- translation$n_plural_forms - 1L
  translation$countable$msgstr[to_translate] <- Map(
    function(singular, plural) {
      c(singular, rep.int(plural, m))
    },
    singular_countable_translations,
    plural_countable_translations
  )
  translation
}

#' @rdname auto_translate
#' @export
google_translate <- function(translation, overwrite = FALSE, 
    api_key = Sys.getenv("GOOGLE_TRANSLATE_API_KEY"),
    parallelization_strategy = c("sequential", "multicore", "cluster")) {
  auto_translate(
    translation, 
    overwrite = overwrite, 
    api_key = api_key,
    engine = "google"
  )
}

#' @rdname auto_translate
#' @export
microsoft_translate <- function(translation, overwrite = FALSE, 
    api_key = Sys.getenv("MICROSOFT_TRANSLATOR_API_KEY"),
    parallelization_strategy = c("sequential", "multicore", "cluster")) {
  auto_translate(
    translation, 
    overwrite = overwrite, 
    api_key = api_key, 
    engine = "microsoft"
  )
}
