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
#' @return An object of class \code{"po"} containing a message translation.
#' @note To get a Google Translate API KEY, you must create a Google Cloud
#' account. See \url{https://cloud.google.com/translate/docs/getting-started}
#' to get started.
#' Likewise, to use Microsfot Translator, you need a Microsoft Azure account.
#' See \url{https://www.microsoft.com/en-us/translator/getstarted.aspx} to get 
#' started.
auto_translate <- function(translation, overwrite = FALSE, api_key, 
                           engine = c("google", "microsoft")) {
  engine <- match.arg(engine)
  
  to_translate <- if(overwrite) {
    rep.int(TRUE, nrow(translation$direct))
  } else {
    translation$direct$msgstr == ""
  }
  direct_translations <- translate(
    content.vec = translation$direct$msgid[to_translate],
    google.api.key = api_key,
    source.lang = "en",
    target.lang = translation$lang
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
  singular_countable_translations <- translate(
    content.vec = translation$countable$msgid[to_translate],
    google.api.key = api_key,
    source.lang = "en",
    target.lang = translation$lang
  )
  plural_countable_translations <- if (translation$n_plural_forms > 1L) {
    translate(
      content.vec = translation$countable$msgid_plural[to_translate],
      google.api.key = api_key,
      source.lang = "en",
      target.lang = translation$lang
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
                             api_key = Sys.getenv("GOOGLE_TRANSLATE_API_KEY")) {
  auto_translate(translation, overwrite = overwrite, api_key, engine = "google")
}

#' @rdname auto_translate
#' @export
microsoft_translate <- function(translation, overwrite = FALSE, 
                                api_key = Sys.getenv("MICROSOFT_TRANSLATER_API_KEY")) {
  auto_translate(translation, overwrite = overwrite, api_key, engine = "microsoft")
}

# translateR::translate() is currently broken.
# Maintainers contacted and awaiting a fix.
# In the meantime, this is the same function with lapply() instead of mclapply().
translate <- function (dataset = NULL, content.field = NULL, content.vec = NULL, 
  google.api.key = NULL, microsoft.client.id = NULL, microsoft.client.secret = NULL, 
  source.lang = NULL, target.lang = NULL) {
  translator <- translateR:::validateInput(dataset, content.field, content.vec, 
    google.api.key, microsoft.client.id, microsoft.client.secret, 
    source.lang, target.lang)
  if (!(is.null(dataset))) {
    to.translate <- dataset[[content.field]]
  }
  if (!(is.null(content.vec))) {
    to.translate <- content.vec
  }
  translateR:::checkLang(to.translate, source.lang, translator)
  if (translator == "Google") {
    translated <- unname(unlist(lapply(to.translate, function(x) translateR:::googleTranslate(x, 
      google.api.key, source.lang, target.lang))))
  }
  if (translator == "Microsoft") {
    ptm <- proc.time()
    access.token <- getAccessToken(microsoft.client.id, microsoft.client.secret)
    translated <- c()
    for (doc in to.translate) {
      translated <- c(translated, microsoftTranslate(doc, 
        access.token, source.lang, target.lang))
      if ((proc.time() - ptm)[3] > 540) {
        ptm <- proc.time()
        access.token <- getAccessToken(microsoft.client.id, 
          microsoft.client.secret)
      }
    }
  }
  if (!(is.null(content.vec))) {
    return(translated)
  }
  if (!(is.null(dataset) & is.null(content.field))) {
    dataset$translatedContent <- translated
    return(dataset)
  }
}

