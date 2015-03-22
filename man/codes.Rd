\name{codes}
\alias{language_code}
\alias{country_code}
\title{Find ISO codes}
\description{Find country or language codes}
\usage{
language_code(language)

country_code(country)
}
\arguments{
    \item{language}{A character string containing a language name.}
    \item{country}{A character string containing a country name.}
}
\details{
\code{language_code} searches for languages by name and returns information for the ISO-639-2 language name and code, which must be used as language names in creating message translations (see \code{\link{make_translation}}) or setting the R environment language in \code{\link{setlang}}.

\code{country_code} searches for countries by name and returns information for the ISO-3166-1 country name and code, which can be used to create further localizations of languages.

Both functions search through data in the \code{ISOcodes} package and return data.frames containing any matching languages or countries, respectively.
}
\value{A data.frame containing language or country information.}
\author{Thomas J. Leeper}
%\references{}
\seealso{\code{\link{setlang}}}
%\examples{}
