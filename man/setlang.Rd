\name{setlang}
\alias{setlang}
\title{Set or reset locale language}
\description{Set or reset a locale language}
\usage{
setlang(language)

resetlang(language)
}
\arguments{
    \item{language}{A character string containing a language.}
}
\details{\code{setlang} sets the \code{LANG} environment variable and stores the supplied value of \code{language} in \code{options("msgtools_LANG")}. \code{resetlang} resets the language to the value specified in that option or, if that is missing, to English (\dQuote{EN}). This can be useful for testing message translations in various languages.}
\value{Invisibly, a logical indicating with the language setting was successful.}
\author{Thomas J. Leeper}
%\references{}
%\seealso{}
%\examples{}
