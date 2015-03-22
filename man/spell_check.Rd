\name{spell_check}
\alias{spell_check}
\title{Spell check diagnostic messages}
\description{Extracts diagnostic messages from a package and checks them for spelling errors.}
\usage{
spell_check(pkg = ".", control = list(), program = NULL)
}
\arguments{
    \item{pkg}{The directory of an R package to extract diagnostic messages from. Path is passed to \code{\link[devtools]{as.package}}.}
    \item{control}{A list or character vector of control options for the spell checker. See \code{\link[utils]{aspell}}.}
    \item{program}{A character string giving the name (if on the system path) or full path of the spell check program to be used, or \code{NULL} (default). By default, the system path is searched for \code{aspell}, \code{hunspell}, and \code{ispell} (in that order), and the first one found is used.}
}
\details{Extracts diagnostic messages from a package, saves those messages as temporary files, and processes them using \code{\link[utils]{aspell}}. This function requires an installed and accessible spell checking program. \code{aspell} will try looking for each of the following (in this order): \href{http://aspell.net}{aspell}, \href{http://hunspell.sourceforge.net/}{hunspell}, or \href{http://lasr.cs.ucla.edu/geoff/ispell.html}{ispell}).}
\value{A data.frame of class \dQuote{msgspellcheck}, containing each potentially misspelled word, the message in which it is located, the name and location of the message in the source code file, and (for each word) a list of suggested corrections.}
\author{Thomas J. Leeper}
%\references{}
%\seealso{}
%\examples{}
