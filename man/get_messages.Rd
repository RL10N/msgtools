\name{get_messages}
\alias{get_messages}
\title{Extract diagnostic messages}
\description{Extracts diagnostic messages from a package using \code{xgettext}}
\usage{
get_messages(pkg = ".", type = "xgettext")
}
\arguments{
    \item{pkg}{The directory of an R package to extract diagnostic messages from. Path is passed to \code{\link[devtools]{as.package}}.}
    \item{type}{Type of messages to retrieve. Either \dQuote{xgettext} (for \code{\link[link]{xgettext}}) or \dQuote{xngettext} (for \code{\link[link]{xngettext}}).}
}
\details{Extracts diagnostic messages from a package.}
\value{A named list of diagnostic messages. The list elements are code files (named with their complete file path) and contain the messages as elements.}
\author{Thomas J. Leeper}
%\references{}
%\seealso{}
%\examples{}
