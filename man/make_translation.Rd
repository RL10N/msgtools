\name{make_translation}
\alias{make_translation}
\title{Make Translation}
\description{Make a translation of diagnostic messages}
\usage{
document_msgs(pkg = ".")
}
\arguments{
    \item{language}{}
    \item{pkg}{The directory of an R package to extract diagnostic messages from. Path is passed to \code{\link[devtools]{as.package}}.}
    \item{domain}{}
    \item{write}{}
}
\details{This function creates a translation object from a message template file, with the optional (default \code{TRUE}) side-effect of saving it to disk in the package \code{/po} directory.}
\value{A character string containing the file path of the created translation (\dQuote{.po}) file, with the internal translation object returned as an attribute.}
\author{Thomas J. Leeper}
%\references{}
%\seealso{}
\examples{
\dontrun{
generate_template()
make_translation("fr")
}
}
