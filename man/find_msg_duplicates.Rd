\name{find_msg_duplicates}
\alias{find_msg_duplicates}
\title{Check for duplicated messages}
\description{Check the diagnostic messages in a package for duplicates and near-duplicates}
\usage{
find_msg_duplicates(pkg = ".")
}
\arguments{
    \item{pkg}{The directory of an R package to extract diagnostic messages from. Path is passed to \code{\link[devtools]{as.package}}.}
}
\details{Extracts diagnostic messages from a package and checks those messages for duplicates and near-duplicates. This can be useful to identifying unintentional mismatches between different invocations of the same message, warning, or error.}
\value{A named list of information about each message. The list contains the messages, their source code file locations, a list of identical matches for each message, and a distance matrix indicating the string similarity of the each message to all of the others.}
\author{Thomas J. Leeper}
%\references{}
%\seealso{}
%\examples{}
