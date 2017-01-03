# Create a dummy package for use with msgtools examples

library(devtools)
library(pathological)

# Create a pacakge in the temp dir
description <- list(
  Title = "A Small Package With Translatable Messages",
  Description = "For use in msgtools examples.",
  License = "Unlimited"
)
pkg_dir <- temp_dir("translateme")
create(pkg_dir, description = description, rstudio = FALSE)

# Add a function to that package
writeLines(
  "#' Create a translatable message
#' 
#' Create a translatable message using \\code{\\link[base]{gettext}}, 
#' \\code{ngettext}, \\code{\\link[base]{gettextf}}, \\code{\\link[base]{gettext}},
#' \\code{\\link[base]{message}}, \\code{\\link[base]{warning}}, and 
#' \\code{\\link[base]{stop}}.
#' @param n A non-negative integer.
#' @return Some output is generated, then the function throws an error. 
#' the existence of the function means that the pacakge contains seven 
#' translatable messages.
#' @export
translatable_messages <- function(n = 7) {
  # Simple way of making a message translatable
  gettext('As I was going to St. Ives')

  # If the message depends upon a number, use ngettext instead
  wives <- ngettext(
    n, 
    'I met a man with %d wife', 
    'I met a man with %d wives'
  )

  # If variables have to be included in the message, use gettextf
  gettextf('Every wife had %d sacks', n)

  # The contents of messages will be translated, but splitting into
  # multiple variables is bad practise
  message('Every sack had ', n, ' cats')

  # Instead, use gettextf inside message
  message(gettextf('Every cat had %d kits', n))

  # Warnings and errors are already included for translation
  warning('Kits, cats, sacks, and wives')
  stop('How many were going to St. Ives?')
}  
",
  con = file.path(pkg_dir, "R", "fns.R")
)

# Also useful to check that it works
# check(pkg_dir)

# Zip contents and copy to msgtools/inst/extdata
current_dir <- getwd()
withr::with_dir(
  parent_dir(pkg_dir),
  zip(
    file.path(current_dir, "inst/extdata/translateme.zip"), 
    files = file.path("translateme", dir(pkg_dir, recursive = TRUE))
  )
)

