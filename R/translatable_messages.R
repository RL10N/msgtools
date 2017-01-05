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
