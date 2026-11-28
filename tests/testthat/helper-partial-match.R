# `$` on a list matches a longer name partially (s$text finds s$texts): such a
# match is a bug in sasonr, so the tests warn on it.
options(warnPartialMatchDollar = TRUE)
