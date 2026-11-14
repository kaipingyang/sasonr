# PROPCASE: each call is explained above it and followed by its result

# PROPCASE capitalizes the first letter of each word and lowercases the other
# letters.
sas_f_propcase("hello WORLD")  # propcase('hello WORLD') = "Hello World"

# The second argument replaces the default delimiters: with only '-' the blank
# no longer starts a word, so 'c' stays lowercase.
sas_f_propcase("a-b c", "-")  # propcase('a-b c', '-') = "A-B c"

# With the default delimiters (blank, slash, hyphen, open parenthesis, period,
# tab) the letter after '-' is capitalized.
sas_f_propcase("jean-luc")  # propcase('jean-luc') = "Jean-Luc"

# A blank string stays blank.
sas_f_propcase(" ")  # propcase('') = ""
