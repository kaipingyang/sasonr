# PROPCASE: each call is explained above it and followed by the result of the same call in SAS 9.4

# PROPCASE capitalizes the first letter of each word and lowercases the other
# letters.
sas_f_propcase("hello WORLD")  # SAS 9.4: propcase('hello WORLD') = "Hello World"

# The second argument replaces the default delimiters: with only '-' the blank
# no longer starts a word, so 'c' stays lowercase.
sas_f_propcase("a-b c", "-")  # SAS 9.4: propcase('a-b c', '-') = "A-B c"

# With the default delimiters (blank, slash, hyphen, open parenthesis, period,
# tab) the letter after '-' is capitalized.
sas_f_propcase("jean-luc")  # SAS 9.4: propcase('jean-luc') = "Jean-Luc"

# A blank string stays blank.
sas_f_propcase(" ")  # SAS 9.4: propcase('') = ""
