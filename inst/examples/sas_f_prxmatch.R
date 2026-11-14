# PRXMATCH: each call is explained above it and followed by its result

# PRXMATCH returns the position of the first match of the Perl regular
# expression /b+/: 2.
sas_f_prxmatch("/b+/", "abbc")  # prxmatch('/b+/', 'abbc') = 2

# There is no match, so the result is 0.
sas_f_prxmatch("/z/", "abc")  # prxmatch('/z/', 'abc') = 0

# The i option makes matching case-insensitive, so /B/i matches 'b' at position
# 2.
sas_f_prxmatch("/B/i", "abc")  # prxmatch('/B/i', 'abc') = 2

# ^ anchors the match at the start, and the string starts with a blank, so
# there is no match.
sas_f_prxmatch("/^a/", " abc")  # prxmatch('/^a/', ' abc') = 0
