# PRXMATCH: each call is explained above it and followed by the result of the same call in SAS 9.4

# PRXMATCH returns the position of the first match of the Perl regular
# expression /b+/: 2.
sas_f_prxmatch("/b+/", "abbc")  # SAS 9.4: prxmatch('/b+/', 'abbc') = 2

# There is no match, so the result is 0.
sas_f_prxmatch("/z/", "abc")  # SAS 9.4: prxmatch('/z/', 'abc') = 0

# The i option makes matching case-insensitive, so /B/i matches 'b' at position
# 2.
sas_f_prxmatch("/B/i", "abc")  # SAS 9.4: prxmatch('/B/i', 'abc') = 2

# ^ anchors the match at the start, and the string starts with a blank, so
# there is no match.
sas_f_prxmatch("/^a/", " abc")  # SAS 9.4: prxmatch('/^a/', ' abc') = 0
