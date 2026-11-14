# COUNTW: each call is explained above it and followed by its result

# With the default delimiters, consecutive blanks act as one separator, giving
# 3 words.
sas_f_countw("a b  c")  # countw('a b  c') = 3

# With ',' as the delimiter, consecutive commas act as one separator, giving 3
# words.
sas_f_countw("a,b,,c", ",")  # countw('a,b,,c', ',') = 3

# Modifier m makes each delimiter count, so the two adjacent commas enclose an
# empty word: 4 words.
sas_f_countw("a,b,,c", ",", "m")  # countw('a,b,,c', ',', 'm') = 4

# A blank string contains no words.
sas_f_countw(" ")  # countw('') = 0
