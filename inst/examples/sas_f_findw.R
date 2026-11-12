# FINDW: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# FINDW returns the position of the word 'cat', delimited by blanks: 5.
sas_f_findw("the cat sat", "cat")  # SAS 9.4: findw('the cat sat', 'cat') = 5

# 'at' occurs only inside other words, never as a whole word, so the result is
# 0.
sas_f_findw("the cat sat", "at")  # SAS 9.4: findw('the cat sat', 'at') = 0

# With ',' as the delimiter, the word 'b' starts at position 3.
sas_f_findw("a,b,c", "b", ",")  # SAS 9.4: findw('a,b,c', 'b', ',') = 3

# Modifier i ignores case, so 'cat' matches 'Cat' at position 5.
sas_f_findw("The Cat", "cat", " ", "i")  # SAS 9.4: findw('The Cat', 'cat', ' ', 'i') = 5

# Modifier e returns the word number instead of the character position: 'sat'
# is the 3rd word.
sas_f_findw("the cat sat", "sat", " ", "e")  # SAS 9.4: findw('the cat sat', 'sat', ' ', 'e') = 3
