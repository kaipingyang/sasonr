# INDEXW: each call is explained above it and followed by its result
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# INDEXW returns the position of 'cat' as a whole word delimited by blanks.
sas_f_indexw("the cat sat", "cat")  # indexw('the cat sat', 'cat') = 5

# 'at' occurs only inside other words, so the result is 0.
sas_f_indexw("the cat sat", "at")  # indexw('the cat sat', 'at') = 0

# The third argument sets the word delimiters; with ';' the word 'b' starts at
# position 3.
sas_f_indexw("a;b;c", "b", ";")  # indexw('a;b;c', 'b', ';') = 3

# A word may be the entire string.
sas_f_indexw("cat", "cat")  # indexw('cat', 'cat') = 1
