# ANYSPACE: each call is explained above it and followed by its result

# The first whitespace character (a blank) is at position 3.
sas_f_anyspace("ab c")  # anyspace('ab c') = 3

# No blank or other whitespace character occurs, so the result is 0.
sas_f_anyspace("abc")  # anyspace('abc') = 0

# Start 3 searches rightwards from position 3, skipping the blank at position
# 2, and finds the blank at position 4.
sas_f_anyspace("a b c", 3)  # anyspace('a b c', 3) = 4
