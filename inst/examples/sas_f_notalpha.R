# NOTALPHA: each call is explained above it and followed by the result of the same call in SAS 9.4

# The first character that is not a letter is '1' at position 3.
sas_f_notalpha("ab1")  # SAS 9.4: notalpha('ab1') = 3

# All characters are letters, so the result is 0.
sas_f_notalpha("abc")  # SAS 9.4: notalpha('abc') = 0

# Start 3 searches rightwards from position 3, which holds the letter 'b', so
# the result is 0.
sas_f_notalpha("a1b", 3)  # SAS 9.4: notalpha('a1b', 3) = 0
