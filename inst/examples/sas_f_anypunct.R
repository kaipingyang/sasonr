# ANYPUNCT: each call is explained above it and followed by the result of the same call in SAS 9.4

# The comma at position 3 is the first punctuation character.
sas_f_anypunct("ab,c")  # SAS 9.4: anypunct('ab,c') = 3

# No punctuation character occurs, so the result is 0.
sas_f_anypunct("abc")  # SAS 9.4: anypunct('abc') = 0

# Start 3 searches rightwards from position 3, skipping the '!' at position 2,
# and finds '?' at position 4.
sas_f_anypunct("a!b?", 3)  # SAS 9.4: anypunct('a!b?', 3) = 4
