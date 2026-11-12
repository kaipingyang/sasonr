# ANYUPPER: each call is explained above it and followed by the result of the same call in SAS 9.4

# The first uppercase letter is 'C' at position 3.
sas_f_anyupper("abC")  # SAS 9.4: anyupper('abC') = 3

# No uppercase letter occurs, so the result is 0.
sas_f_anyupper("abc")  # SAS 9.4: anyupper('abc') = 0

# Negative start -2 searches leftwards from position 2 ('b') and finds the
# uppercase 'A' at position 1.
sas_f_anyupper("AbC", -2)  # SAS 9.4: anyupper('AbC', -2) = 1
