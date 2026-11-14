# NOTLOWER: each call is explained above it and followed by its result

# The first character that is not a lowercase letter is 'C' at position 3.
sas_f_notlower("abC")  # notlower('abC') = 3

# All characters are lowercase letters, so the result is 0.
sas_f_notlower("abc")  # notlower('abc') = 0

# Start 3 searches rightwards from position 3, which holds the lowercase 'c',
# so the result is 0.
sas_f_notlower("aBc", 3)  # notlower('aBc', 3) = 0
