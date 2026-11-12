# NOTLOWER: each call is explained above it and followed by the result of the same call in SAS 9.4

# The first character that is not a lowercase letter is 'C' at position 3.
sas_f_notlower("abC")  # SAS 9.4: notlower('abC') = 3

# All characters are lowercase letters, so the result is 0.
sas_f_notlower("abc")  # SAS 9.4: notlower('abc') = 0

# Start 3 searches rightwards from position 3, which holds the lowercase 'c',
# so the result is 0.
sas_f_notlower("aBc", 3)  # SAS 9.4: notlower('aBc', 3) = 0
