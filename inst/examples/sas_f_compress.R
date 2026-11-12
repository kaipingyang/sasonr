# COMPRESS: each call is explained above it and followed by the result of the same call in SAS 9.4
# (dates are SAS day counts from 1960-01-01)

# With one argument COMPRESS removes all blanks.
sas_f_compress("a b c")  # SAS 9.4: compress('a b c') = "abc"

# The second argument lists the characters to remove: here '-'.
sas_f_compress("a-b-c", "-")  # SAS 9.4: compress('a-b-c','-') = "abc"

# The list '' is one blank and modifier 'd' adds the digits: blanks and digits
# are removed.
sas_f_compress("a1 b2", "", "d")  # SAS 9.4: compress('a1 b2','','d') = "ab"

# Modifier 'k' keeps instead of removing: the listed blank and the digits ('d')
# are kept.
sas_f_compress("a1 b2", "", "kd")  # SAS 9.4: compress('a1 b2','','kd') = "1 2"

# Modifier 'i' ignores case: removing 'a' also removes 'A'.
sas_f_compress("ABC abc", "a", "i")  # SAS 9.4: compress('ABC abc','a','i') = "BC bc"
