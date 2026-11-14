# STRIP: each call is explained above it and followed by its result

# STRIP removes leading and trailing blanks.
sas_f_strip("  a b  ")  # strip('  a b  ') = "a b"

# A blank string stays blank.
sas_f_strip(" ")  # strip('') = ""

# A string without leading or trailing blanks is unchanged.
sas_f_strip("abc")  # strip('abc') = "abc"

# Stripping an all-blank string gives a zero-length result, so LENGTHC is 0.
sas_f_lengthc(sas_f_strip("   "))  # lengthc(strip('   ')) = 0
