# STRIP: each call is explained above it and followed by the result of the same call in SAS 9.4

# STRIP removes leading and trailing blanks.
sas_f_strip("  a b  ")  # SAS 9.4: strip('  a b  ') = "a b"

# A blank string stays blank.
sas_f_strip(" ")  # SAS 9.4: strip('') = ""

# A string without leading or trailing blanks is unchanged.
sas_f_strip("abc")  # SAS 9.4: strip('abc') = "abc"

# Stripping an all-blank string gives a zero-length result, so LENGTHC is 0.
sas_f_lengthc(sas_f_strip("   "))  # SAS 9.4: lengthc(strip('   ')) = 0
