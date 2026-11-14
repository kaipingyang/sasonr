# TRIMN: each call is explained above it and followed by its result

# TRIMN removes trailing blanks.
sas_f_trimn("abc  ")  # trimn('abc  ') = "abc"

# Leading blanks are kept.
sas_f_trimn("  a")  # trimn('  a') = "  a"

# Unlike TRIM, TRIMN returns a zero-length string for an all-blank value, so
# LENGTHC is 0.
sas_f_lengthc(sas_f_trimn("   "))  # lengthc(trimn('   ')) = 0
