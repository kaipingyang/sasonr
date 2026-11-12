# TRIMN: each call is explained above it and followed by the result of the same call in SAS 9.4

# TRIMN removes trailing blanks.
sas_f_trimn("abc  ")  # SAS 9.4: trimn('abc  ') = "abc"

# Leading blanks are kept.
sas_f_trimn("  a")  # SAS 9.4: trimn('  a') = "  a"

# Unlike TRIM, TRIMN returns a zero-length string for an all-blank value, so
# LENGTHC is 0.
sas_f_lengthc(sas_f_trimn("   "))  # SAS 9.4: lengthc(trimn('   ')) = 0
