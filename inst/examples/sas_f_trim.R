# TRIM: each call is explained above it and followed by the result of the same call in SAS 9.4

# TRIM removes trailing blanks.
sas_f_trim("abc  ")  # SAS 9.4: trim('abc  ') = "abc"

# Leading blanks are kept.
sas_f_trim("  a")  # SAS 9.4: trim('  a') = "  a"

# TRIM of an all-blank string returns one blank, so LENGTHC is 1.
sas_f_lengthc(sas_f_trim("   "))  # SAS 9.4: lengthc(trim('   ')) = 1
