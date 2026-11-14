# TRIM: each call is explained above it and followed by its result

# TRIM removes trailing blanks.
sas_f_trim("abc  ")  # trim('abc  ') = "abc"

# Leading blanks are kept.
sas_f_trim("  a")  # trim('  a') = "  a"

# TRIM of an all-blank string returns one blank, so LENGTHC is 1.
sas_f_lengthc(sas_f_trim("   "))  # lengthc(trim('   ')) = 1
