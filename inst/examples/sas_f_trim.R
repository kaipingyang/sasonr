# TRIM: each call is followed by the result of the same call in SAS 9.4
sas_f_trim("abc  ")  # SAS: trim('abc  ') -> "abc"
sas_f_trim("  a")  # SAS: trim('  a') -> "  a"
sas_f_lengthc(sas_f_trim("   "))  # SAS: lengthc(trim('   ')) -> 1
