# LEFT: each call is explained above it and followed by the result of the same call in SAS 9.4

# LEFT moves leading blanks to the end; trailing blanks are not shown, so the
# result reads 'abc'.
sas_f_left("  abc")  # SAS 9.4: left('  abc') = "abc"

# A string without leading blanks is unchanged.
sas_f_left("abc")  # SAS 9.4: left('abc') = "abc"

# A blank string stays blank.
sas_f_left("   ")  # SAS 9.4: left('   ') = ""

# LEFT keeps the length: the two leading blanks move to the end, so LENGTHC is
# still 4.
sas_f_lengthc(sas_f_left("  ab"))  # SAS 9.4: lengthc(left('  ab')) = 4
