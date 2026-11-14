# LEFT: each call is explained above it and followed by its result

# LEFT moves leading blanks to the end; trailing blanks are not shown, so the
# result reads 'abc'.
sas_f_left("  abc")  # left('  abc') = "abc"

# A string without leading blanks is unchanged.
sas_f_left("abc")  # left('abc') = "abc"

# A blank string stays blank.
sas_f_left("   ")  # left('   ') = ""

# LEFT keeps the length: the two leading blanks move to the end, so LENGTHC is
# still 4.
sas_f_lengthc(sas_f_left("  ab"))  # lengthc(left('  ab')) = 4
