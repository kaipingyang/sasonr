# RIGHT: each call is explained above it and followed by the result of the same call in SAS 9.4

# RIGHT moves trailing blanks to the front, right-aligning the text.
sas_f_right("abc  ")  # SAS 9.4: right('abc  ') = "  abc"

# A string without trailing blanks is unchanged.
sas_f_right("  abc")  # SAS 9.4: right('  abc') = "  abc"

# A blank string stays blank.
sas_f_right(" ")  # SAS 9.4: right('') = ""
