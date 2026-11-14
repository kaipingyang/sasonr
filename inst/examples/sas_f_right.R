# RIGHT: each call is explained above it and followed by its result

# RIGHT moves trailing blanks to the front, right-aligning the text.
sas_f_right("abc  ")  # right('abc  ') = "  abc"

# A string without trailing blanks is unchanged.
sas_f_right("  abc")  # right('  abc') = "  abc"

# A blank string stays blank.
sas_f_right(" ")  # right('') = ""
