# DEQUOTE: each call is explained above it and followed by its result

# DEQUOTE removes the enclosing double quotation marks.
sas_f_dequote("\"abc\"")  # dequote('"abc"') = "abc"

# Inside single quotes a doubled quote '' stands for one quote, so the result
# is a'b.
sas_f_dequote("'a''b'")  # dequote("'a''b'") = "a'b"

# A string that does not start with a quotation mark is returned unchanged.
sas_f_dequote("abc")  # dequote('abc') = "abc"

# Text after the closing quotation mark is discarded, so only 'ab' remains.
sas_f_dequote("\"ab\"cd")  # dequote('"ab"cd') = "ab"
