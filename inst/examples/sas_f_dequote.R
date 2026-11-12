# DEQUOTE: each call is explained above it and followed by the result of the same call in SAS 9.4

# DEQUOTE removes the enclosing double quotation marks.
sas_f_dequote("\"abc\"")  # SAS 9.4: dequote('"abc"') = "abc"

# Inside single quotes a doubled quote '' stands for one quote, so the result
# is a'b.
sas_f_dequote("'a''b'")  # SAS 9.4: dequote("'a''b'") = "a'b"

# A string that does not start with a quotation mark is returned unchanged.
sas_f_dequote("abc")  # SAS 9.4: dequote('abc') = "abc"

# Text after the closing quotation mark is discarded, so only 'ab' remains.
sas_f_dequote("\"ab\"cd")  # SAS 9.4: dequote('"ab"cd') = "ab"
