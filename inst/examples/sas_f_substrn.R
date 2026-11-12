# SUBSTRN: each call is explained above it and followed by the result of the same call in SAS 9.4

# SUBSTRN extracts 3 characters starting at position 2.
sas_f_substrn("abcdef", 2, 3)  # SAS 9.4: substrn('abcdef', 2, 3) = "bcd"

# Position 0 lies before the string; of positions 0 to 2 only 1 and 2 are
# inside it, giving 'ab'.
sas_f_substrn("abcdef", 0, 3)  # SAS 9.4: substrn('abcdef', 0, 3) = "ab"

# A length that runs past the end is cut at the end of the string: 'ef'.
sas_f_substrn("abcdef", 5, 10)  # SAS 9.4: substrn('abcdef', 5, 10) = "ef"

# A start position beyond the end gives an empty result, without the note that
# SUBSTR would write.
sas_f_substrn("abc", 4)  # SAS 9.4: substrn('abc', 4) = ""
