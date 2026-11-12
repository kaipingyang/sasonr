# TRANSTRN: each call is explained above it and followed by the result of the same call in SAS 9.4

# TRANSTRN replaces every occurrence of the substring 'an' by 'AN'.
sas_f_transtrn("banana", "an", "AN")  # SAS 9.4: transtrn('banana', 'an', 'AN') = "bANANa"

# TRIMN('') is a zero-length string, so the target 'b' is removed entirely.
sas_f_transtrn("abc", "b", sas_f_trimn(" "))  # SAS 9.4: transtrn('abc', 'b', trimn('')) = "ac"

# The target does not occur, so the string is unchanged.
sas_f_transtrn("abc", "x", "y")  # SAS 9.4: transtrn('abc', 'x', 'y') = "abc"
