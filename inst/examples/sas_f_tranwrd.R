# TRANWRD: each call is explained above it and followed by the result of the same call in SAS 9.4

# TRANWRD replaces every occurrence of the substring 'an' by 'AN'.
sas_f_tranwrd("banana", "an", "AN")  # SAS 9.4: tranwrd('banana', 'an', 'AN') = "bANANa"

# Every blank is replaced by '-'.
sas_f_tranwrd("a b c", " ", "-")  # SAS 9.4: tranwrd('a b c', ' ', '-') = "a-b-c"

# The replacement '' is one blank in SAS, so 'b' becomes a blank (use TRANSTRN
# with TRIMN to delete it).
sas_f_tranwrd("abc", "b", " ")  # SAS 9.4: tranwrd('abc', 'b', '') = "a c"
