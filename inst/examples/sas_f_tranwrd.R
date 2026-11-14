# TRANWRD: each call is explained above it and followed by its result

# TRANWRD replaces every occurrence of the substring 'an' by 'AN'.
sas_f_tranwrd("banana", "an", "AN")  # tranwrd('banana', 'an', 'AN') = "bANANa"

# Every blank is replaced by '-'.
sas_f_tranwrd("a b c", " ", "-")  # tranwrd('a b c', ' ', '-') = "a-b-c"

# The replacement '' is one blank in SAS, so 'b' becomes a blank (use TRANSTRN
# with TRIMN to delete it).
sas_f_tranwrd("abc", "b", " ")  # tranwrd('abc', 'b', '') = "a c"
