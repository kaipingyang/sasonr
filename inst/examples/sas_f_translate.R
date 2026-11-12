# TRANSLATE: each call is explained above it and followed by the result of the same call in SAS 9.4

# TRANSLATE(source, to, from) replaces each character of from by the character
# at the same position in to: a to x, b to y.
sas_f_translate("abcabc", "xy", "ab")  # SAS 9.4: translate('abcabc', 'xy', 'ab') = "xycxyc"

# Several to/from pairs may be given: a becomes X and b becomes Y.
sas_f_translate("abc", "X", "a", "Y", "b")  # SAS 9.4: translate('abc', 'X', 'a', 'Y', 'b') = "XYc"

# A from character that does not occur leaves the string unchanged.
sas_f_translate("abc", "x", "z")  # SAS 9.4: translate('abc', 'x', 'z') = "abc"
