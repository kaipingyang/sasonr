# TRANSLATE: each call is followed by the result of the same call in SAS 9.4
sas_f_translate("abcabc", "xy", "ab")  # SAS: translate('abcabc', 'xy', 'ab') -> "xycxyc"
sas_f_translate("abc", "X", "a", "Y", "b")  # SAS: translate('abc', 'X', 'a', 'Y', 'b') -> "XYc"
sas_f_translate("abc", "x", "z")  # SAS: translate('abc', 'x', 'z') -> "abc"
