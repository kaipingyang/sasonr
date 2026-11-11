# NOTLOWER: each call is followed by the result of the same call in SAS 9.4
sas_f_notlower("abC")  # SAS: notlower('abC') -> 3
sas_f_notlower("abc")  # SAS: notlower('abc') -> 0
sas_f_notlower("aBc", 3)  # SAS: notlower('aBc', 3) -> 0
