# LEFT: each call is followed by the result of the same call in SAS 9.4
sas_f_left("  abc")  # SAS: left('  abc') -> "abc"
sas_f_left("abc")  # SAS: left('abc') -> "abc"
sas_f_left("   ")  # SAS: left('   ') -> ""
sas_f_lengthc(sas_f_left("  ab"))  # SAS: lengthc(left('  ab')) -> 4
