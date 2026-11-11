# NOTSPACE: each call is followed by the result of the same call in SAS 9.4
sas_f_notspace("  ab")  # SAS: notspace('  ab') -> 3
sas_f_notspace("   ")  # SAS: notspace('   ') -> 0
sas_f_notspace("a  b", 2)  # SAS: notspace('a  b', 2) -> 4
