# ANYALNUM: each call is followed by the result of the same call in SAS 9.4
sas_f_anyalnum("  ab1")  # SAS: anyalnum('  ab1') -> 3
sas_f_anyalnum("!!!")  # SAS: anyalnum('!!!') -> 0
sas_f_anyalnum("a1!", -3)  # SAS: anyalnum('a1!', -3) -> 2
sas_f_anyalnum("!a!", 3)  # SAS: anyalnum('!a!', 3) -> 0
