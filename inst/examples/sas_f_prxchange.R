# PRXCHANGE: each call is followed by the result of the same call in SAS 9.4
sas_f_prxchange("s/a/X/", -1, "banana")  # SAS: prxchange('s/a/X/', -1, 'banana') -> "bXnXnX"
sas_f_prxchange("s/a/X/", 1, "banana")  # SAS: prxchange('s/a/X/', 1, 'banana') -> "bXnana"
sas_f_prxchange("s/(\\w+) (\\w+)/$2 $1/", -1, "hello world")  # SAS: prxchange('s/(\w+) (\w+)/$2 $1/', -1, 'hello world') -> "world hello"
sas_f_prxchange("s/x/y/i", -1, "XxX")  # SAS: prxchange('s/x/y/i', -1, 'XxX') -> "yyy"
