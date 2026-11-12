# PRXCHANGE: each call is explained above it and followed by the result of the same call in SAS 9.4

# Times -1 applies the Perl substitution s/a/X/ to every match.
sas_f_prxchange("s/a/X/", -1, "banana")  # SAS 9.4: prxchange('s/a/X/', -1, 'banana') = "bXnXnX"

# Times 1 replaces only the first match.
sas_f_prxchange("s/a/X/", 1, "banana")  # SAS 9.4: prxchange('s/a/X/', 1, 'banana') = "bXnana"

# The capture groups $1 and $2 swap the two words.
sas_f_prxchange("s/(\\w+) (\\w+)/$2 $1/", -1, "hello world")  # SAS 9.4: prxchange('s/(\w+) (\w+)/$2 $1/', -1, 'hello world') = "world hello"

# The i option after the pattern makes matching case-insensitive.
sas_f_prxchange("s/x/y/i", -1, "XxX")  # SAS 9.4: prxchange('s/x/y/i', -1, 'XxX') = "yyy"
