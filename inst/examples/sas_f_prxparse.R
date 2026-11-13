# PRXPARSE: each call is explained above it and followed by the result of the same call in SAS 9.4

# Outside sas_run() the PRXPARSE ids are kept in an explicit SAS session.
s <- sas_session()

# PRXPARSE compiles a Perl regular expression and returns a numeric pattern
# identifier; the first one is 1.
sas_f_prxparse("/a+/", session = s)  # SAS 9.4: prxparse('/a+/') = 1

# Each further PRXPARSE call gets the next identifier, 2.
sas_f_prxparse("/b/i", session = s)  # SAS 9.4: prxparse('/b/i') = 2
