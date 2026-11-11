# PDF: each call is followed by the result of the same call in SAS 9.4
sas_f_pdf("NORMAL", 0)  # SAS: pdf('NORMAL', 0) -> 0.39894228040143
sas_f_pdf("BINOMIAL", 3, 0.5, 10)  # SAS: pdf('BINOMIAL', 3, 0.5, 10) -> 0.1171875
sas_f_pdf("POISSON", 2, 1.5)  # SAS: pdf('POISSON', 2, 1.5) -> 0.25102143016698
sas_f_pdf("UNIFORM", 0.5)  # SAS: pdf('UNIFORM', 0.5) -> 1
sas_f_pdf("NORMAL", NA_real_)  # SAS: pdf('NORMAL', .) -> .
