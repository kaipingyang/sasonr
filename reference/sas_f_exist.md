# SAS `EXIST(member-name <, member-type>)`

Whether a SAS library member exists (1) or not (0). See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - name:
    
    Name.

  - type:
    
    Argument `type` of the SAS function.

  - .work:
    
    The WORK library to look in: an environment whose objects are the
    WORK data sets (for example `list2env(res$work)` for a result `res`
    of `sas_run()`; translated code passes the one of the running
    program). Without it a WORK member fails loud; SASHELP members are
    looked up in the SASHELP directory.

  - .at:
    
    Internal: source position `c(line, column)` used in log NOTEs.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Implements the SAS DATA step semantics (log NOTEs, `_ERROR_`), which
base R has no counterpart for.

## See also

Other special functions: `sas_f_band()`, `sas_f_blshift()`,
`sas_f_bnot()`, `sas_f_bor()`, `sas_f_brshift()`, `sas_f_bxor()`,
`sas_f_input()`, `sas_f_inputc()`, `sas_f_inputn()`, `sas_f_missing()`,
`sas_f_nvalid()`, `sas_f_prxchange()`, `sas_f_prxmatch()`,
`sas_f_prxparse()`, `sas_f_put()`, `sas_f_putc()`, `sas_f_putn()`
