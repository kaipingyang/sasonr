# Package index

## Run and translate SAS programs

Entry points. Run a SAS program on the R engine or translate it into
tidyverse R code.

<!-- end list -->

  - `sas_run()` : Run a SAS program on the R engine
  - `sas_translate()` : Translate a SAS program into R (tidyverse) code
  - `sas_check()` : List the unsupported features of a SAS program
  - `sas_session()` : Create a persistent session
  - `sas_work()` `sas_session_add()` `sas_meta()` `sas_macro_vars()`
    `sas_listing()` `sas_session_close()` : Inspect a session
  - `sas_log()` : The log of a result or a session
  - `sas_html()` : The HTML document of a result
  - `sas_export_work()` : Write WORK data sets as CSV files with an
    attribute table
  - `sasonr_strict` `sasonr.strict` : Strict mode: approximate and
    unsupported features fail loud

## The translation as an R script

A runnable script for a translation or for the trace of a run, and the
step verbs it calls.

<!-- end list -->

  - `sas_script()` : The translation of a SAS program as an R script
  - `sas_data_step()` `sas_proc_sort()` `sas_proc_means()`
    `sas_proc_freq()` `sas_proc_transpose()` `sas_proc_print()`
    `sas_proc_tabulate()` `sas_proc_sql()` `sas_proc()` `sas_options()`
    `sas_title()` `sas_global()` `sas_deferred()` `sas_macro_put()` :
    Run one SAS step in a session
  - `sasonr_tidy_log()` : Show tidylog messages for an expression

## Procedures on data frames

Run one procedure on a data frame without writing a SAS program.

<!-- end list -->

  - `sas_means_df()` `sas_tabulate_df()` `sas_freq_df()`
    `sas_transpose_df()` : Data-level PROC MEANS, PROC TABULATE, PROC
    FREQ and PROC TRANSPOSE

## Front end (lexer, parser, analysis)

<!-- end list -->

  - `sas_lex()` : Tokenize SAS source code
  - `sas_parse()` : Parse a SAS program
  - `sas_parse_expr()` : Parse a single SAS expression
  - `sas_analyze()` : Analyse a parsed DATA step

## SAS values

<!-- end list -->

  - `sas_missing()` : SAS missing value

## SAS character functions

Implementations used by translated code; each page gives the SAS syntax
and the differences from base R.

<!-- end list -->

  - `sas_f_anyalnum` :
    
    SAS `ANYALNUM(string <, start>)`

  - `sas_f_anyalpha` :
    
    SAS `ANYALPHA(string <, start>)`

  - `sas_f_anydigit` :
    
    SAS `ANYDIGIT(string <, start>)`

  - `sas_f_anylower` :
    
    SAS `ANYLOWER(string <, start>)`

  - `sas_f_anyprint` :
    
    SAS `ANYPRINT(string <, start>)`

  - `sas_f_anypunct` :
    
    SAS `ANYPUNCT(string <, start>)`

  - `sas_f_anyspace` :
    
    SAS `ANYSPACE(string <, start>)`

  - `sas_f_anyupper` :
    
    SAS `ANYUPPER(string <, start>)`

  - `sas_f_byte` :
    
    SAS `BYTE(n)`

  - `sas_f_cat` :
    
    SAS `CAT(item-1 <, ..., item-n>)`

  - `sas_f_cats` :
    
    SAS `CATS(item-1 <, ..., item-n>)`

  - `sas_f_catt` :
    
    SAS `CATT(item-1 <, ..., item-n>)`

  - `sas_f_catx` :
    
    SAS `CATX(delimiter, item-1 <, ..., item-n>)`

  - `sas_f_char` :
    
    SAS `CHAR(string, position)`

  - `sas_f_choosec` :
    
    SAS `CHOOSEC(index, value-1 <, ..., value-n>)`

  - `sas_f_coalescec` :
    
    SAS `COALESCEC(argument-1 <, ..., argument-n>)`

  - `sas_f_collate` :
    
    SAS `COLLATE(start-position <, end-position> | <, , length>)`

  - `sas_f_compare` :
    
    SAS `COMPARE(string-1, string-2 <, modifiers>)`

  - `sas_f_compbl` :
    
    SAS `COMPBL(source)`

  - `sas_f_compress` :
    
    SAS `COMPRESS(source <, characters> <, modifiers>)`

  - `sas_f_count` :
    
    SAS `COUNT(string, substring <, modifiers>)`

  - `sas_f_countc` :
    
    SAS `COUNTC(string, characters <, modifiers>)`

  - `sas_f_countw` :
    
    SAS `COUNTW(string <, delimiters> <, modifiers>)`

  - `sas_f_dequote` :
    
    SAS `DEQUOTE(string)`

  - `sas_f_find` :
    
    SAS `FIND(string, substring <, modifiers> <, start>)`

  - `sas_f_findc` :
    
    SAS `FINDC(string <, characters> <, modifiers> <, start>)`

  - `sas_f_findw` :
    
    SAS `FINDW(string, word <, delimiters> <, modifiers> <, start>)`

  - `sas_f_first` :
    
    SAS `FIRST(string)`

  - `sas_f_ifc` :
    
    SAS `IFC(condition, true, false <, missing>)`

  - `sas_f_index` :
    
    SAS `INDEX(source, excerpt)`

  - `sas_f_indexc` :
    
    SAS `INDEXC(source, excerpt-1 <, ..., excerpt-n>)`

  - `sas_f_indexw` :
    
    SAS `INDEXW(source, excerpt <, delimiters>)`

  - `sas_f_klength` :
    
    SAS `KLENGTH(string)`

  - `sas_f_ksubstr` :
    
    SAS `KSUBSTR(string, position <, length>)`

  - `sas_f_left` :
    
    SAS `LEFT(string)`

  - `sas_f_length` :
    
    SAS `LENGTH(string)`

  - `sas_f_lengthc` :
    
    SAS `LENGTHC(string)`

  - `sas_f_lengthm` :
    
    SAS `LENGTHM(string)`

  - `sas_f_lengthn` :
    
    SAS `LENGTHN(string)`

  - `sas_f_lowcase` :
    
    SAS `LOWCASE(argument)`

  - `sas_f_notalnum` :
    
    SAS `NOTALNUM(string <, start>)`

  - `sas_f_notalpha` :
    
    SAS `NOTALPHA(string <, start>)`

  - `sas_f_notdigit` :
    
    SAS `NOTDIGIT(string <, start>)`

  - `sas_f_notlower` :
    
    SAS `NOTLOWER(string <, start>)`

  - `sas_f_notprint` :
    
    SAS `NOTPRINT(string <, start>)`

  - `sas_f_notpunct` :
    
    SAS `NOTPUNCT(string <, start>)`

  - `sas_f_notspace` :
    
    SAS `NOTSPACE(string <, start>)`

  - `sas_f_notupper` :
    
    SAS `NOTUPPER(string <, start>)`

  - `sas_f_propcase` :
    
    SAS `PROPCASE(argument <, delimiters>)`

  - `sas_f_quote` :
    
    SAS `QUOTE(argument <, character>)`

  - `sas_f_rank` :
    
    SAS `RANK(x)`

  - `sas_f_repeat` :
    
    SAS `REPEAT(argument, n)`

  - `sas_f_reverse` :
    
    SAS `REVERSE(argument)`

  - `sas_f_right` :
    
    SAS `RIGHT(argument)`

  - `sas_f_scan` :
    
    SAS `SCAN(string, count <, character-list <, modifiers>>)`

  - `sas_f_strip` :
    
    SAS `STRIP(string)`

  - `sas_f_substr` :
    
    SAS `SUBSTR(string, position <, length>)`

  - `sas_f_substrn` :
    
    SAS `SUBSTRN(string, position <, length>)`

  - `sas_f_translate` :
    
    SAS `TRANSLATE(source, to-1, from-1 <, ..., to-n, from-n>)`

  - `sas_f_transtrn` :
    
    SAS `TRANSTRN(source, target, replacement)`

  - `sas_f_tranwrd` :
    
    SAS `TRANWRD(source, target, replacement)`

  - `sas_f_trim` :
    
    SAS `TRIM(argument)`

  - `sas_f_trimn` :
    
    SAS `TRIMN(argument)`

  - `sas_f_upcase` :
    
    SAS `UPCASE(argument)`

  - `sas_f_verify` :
    
    SAS `VERIFY(source, excerpt-1 <, ..., excerpt-n>)`

  - `sas_f_whichc` :
    
    SAS `WHICHC(string, value-1 <, ..., value-n>)`

## SAS numeric, truncation and rounding functions

Implementations used by translated code; each page gives the SAS syntax
and the differences from base R.

<!-- end list -->

  - `sas_f_abs` :
    
    SAS `ABS(argument)`

  - `sas_f_arcos` :
    
    SAS `ARCOS(argument)`

  - `sas_f_arsin` :
    
    SAS `ARSIN(argument)`

  - `sas_f_atan` :
    
    SAS `ATAN(argument)`

  - `sas_f_atan2` :
    
    SAS `ATAN2(argument-1, argument-2)`

  - `sas_f_beta` :
    
    SAS `BETA(a, b)`

  - `sas_f_ceil` :
    
    SAS `CEIL(argument)`

  - `sas_f_ceilz` :
    
    SAS `CEILZ(argument)`

  - `sas_f_choosen` :
    
    SAS `CHOOSEN(index, value-1 <, ..., value-n>)`

  - `sas_f_coalesce` :
    
    SAS `COALESCE(argument-1 <, ..., argument-n>)`

  - `sas_f_comb` :
    
    SAS `COMB(n, r)`

  - `sas_f_constant` :
    
    SAS `CONSTANT(constant <, parameter>)`

  - `sas_f_cos` :
    
    SAS `COS(argument)`

  - `sas_f_cosh` :
    
    SAS `COSH(argument)`

  - `sas_f_divide` :
    
    SAS `DIVIDE(x, y)`

  - `sas_f_exp` :
    
    SAS `EXP(argument)`

  - `sas_f_fact` :
    
    SAS `FACT(n)`

  - `sas_f_floor` :
    
    SAS `FLOOR(argument)`

  - `sas_f_floorz` :
    
    SAS `FLOORZ(argument)`

  - `sas_f_gamma` :
    
    SAS `GAMMA(argument)`

  - `sas_f_gcd` :
    
    SAS `GCD(x-1 <, ..., x-n>)`

  - `sas_f_ifn` :
    
    SAS `IFN(condition, true, false <, missing>)`

  - `sas_f_int` :
    
    SAS `INT(argument)`

  - `sas_f_intz` :
    
    SAS `INTZ(argument)`

  - `sas_f_lcm` :
    
    SAS `LCM(x-1 <, ..., x-n>)`

  - `sas_f_lgamma` :
    
    SAS `LGAMMA(argument)`

  - `sas_f_log` :
    
    SAS `LOG(argument)`

  - `sas_f_log10` :
    
    SAS `LOG10(argument)`

  - `sas_f_log2` :
    
    SAS `LOG2(argument)`

  - `sas_f_logbeta` :
    
    SAS `LOGBETA(a, b)`

  - `sas_f_mod` :
    
    SAS `MOD(dividend, divisor)`

  - `sas_f_modz` :
    
    SAS `MODZ(dividend, divisor)`

  - `sas_f_perm` :
    
    SAS `PERM(n <, r>)`

  - `sas_f_round` :
    
    SAS `ROUND(argument <, rounding-unit>)`

  - `sas_f_rounde` :
    
    SAS `ROUNDE(argument <, rounding-unit>)`

  - `sas_f_roundz` :
    
    SAS `ROUNDZ(argument <, rounding-unit>)`

  - `sas_f_sign` :
    
    SAS `SIGN(argument)`

  - `sas_f_sin` :
    
    SAS `SIN(argument)`

  - `sas_f_sinh` :
    
    SAS `SINH(argument)`

  - `sas_f_sqrt` :
    
    SAS `SQRT(argument)`

  - `sas_f_tan` :
    
    SAS `TAN(argument)`

  - `sas_f_tanh` :
    
    SAS `TANH(argument)`

  - `sas_f_whichn` :
    
    SAS `WHICHN(argument, value-1 <, ..., value-n>)`

## SAS descriptive statistics functions

Implementations used by translated code; each page gives the SAS syntax
and the differences from base R.

<!-- end list -->

  - `sas_f_cmiss` :
    
    SAS `CMISS(argument-1 <, ..., argument-n>)`

  - `sas_f_css` :
    
    SAS `CSS(argument-1 <, ..., argument-n>)`

  - `sas_f_cv` :
    
    SAS `CV(argument-1 <, ..., argument-n>)`

  - `sas_f_geomean` :
    
    SAS `GEOMEAN(argument-1 <, ..., argument-n>)`

  - `sas_f_harmean` :
    
    SAS `HARMEAN(argument-1 <, ..., argument-n>)`

  - `sas_f_kurtosis` :
    
    SAS `KURTOSIS(argument-1, ..., argument-n)`

  - `sas_f_largest` :
    
    SAS `LARGEST(k, value-1 <, ..., value-n>)`

  - `sas_f_max` :
    
    SAS `MAX(argument-1 <, ..., argument-n>)`

  - `sas_f_mean` :
    
    SAS `MEAN(argument-1 <, ..., argument-n>)`

  - `sas_f_median` :
    
    SAS `MEDIAN(value-1 <, ..., value-n>)`

  - `sas_f_min` :
    
    SAS `MIN(argument-1 <, ..., argument-n>)`

  - `sas_f_n` :
    
    SAS `N(argument-1 <, ..., argument-n>)`

  - `sas_f_nmiss` :
    
    SAS `NMISS(argument-1 <, ..., argument-n>)`

  - `sas_f_ordinal` :
    
    SAS `ORDINAL(k, argument-1 <, ..., argument-n>)`

  - `sas_f_pctl` :
    
    SAS `PCTL(percentage, value-1 <, ..., value-n>)`

  - `sas_f_range` :
    
    SAS `RANGE(argument-1 <, ..., argument-n>)`

  - `sas_f_skewness` :
    
    SAS `SKEWNESS(argument-1, argument-2, argument-3 <, ...,
    argument-n>)`

  - `sas_f_smallest` :
    
    SAS `SMALLEST(k, value-1 <, ..., value-n>)`

  - `sas_f_std` :
    
    SAS `STD(argument-1, argument-2 <, ..., argument-n>)`

  - `sas_f_stderr` :
    
    SAS `STDERR(argument-1, argument-2 <, ..., argument-n>)`

  - `sas_f_sum` :
    
    SAS `SUM(argument-1 <, ..., argument-n>)`

  - `sas_f_uss` :
    
    SAS `USS(argument-1 <, ..., argument-n>)`

  - `sas_f_var` :
    
    SAS `VAR(argument-1, argument-2 <, ..., argument-n>)`

## SAS probability and distribution functions

Implementations used by translated code; each page gives the SAS syntax
and the differences from base R.

<!-- end list -->

  - `sas_f_cdf` :
    
    SAS `CDF(distribution, quantile <, parameters>)`

  - `sas_f_cinv` :
    
    SAS `CINV(p, df <, nc>)`

  - `sas_f_finv` :
    
    SAS `FINV(p, ndf, ddf <, nc>)`

  - `sas_f_logcdf` :
    
    SAS `LOGCDF(distribution, quantile <, parameters>)`

  - `sas_f_logpdf` :
    
    SAS `LOGPDF(distribution, quantile <, parameters>)`

  - `sas_f_logsdf` :
    
    SAS `LOGSDF(distribution, quantile <, parameters>)`

  - `sas_f_pdf` :
    
    SAS `PDF(distribution, quantile <, parameters>)`

  - `sas_f_poisson` :
    
    SAS `POISSON(m, n)`

  - `sas_f_probbnml` :
    
    SAS `PROBBNML(p, n, m)`

  - `sas_f_probchi` :
    
    SAS `PROBCHI(x, df <, nc>)`

  - `sas_f_probf` :
    
    SAS `PROBF(x, ndf, ddf <, nc>)`

  - `sas_f_probit` :
    
    SAS `PROBIT(p)`

  - `sas_f_probnorm` :
    
    SAS `PROBNORM(x)`

  - `sas_f_probt` :
    
    SAS `PROBT(x, df <, nc>)`

  - `sas_f_quantile` :
    
    SAS `QUANTILE(distribution, probability <, parameters>)`

  - `sas_f_sdf` :
    
    SAS `SDF(distribution, quantile <, parameters>)`

  - `sas_f_tinv` :
    
    SAS `TINV(p, df <, nc>)`

## SAS date and time functions

Implementations used by translated code; each page gives the SAS syntax
and the differences from base R.

<!-- end list -->

  - `sas_f_datdif` :
    
    SAS `DATDIF(sdate, edate, basis)`

  - `sas_f_date` :
    
    SAS `DATE()`

  - `sas_f_datejul` :
    
    SAS `DATEJUL(julian-date)`

  - `sas_f_datepart` :
    
    SAS `DATEPART(datetime)`

  - `sas_f_datetime` :
    
    SAS `DATETIME()`

  - `sas_f_day` :
    
    SAS `DAY(date)`

  - `sas_f_dhms` :
    
    SAS `DHMS(date, hour, minute, second)`

  - `sas_f_hms` :
    
    SAS `HMS(hour, minute, second)`

  - `sas_f_hour` :
    
    SAS `HOUR(time | datetime)`

  - `sas_f_intck` :
    
    SAS `INTCK(interval, start, end <, method>)`

  - `sas_f_intnx` :
    
    SAS `INTNX(interval, start, increment <, alignment>)`

  - `sas_f_juldate` :
    
    SAS `JULDATE(date)`

  - `sas_f_juldate7` :
    
    SAS `JULDATE7(date)`

  - `sas_f_mdy` :
    
    SAS `MDY(month, day, year)`

  - `sas_f_minute` :
    
    SAS `MINUTE(time | datetime)`

  - `sas_f_month` :
    
    SAS `MONTH(date)`

  - `sas_f_qtr` :
    
    SAS `QTR(date)`

  - `sas_f_second` :
    
    SAS `SECOND(time | datetime)`

  - `sas_f_time` :
    
    SAS `TIME()`

  - `sas_f_timepart` :
    
    SAS `TIMEPART(datetime)`

  - `sas_f_today` :
    
    SAS `TODAY()`

  - `sas_f_week` :
    
    SAS `WEEK(<date <, descriptor>>)`

  - `sas_f_weekday` :
    
    SAS `WEEKDAY(date)`

  - `sas_f_year` :
    
    SAS `YEAR(date)`

  - `sas_f_yrdif` :
    
    SAS `YRDIF(sdate, edate <, basis>)`

## SAS special functions (PUT/INPUT, PRX, bitwise, MISSING)

Implementations used by translated code; each page gives the SAS syntax
and the differences from base R.

<!-- end list -->

  - `sas_f_band` :
    
    SAS `BAND(argument-1, argument-2)`

  - `sas_f_blshift` :
    
    SAS `BLSHIFT(argument-1, argument-2)`

  - `sas_f_bnot` :
    
    SAS `BNOT(argument)`

  - `sas_f_bor` :
    
    SAS `BOR(argument-1, argument-2)`

  - `sas_f_brshift` :
    
    SAS `BRSHIFT(argument-1, argument-2)`

  - `sas_f_bxor` :
    
    SAS `BXOR(argument-1, argument-2)`

  - `sas_f_input` :
    
    SAS `INPUT(source, <? | ??> informat.)`

  - `sas_f_inputc` :
    
    SAS `INPUTC(source, informat <, w>)`

  - `sas_f_inputn` :
    
    SAS `INPUTN(source, informat <, w <, d>>)`

  - `sas_f_missing` :
    
    SAS `MISSING(numeric-expression | character-expression)`

  - `sas_f_nvalid` :
    
    SAS `NVALID(string <, validvarname>)`

  - `sas_f_prxchange` :
    
    SAS `PRXCHANGE(perl-regular-expression, times, source)`

  - `sas_f_prxmatch` :
    
    SAS `PRXMATCH(regular-expression-id | perl-regular-expression,
    source)`

  - `sas_f_prxparse` :
    
    SAS `PRXPARSE(perl-regular-expression)`

  - `sas_f_put` :
    
    SAS `PUT(source, format.)`

  - `sas_f_putc` :
    
    SAS `PUTC(source, format <, w>)`

  - `sas_f_putn` :
    
    SAS `PUTN(source, format <, w <, d>>)`

## SAS functions related to macro variables

Implementations used by translated code; each page gives the SAS syntax
and the differences from base R.

<!-- end list -->

  - `sas_f_symget` :
    
    SAS `SYMGET(argument)`

## SAS file and library functions

Implementations used by translated code; each page gives the SAS syntax
and the differences from base R.

<!-- end list -->

  - `sas_f_exist` :
    
    SAS `EXIST(member-name <, member-type>)`
