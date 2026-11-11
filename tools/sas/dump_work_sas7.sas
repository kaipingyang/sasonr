/* sasonr conformance epilogue: keep every WORK data set as SAS writes it.
   <MEM>.sas7bdat  copied unchanged from WORK (exact values, lengths, attributes)
   __attr.csv      memname, varnum, name, type, length, format, informat, label
   The target directory is the macro variable __sasonr_dsdir (set by the runner). */
options nonotes nosource nomprint nosymbolgen nomlogic obs=max replace;
%macro __sasonr_dump(dir);
  %local n;
  libname __sds "&dir";
  proc sql noprint;
    select count(*) into :n trimmed from dictionary.tables
      where libname = 'WORK' and memtype = 'DATA' and memname not like '__SASONR%';
    create table work.__sasonr_attr as
      select memname, varnum, name, type, length, format, informat, label
      from dictionary.columns where libname = 'WORK' and memname not like '__SASONR%'
      order by memname, varnum;
  quit;
  %if &n > 0 %then %do;
    proc copy in=work out=__sds memtype=data;
      exclude __sasonr_attr;
    run;
  %end;
  proc export data=work.__sasonr_attr outfile="&dir/__attr.csv" dbms=csv replace; run;
  libname __sds clear;
%mend;
%__sasonr_dump(&__sasonr_dsdir)
