data a; input g $ v; datalines;
A 1
B 2
A 3
;
run;
proc sql noprint;
  select distinct g into :glist separated by ',' from a;
  create table s as select g, sum(v) as tot, calculated tot * 2 as dbl,
         case when calculated tot > 3 then 'H' else 'L' end as cls
    from a group by g;
quit;
%put R=&glist;
data _null_; set s; put g= tot= dbl= cls=; run;
