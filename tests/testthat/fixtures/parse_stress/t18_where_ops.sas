data a; input n $ v; datalines;
abc 1
xbz 3
ybb 4
qqq 3
;
run;
data _null_; set a; where v between 2 and 4 and n contains 'b'; put n= v=; run;
