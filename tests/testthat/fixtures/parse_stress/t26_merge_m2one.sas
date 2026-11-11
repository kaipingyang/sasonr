data h; input id v; datalines;
1 10
2 20
;
run;
data d; input id w; datalines;
1 1
1 2
2 3
;
run;
data m; merge d h; by id; if w=2 then v=99; run;
data _null_; set m; put id= w= v=; run;
