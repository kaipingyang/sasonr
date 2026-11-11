data a; input g $ h $ v; datalines;
A x 1
A x 2
A y 3
B y 4
;
run;
data _null_; set a; by g h; fg=first.g; fh=first.h; lh=last.h; put g= h= fg= fh= lh=; run;
