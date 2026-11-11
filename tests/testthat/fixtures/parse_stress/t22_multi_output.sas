data a; do v=1 to 5; output; end; run;
data hi lo; set a; if v>2 then output hi; else output lo; run;
data _null_; set hi end=e; n+1; if e then do; nhi=n; put nhi=; end; run;
data _null_; set lo end=e; n+1; if e then do; nlo=n; put nlo=; end; run;
