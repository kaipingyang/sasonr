data _null_; array a{*} x1-x3 (1 2 3); array t{3} _temporary_ (10 20 30); tot=0;
do i=1 to dim(a); tot+a{i}*t{i}; end; put tot=; run;
