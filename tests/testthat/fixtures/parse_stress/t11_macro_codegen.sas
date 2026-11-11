%macro m; %do i=1 %to 3; data d&i; x=&i; run; %end; %mend m;
%m
data all; set d1-d3; run;
data _null_; set all end=e; s+x; if e then put s=; run;
