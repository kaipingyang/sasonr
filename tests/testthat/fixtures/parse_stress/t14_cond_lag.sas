data a; do i=1 to 5; output; end; run;
data _null_; set a; if mod(i,2)=1 then l=lag(i); put i= l=; run;
