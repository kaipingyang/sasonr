data a; x=1;
data b; set a; y=x+1;
data _null_; set b; put x= y=;
run;
