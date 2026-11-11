data a; input s $char5.; datalines4;
a;b
;;;;
run;
data _null_; set a; put s=; run;
