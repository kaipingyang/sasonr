data _null_; x=5; select; when (x<3) g='L'; when (x<10) g='M'; otherwise g='H'; end;
if x>1 then if x>10 then h=1; else h=2; put g= h=; run;
