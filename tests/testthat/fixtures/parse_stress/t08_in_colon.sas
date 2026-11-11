data _null_; s='ABC'; a = s in ('ABC','X'); b = (s =: 'AB'); c = 3 in (1:5); put a= b= c=; run;
