%let a=1; %let b=a; %put R1=&&&b;
%let i=2; %let v2=hello; %put R2=&&v&i;
%put R3=%str(x;y);
%put R4=%eval(7/2) R5=%sysevalf(7/2);
