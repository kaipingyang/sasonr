%let s = Hello World;
%put R1=%upcase(&s) R2=%scan(&s, 2) R3=%substr(&s, 1, 5) R4=%index(&s, World) R5=%length(&s);
%put R6=%sysfunc(compress(a b c));
%macro t; %local x; %let x = %eval(2 + 3 * 4); %put R7=&x; %mend; %t
