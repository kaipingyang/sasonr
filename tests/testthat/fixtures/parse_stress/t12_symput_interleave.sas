%macro chk;
  data _null_; call symputx('nn', 5, 'G'); run;
  %if &nn > 3 %then %put R=BIG; %else %put R=SMALL;
%mend chk;
%chk
