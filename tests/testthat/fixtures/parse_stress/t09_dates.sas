data _null_; d='15MAR2026'd; t='10:30't; n=intck('month','01JAN2026'd, d); e=intnx('month', d, 1, 'b');
put d= date9. n= e= yymmdd10. t= time5.; run;
