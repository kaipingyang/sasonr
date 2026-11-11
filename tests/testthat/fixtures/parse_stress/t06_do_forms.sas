data _null_; t=0;
do i=1 to 5; if i=2 then continue; do j=1 to 3 by 2; t+i*j; end; if i=4 then leave; end;
put t= i= j=;
k=0; do while(k<3); k+1; end; m=10; do until(m>12); m+1; end; put k= m=; run;
