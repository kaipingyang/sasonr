data _null_; s=0; do k = 1, 3, 5; s+k; end; length c $3; t=''; do c = 'a', 'b'; t = cats(t, c); end; put s= t=; run;
