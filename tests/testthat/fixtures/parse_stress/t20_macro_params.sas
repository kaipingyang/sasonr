%macro p(a, b=2, c=); %put R=&a|&b|&c|; %mend p;
%p(1)
%p(x, c=z, b=)
%macro q / parmbuff; %put R=&syspbuff; %mend q;
%q(1,2)
