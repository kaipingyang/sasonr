data _null_; x=.; a = (x < 0); b = (x = .); c = (.A < .); d = (._ < .); e = min(x, 3); f = x + 1; g = sum(x, 1); put a= b= c= d= e= f= g=; run;
