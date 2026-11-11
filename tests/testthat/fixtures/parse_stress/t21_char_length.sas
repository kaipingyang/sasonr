data _null_; x='ab'; x='abcd'; if 0 then y='long text'; y='hi'; z = x || '|'; put x= y= z=; run;
