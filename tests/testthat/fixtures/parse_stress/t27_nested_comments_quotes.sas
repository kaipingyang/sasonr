%* macro comment with 'quote ;
data _null_; s = "/* not a comment */"; t = 'x'; /* real comment with ' quote */ put s= t=; run;
