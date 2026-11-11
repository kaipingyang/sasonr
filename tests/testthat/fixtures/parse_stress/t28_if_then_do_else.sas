data _null_; do x = 1 to 3;
  if x = 1 then do; y = 'one'; z = 1; end;
  else if x = 2 then do; y = 'two'; z = 2; end;
  else do; y = 'oth'; z = 3; end;
  put x= y= z=;
end; run;
