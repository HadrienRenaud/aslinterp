comparevalue = X[s];
newvalue = X[t];
address = X[n];
oldvalue = Mem[address];
if comparevalue == oldvalue then
Mem[address] = newvalue
else pass end;
X[s] = oldvalue