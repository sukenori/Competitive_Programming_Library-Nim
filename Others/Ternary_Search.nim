#実数
var
  l=0.0
  r=10.0^n
while r-l>pow(10,-6.0):
  let
    ml=(2*l+r)/3
    mr=(l+2*r)/3
  if f(ml)>f(mr): l=ml
  else: r=mr

#整数
while r-l>1:
  let
    ml=(l*2+r) div 3
    mr=(l+m*2) div 3
  if f(ml)>f(mr): l=ml
  else: r=mr
for i in l..r: a.min=f(i)