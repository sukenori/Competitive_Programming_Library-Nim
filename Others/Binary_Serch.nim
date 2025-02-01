import atcoder/extra/other/binary_search
minLeft((x:int)=>f(x):bool,a..b)
minLeft((x:int)=>f(x):bool,a..b)

var
  l=min
  r=max
while r-l>1:
  let m=(l+r) div 2
  if m<k: l=m
  else: r=m
echo r