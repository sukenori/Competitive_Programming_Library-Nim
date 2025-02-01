let s=(@[0]&A).cumsummed
var
  r=1
  a=0
for l in 0..<N:
  #総和がx以下となる区間の最大長
  while r<N and s[r+1]-s[l]<=x: r+=1
  a.max=r-l
echo a