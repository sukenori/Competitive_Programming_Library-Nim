var t:Table[(int,int),mint]
proc c(n,r:int):mint=
  #if r<0 or r>n: return 0.mint
  if not t.hasKey((n,r)):
    t[(n,r)]=if r==0 or r==n: 1.mint else: c(n-1,r-1)+c(n-1,r)
  return t[(n,r)]