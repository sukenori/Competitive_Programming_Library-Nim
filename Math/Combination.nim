var t:Table[(int,int),mint]
proc c(n,r:int):mint=
  #if r<0 or r>n: return 0.mint
  if not t.hasKey((n,r)):
    t[(n,r)]=if r==0 or r==n: 1.mint else: c(n-1,r-1)+c(n-1,r)
  return t[(n,r)]

var c=newSeqWith(N+1,newSeq[mint](N+1))
for n in 0..5000:
  for r in 0..n:
    c[n][r]=if r==0 or r==n: 1.mint else: c[i-1][j]+c[i-1][j-1]

#重複組み合わせ全列挙
A.repeat(n).product