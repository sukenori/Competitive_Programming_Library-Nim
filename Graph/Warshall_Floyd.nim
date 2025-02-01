import atcoder/extra/graph/warshall_floyd
var g=newSeqWith(N,newSeqWith(N,int.inf))
for i in 0..<N: g[i][i]=0
for _ in 1..M:
  let u,v=nextInt()-1
  let w=nextInt()
  g[u][v]=w
let d=g.warshallFloyd
#echo d[s,t]
#echo d.path(s,t)