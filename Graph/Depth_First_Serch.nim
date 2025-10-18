var d=false.repeat(N)
proc dfs(i:int)=
  d[i]=true
  #行き preorder
  for j in g[i]:
    if not d[j.t]:
      dfs(j.t)
  #帰り postorder
  #d[i]=false #単純パス／この探索で初めて
dfs(0)

import deques
var
  q=[0].toDeque
  d=false.repeat(N)
d[0]=true
while q.len>0:
  let i=q.popLast
  for j in g[i]:
    if not d[j.t]:
      d[j.t]=true
      q.addLast(j.t)