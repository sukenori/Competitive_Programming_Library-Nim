import deques
var
  q=[0].toDeque
  d=false.repeat(N)
d[0]=true
while q.len>0:
  let u=q.popFirst
  for v in g[u]:
    if not d[v.t]:
      d[v.t]=true
      q.addLast(v.t)