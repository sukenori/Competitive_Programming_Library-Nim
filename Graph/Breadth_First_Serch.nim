import deques
var
  q=[0].toDeque
  d=false.repeat(N)
d[0]=true
while q.len>0:
  let i=q.popFirst
  for j in g[i]:
    if not d[j.t]:
      d[j.t]=true
      q.addLast(j.t)