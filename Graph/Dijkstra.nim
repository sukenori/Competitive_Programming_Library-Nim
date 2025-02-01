import heapqueue
var
  q=[(w:0,i:0)].toHeapQueue
  w=newSeqWith(N,int.inf)
  d=newSeqWith(N,false)
w[0]=0
while q.len>0:
  let i=q.pop
  if not d[i.i]:
    d[i.i]=true
    for j in g[i.i]:
      let nw=i.w+j.w
      if nw<w[j.t]: w[j.t]=nw; q.push((nw,j.t))
#echo w[^1]