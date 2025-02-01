import atcoder/dsu
var d=initDSU(N)
for _ in 1..M:
  let u,v=nextInt()-1
  d.merge(u,v)

if d.same(u,v):

d.leader(i)

d.size(i)

d.groups

var p=(0..<N).toSeq
var r=newSeqWith(N,0)
proc root(i:int):int=
  if p[i]==i: return i
  else: p[i]=root(p[i]); return p[i]
proc union(u,v:int)=
  let
    ru=root(u)
    rv=root(v)
  if ru==rv: return
  else:
    if r[ru]<r[rv]: p[ru]=rv
    else:
      p[rv]=ru
      if r[ru]==r[rv]: r[ru]+=1
for _ in 1..M:
  let u,v=nextInt()-1
  union(u,v)

include atcoder/extra/structure/weighted_union_find
var s=initWeightedUnionFind[int](N)

s.root(u)

s.unionSet(u,v,w)

s.diff(u,v)