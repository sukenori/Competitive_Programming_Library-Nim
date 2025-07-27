var a:seq[int]
while n>0: a.insert(n mod b,0); n=n div b

import deques
var a:Deques[int]
while n>0: a.addFirst(n mod b); n=n div b
a.toSeq

var a=0.repeat(d)
var i=0; while n>0: a[i]=n mod b; n=n div b; i+=1

for n in (0..<N).toSeq.repeat(K).product.mapIt(it.join).sorted:
  echo n

var
  n=0.repeat(K)
  i=1
while i<=K:
  if i==1: echo n
  n[^i]+=1
  if n[^i]==N:
    n[^i]=0; i+=1 
  else: i=1