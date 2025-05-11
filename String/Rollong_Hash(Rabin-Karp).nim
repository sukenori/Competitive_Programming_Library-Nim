import random
randomize()
let
  m=2^61-1
  b=rand(m-1)
proc H(s:string):int=
  for i in 0..<s.len:
    result=(result*b+s[i].ord) mod m