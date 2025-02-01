import atcoder/extra/other/direction
for (nh,nw) in (h,w).neighbor(dir4,(0..<H,0..<W)): #dir8

for (dh,dw) in [(-1,0),(0,1),(1,0),(0,-1)]:
  let (nh,nw)=(h+dh,w+dw)
  if nh in 0..<H and nw in 0..<W and 

#Sentinel
var m=newSeqWith(H,"#"&nextString()&"#")
for i in [0,H+1]: m.insert("#".repeat(W+2),i)