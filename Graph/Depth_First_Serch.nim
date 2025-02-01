var d=newSeqWith(N,false)
proc dfs(i:int)=
  d[i]=true
  #行き preorder
  for j in g[i]:
    if not d[j.t]:
      dfs(j.t)
  #帰り postorder
  #d[i]=false #これまでの探索で初めて／この探索で初めて