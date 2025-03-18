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