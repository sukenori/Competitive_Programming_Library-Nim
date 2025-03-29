proc dfs(p,i:int)=
  #行き preorder
  for j in g[i]:
    if j.t!=p:
      dfs(i,j.t)
  #帰り postorder
dfs(-1,0)