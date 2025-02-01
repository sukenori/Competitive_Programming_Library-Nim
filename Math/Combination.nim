var t:Table[(int,int),mint]
proc c(i,j:int):mint=
  if not t.hasKey((i,j)):
    t[(i,j)]=(if i==j: 1.mint else: c(i,j+1)/(i-j)*(j+1))
  t[(i,j)]