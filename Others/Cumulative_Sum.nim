cumsummed

for i in 1..<N: A[i]+=A[i-1]

#2D
import atcoder/extra/dp/cumulative_sum_2d
let s=A.initCumulativeSum2D
s[si..ei,sj..ej]
#s[si..si,sj..sj]=A[si][sj] 

for i in 0..<H:
  for j in 0..<W:
    if i>0: A[i][j]+=A[i-1][j]-(if j>0: A[i-1][j-1] else: 0)
    if j>0: A[i][j]+=A[i][j-1]