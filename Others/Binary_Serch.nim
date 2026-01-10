# 汎用二分探索（めぐる式）
# 常に abs(ok - ng) > 1 の間ループし、最終的に ok を返す
# テンプレートにすることで副作用エラーを回避し、インライン化による高速化も期待できる
template meguruSearch(okVal, ngVal: int, check: untyped): int =
  var ok = okVal
  var ng = ngVal
  while abs(ok - ng) > 1:
    let mid = ok + (ng - ok) div 2
    if check(mid):
      ok = mid
    else:
      ng = mid
  ok

# Type A: 条件 check(x) を満たす「最小」の整数を返す
# 範囲 [L, R] で単調性 (False...False -> True...True) を仮定
template minLeft(L, R: int, checkPred: untyped): int =
  meguruSearch(R, L - 1, proc(i: int): bool = checkPred(i))

# Type B: 条件 check(x) を満たす「最大」の整数を返す
# 範囲 [L, R] で単調性 (True...True -> False...False) を仮定
template maxRight(L, R: int, checkPred: untyped): int =
  meguruSearch(L, R + 1, proc(i: int): bool = checkPred(i))


import atcoder/extra/other/binary_search
minLeft((x:int)=>f(x):bool,a..b)
minLeft((x:int)=>f(x):bool,a..b)

var
  l=min
  r=max+1
while r-l>1:
  let m=(l+r) div 2
  if m<=k: l=m
  else: r=m
echo l
#絶対満たすのはl側かr側か
#範囲を決める際には満たす側がとりうる範囲をベースに、満たさない側が1外に出るようにする
#中央が満たすときには満たす側を動かし、最終的な答えも満たす側になる