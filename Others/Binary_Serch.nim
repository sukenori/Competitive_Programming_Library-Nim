import atcoder/extra/other/binary_search
minLeft((x:int)=>f(x):bool,a..b)
minLeft((x:int)=>f(x):bool,a..b)

var
  l=min
  r=max
while r-l>1:
  let m=(l+r) div 2
  if m<k: l=m
  else: r=m
echo r
#絶対満たすのはl側かr側か（最大値を求めるのか、最小値を求めるのか）
#範囲を決める際には満たす側がとりうる範囲をベースに、満たさない側が1外に出るようにする
#中央が満たすときには満たす側を動かし、最終的な答えも満たす側になる