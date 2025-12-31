when not declared(CUMULATIVE_SUM):
  const CUMULATIVE_SUM = true
import sequtils

# --- 1次元累積和 (CumSum) ---

type
  CumSum*[T] = object
    data: seq[T]

# ヘルパー: インデックス計算
template calcIdx(len: int, i: int): int = i
template calcIdx(len: int, i: BackwardsIndex): int = len - int(i)

# 実体: T(出力型), U(入力型) を両方取る
proc initCumSumImpl*[T, U](arr: seq[U]): CumSum[T] =
  var data = newSeq[T](arr.len + 1)
  data[0] = default(T)
  for i in 0 ..< arr.len:
    when compiles(T(arr[i])):
      data[i + 1] = data[i] + T(arr[i])
    else:
      {.error: "Cannot convert " & $U & " to " & $T.}
  result.data = data

# 公開API 1: 型推論 (initCumSum(intSeq) -> CumSum[int])
proc initCumSum*[T](arr: seq[T]): CumSum[T] =
  return initCumSumImpl[T, T](arr)

# 公開API 2: 型指定 (initCumSum[Mint](intSeq) -> CumSum[Mint])
# テンプレートで U を推論させて Impl に渡す
template initCumSum*[T](arr: seq): auto =
  initCumSumImpl[T, typeof(arr[0])](arr)


# 区間和取得 [l..r] (閉区間)

proc `[]`*[T](cs: CumSum[T], slice: HSlice[int, int]): T =
  let
    l = slice.a
    r = slice.b
  return cs.data[r + 1] - cs.data[l]

proc `[]`*[T](cs: CumSum[T], slice: HSlice[int, BackwardsIndex]): T =
  let len = cs.data.len - 1
  let l = slice.a
  let r = calcIdx(len, slice.b)
  return cs.data[r + 1] - cs.data[l]

proc `[]`*[T](cs: CumSum[T], slice: HSlice[BackwardsIndex, BackwardsIndex]): T =
  let len = cs.data.len - 1
  let l = calcIdx(len, slice.a)
  let r = calcIdx(len, slice.b)
  return cs.data[r + 1] - cs.data[l]


# --- 2次元累積和 (CumSum2D) ---

type
  CumSum2D*[T] = object
    data: seq[seq[T]]
    h, w: int

# 実体: T(出力型), U(入力型)
proc initCumSum2DImpl*[T, U](grid: seq[seq[U]]): CumSum2D[T] =
  let h = grid.len
  let w = if h > 0: grid[0].len else: 0
  var data = newSeqWith(h + 1, newSeq[T](w + 1))
  
  for i in 0 ..< h:
    for j in 0 ..< w:
      when compiles(T(grid[i][j])):
        let val = T(grid[i][j])
        data[i+1][j+1] = data[i][j+1] + data[i+1][j] - data[i][j] + val
      else:
        {.error: "Cannot convert " & $U & " to " & $T.}
  
  result.data = data
  result.h = h
  result.w = w

# 公開API 1: 型推論
proc initCumSum2D*[T](grid: seq[seq[T]]): CumSum2D[T] =
  return initCumSum2DImpl[T, T](grid)

# 公開API 2: 型指定
template initCumSum2D*[T](grid: seq[seq]): auto =
  initCumSum2DImpl[T, typeof(grid[0][0])](grid)


# 区間和取得

# 内部ヘルパー
proc queryRect[T](cs: CumSum2D[T], y1, y2, x1, x2: int): T =
  return cs.data[y2 + 1][x2 + 1] - cs.data[y1][x2 + 1] - cs.data[y2 + 1][x1] + cs.data[y1][x1]

# 1. cs[y1..y2, x1..x2] (カンマ区切り)
proc `[]`*[T](cs: CumSum2D[T], ySlice: HSlice, xSlice: HSlice): T =
  let y1 = calcIdx(cs.h, ySlice.a)
  let y2 = calcIdx(cs.h, ySlice.b)
  let x1 = calcIdx(cs.w, xSlice.a)
  let x2 = calcIdx(cs.w, xSlice.b)
  return queryRect(cs, y1, y2, x1, x2)

# 2. cs[y1..y2][x1..x2] (ブラケット連続) 用のプロキシ
type
  CumSum2DrowProxy*[T] = object
    cs: ptr CumSum2D[T]
    y1, y2: int

# 最初の []
proc `[]`*[T](cs: var CumSum2D[T], ySlice: HSlice): CumSum2DrowProxy[T] =
  result.cs = addr(cs)
  result.y1 = calcIdx(cs.h, ySlice.a)
  result.y2 = calcIdx(cs.h, ySlice.b)

# 2つ目の []
proc `[]`*[T](proxy: CumSum2DrowProxy[T], xSlice: HSlice): T =
  let x1 = calcIdx(proxy.cs[].w, xSlice.a)
  let x2 = calcIdx(proxy.cs[].w, xSlice.b)
  return queryRect(proxy.cs[], proxy.y1, proxy.y2, x1, x2)
