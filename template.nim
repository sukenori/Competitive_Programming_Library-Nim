when not declared(Library_Template):
  const Library_Template = true
  {.warning[UnusedImport]: off.}
  import math, lenientops, strutils, re, strformat, parseutils, sequtils, algorithm, sets, tables, deques, heapqueue, options, macros, bitops, rationals, random, sugar

  randomize()

  template inf(T: typedesc[int]): int = 10**18
  template inf(T: typedesc[float]): float = 1e18
  template inf(T: typedesc[Rational[int]]): Rational[int] = int.inf.toRational
  template inf(T: typedesc[Rational[float]]): Rational[float] = float.inf.toRational

  template `//`(x, y: int): int = x div y
  template `%`(x, y: int): int = x mod y
  template `**`(x, y: int): int = x ^ y
  template `>>`(x, y: int): int = x shr y
  template `<<`(x, y: int): int = x shl y
  template `&`(x, y: int): int = x and y
  template `|`(x, y: int): int = x or y
  template `~`(x: int): int = not x
  template `//=`(x: var int, y: int): void = x = x div y
  template `%=`(x: var int, y: int): void = x = x mod y
  template `**=`(x: var int, y: int): void = x = x ^ y
  template `>>=`(x: var int, y: int): void = x = x shr y
  template `<<=`(x: var int, y: int): void = x = x shl y
  template `&=`(x: var int, y: int): void = x = x and y
  template `|=`(x: var int, y: int): void = x = x or y
  template `~=`(x: var int): void = x = not x

  func `///`[T](n, d: T): Rational[T] = initRational(n, d)

  proc `[]`(x, i: int): int = x shr i and 1
  proc `[]=`(x: var int, i, y: int): void =
    if y == 1: x = x or (1 shl i)
    elif y == 0: x = x and not (1 shl i)
  template count(x: int): int = x.countSetBits

  converter intToBool(x: int) : bool = x != 0

  proc `@`(x: char, a = 'a'): int = x.ord - a.ord
  proc parseInt(x: char): int = ($x).parseInt

  template ceilDiv[T: SomeSignedInt](a, b: T): int = -floorDiv(-a, b)

  proc isqrt(n: int): int {.inline.} =
    var
      x = n
      nx = (x + 1) div 2
    while x > nx:
      x = nx
      nx = (x + n div x) div 2
    return x

  proc chMax[T](a: var T, b: T): bool {.discardable, inline.} =
    if a < b:
      a = b
      return true
    else:
      return false
  proc chMin[T](a: var T, b: T): bool {.discardable, inline.} =
    if a > b:
      a = b
      return true
    else:
      return false

  var currentMod {.threadvar.}: int
  type Mint = object
    v: int
  template getMod(): int =
    if currentMod == 0: 998244353
    else: currentMod
  proc initMint(M: int) =
    currentMod = M
  template mint(a: int): Mint =
    let m = getMod()
    var val = a
    if val < 0: val = val mod m + m
    elif val >= m: val = val mod m
    Mint(v: val)
  proc `+`(a, b: Mint): Mint {.inline.} =
    let m = getMod()
    var res = a.v + b.v
    if res >= m: res -= m
    result.v = res
  proc `-`(a, b: Mint): Mint {.inline.} =
    let m = getMod()
    var res = a.v - b.v
    if res < 0: res += m
    result.v = res
  proc `*`(a, b: Mint): Mint {.inline.} =
    result.v = (a.v * b.v) mod getMod()
  proc pow(a: Mint, p: int): Mint {.inline.} =
    var
      base = a
      exp = p
    result = 1.mint
    while exp > 0:
      if (exp and 1) != 0: result = result * base
      base = base * base
      exp = exp shr 1
  proc inv(a: Mint): Mint {.inline.} =
    a.pow(getMod() - 2)
  proc `/`(a, b: Mint): Mint {.inline.} = a * b.inv()
  proc `+`(a: Mint, b: int): Mint {.inline.} = a + b.mint
  proc `-`(a: Mint, b: int): Mint {.inline.} = a - b.mint
  proc `*`(a: Mint, b: int): Mint {.inline.} = a * b.mint
  proc `/`(a: Mint, b: int): Mint {.inline.} = a / b.mint
  proc `+`(a: int, b: Mint): Mint {.inline.} = a.mint + b
  proc `-`(a: int, b: Mint): Mint {.inline.} = a.mint - b
  proc `*`(a: int, b: Mint): Mint {.inline.} = a.mint * b
  proc `/`(a: int, b: Mint): Mint {.inline.} = a.mint / b
  template defOp(op, op_eq) =
    proc op_eq(a: var Mint, b: Mint) {.inline.} = a = op(a, b)
    proc op_eq(a: var Mint, b: int) {.inline.} = a = op(a, b)
  defOp(`+`, `+=`)
  defOp(`-`, `-=`)
  defOp(`*`, `*=`)
  defOp(`/`, `/=`)
  proc `$`(a: Mint): string = $a.v
  var fact {.threadvar.}: seq[Mint]
  var invFact {.threadvar.}: seq[Mint]
  var fact_mod_version {.threadvar.}: int 
  proc C(n, k: int): Mint =
    let m = getMod()
    if k < 0 or k > n: return 0.mint
    if fact_mod_version != m:
      fact = @[]
      invFact = @[]
      fact_mod_version = m
    if fact.len <= n:
      let oldLen = fact.len
      if oldLen == 0:
        fact = @[1.mint, 1.mint]
        invFact = @[1.mint, 1.mint]
      let start = max(2, oldLen)
      fact.setLen(n + 1)
      invFact.setLen(n + 1)
      for i in start..n:
        fact[i] = fact[i-1] * i
      invFact[n] = fact[n].inv()
      for i in countdown(n-1, start):
        invFact[i] = invFact[i+1] * (i + 1)
    return fact[n] * invFact[k] * invFact[n-k]
  proc P(n, k: int): Mint =
    if k < 0 or k > n: return 0.mint
    discard C(n, k)
    return fact[n] * invFact[n-k]

  type InitSeq = object
  const Seq = InitSeq()
  template makeSeq[T](len: int, init: T): auto = newSeqWith(len, init)
  template makeSeq(len: int, init: typedesc): auto = newSeq[init](len)
  macro `[]`(s: InitSeq, args: varargs[untyped]): untyped =
    if args.len == 1 and args[0].kind != nnkExprColonExpr:
      return newCall(newTree(nnkBracketExpr, ident("newSeq"), args[0]))
    result = newCall(ident("makeSeq"), args[^1][0], args[^1][1])
    for i in countdown(args.len - 2, 0):
      result = newCall(ident("makeSeq"), args[i], result)

  proc `<`[T](a, b: openArray[T]): bool =
    for i in 0 ..< min(a.len, b.len):
      if a[i] < b[i]: return true
      if a[i] > b[i]: return false
    return a.len < b.len
  proc `<=`[T](a, b: openArray[T]): bool =
    return not (b < a)

  type View1D[T] = object
    data: ptr UncheckedArray[T]
    len: int
    offset, step: int
  func view[T](a: var seq[T]): View1D[T] =
    View1D[T](data: cast[ptr UncheckedArray[T]](addr a[0]), len: a.len, offset: 0, step: 1)
  func `[]`[T](v: View1D[T], i: int): var T {.inline.} =
    v.data[v.offset + i * v.step]
  func `[]=`[T](v: View1D[T], i: int, val: T) {.inline.} =
    v.data[v.offset + i * v.step] = val
  func shift[T](v: View1D[T], k: int = 1): View1D[T] =
    result = v
    result.offset -= k * v.step
  func start[T](v: View1D[T], s: int): View1D[T] =
    assert s >= 0 and s <= v.len
    result = v
    result.offset += s * v.step
    result.len -= s
  func size[T](v: View1D[T], l: int): View1D[T] =
    assert l >= 0 and l <= v.len
    result = v
    result.len = l
  func reverse[T](v: View1D[T]): View1D[T] =
    result = v
    result.offset += (v.len - 1) * v.step
    result.step = -v.step
  func shift[T](a: var seq[T], k: int = 1): View1D[T] {.inline.} = a.view.shift(k)
  func start[T](a: var seq[T], s: int): View1D[T] {.inline.} = a.view.start(s)
  func size[T](a: var seq[T], l: int): View1D[T] {.inline.} = a.view.size(l)
  func reverse[T](a: var seq[T]): View1D[T] {.inline.} = a.view.reverse
  type View2D[T] = object
    data: ptr seq[seq[T]]
    h, w: int
    r0, c0: int
    ry, rx, cy, cx: int
  func view[T](grid: var seq[seq[T]]): View2D[T] =
    if grid.len == 0: return
    View2D[T](
      data: addr grid, h: grid.len, w: grid[0].len,
      r0: 0, c0: 0,
      ry: 1, rx: 0,
      cy: 0, cx: 1
    )
  func `[]`*[T](v: View2D[T], y, x: int): var T {.inline.} =
    let r = v.r0 + y * v.ry + x * v.rx
    let c = v.c0 + y * v.cy + x * v.cx
    v.data[][r][c]
  func `[]=`[T](v: View2D[T], y, x: int, val: T) {.inline.} =
    v[y, x] = val
  func shift[T](v: View2D[T], dy: int = 1, dx: int = 1): View2D[T] =
    result = v
    result.r0 -= dy * v.ry + dx * v.rx
    result.c0 -= dy * v.cy + dx * v.cx
  func start[T](v: View2D[T], y: int, x: int): View2D[T] =
    assert y >= 0 and x >= 0
    result = v
    result.r0 += y * v.ry + x * v.rx
    result.c0 += y * v.cy + x * v.cx
    result.h -= y
    result.w -= x
  func size[T](v: View2D[T], h, w: int): View2D[T] =
    assert h <= v.h and w <= v.w
    result = v
    result.h = h
    result.w = w
  func rotate90[T](v: View2D[T]): View2D[T] =
    result = v
    swap(result.h, result.w)
    let new_ry = -v.rx
    let new_rx = v.ry
    let new_cy = -v.cx
    let new_cx = v.cy
    let new_r0 = v.r0 + (v.h - 1) * v.ry
    let new_c0 = v.c0 + (v.h - 1) * v.cy
    result.ry = new_ry; result.rx = new_rx
    result.cy = new_cy; result.cx = new_cx
    result.r0 = new_r0; result.c0 = new_c0
  func swapXY[T](v: View2D[T]): View2D[T] =
    result = v
    swap(result.h, result.w)
    swap(result.ry, result.rx)
    swap(result.cy, result.cx)
  func flipUD[T](v: View2D[T]): View2D[T] =
    result = v
    result.r0 += (v.h - 1) * v.ry
    result.c0 += (v.h - 1) * v.cy
    result.ry = -v.ry
    result.cy = -v.cy
  func flipLR[T](v: View2D[T]): View2D[T] =
    result = v
    result.r0 += (v.w - 1) * v.rx
    result.c0 += (v.w - 1) * v.cx
    result.rx = -v.rx
    result.cx = -v.cx
  func shift[T](g: var seq[seq[T]], dy: int = 1, dx: int = 1): View2D[T] {.inline.} = g.view.shift(dy, dx)
  func start[T](g: var seq[seq[T]], y, x: int): View2D[T] {.inline.} = g.view.start(y, x)
  func size[T](g: var seq[seq[T]], h, w: int): View2D[T] {.inline.} = g.view.size(h, w)
  func rotate90[T](g: var seq[seq[T]]): View2D[T] {.inline.} = g.view.rotate90
  func swapXY[T](g: var seq[seq[T]]): View2D[T] {.inline.} = g.view.swapXY
  func flipUD[T](g: var seq[seq[T]]): View2D[T] {.inline.} = g.view.flipUD
  func flipLR[T](g: var seq[seq[T]]): View2D[T] {.inline.} = g.view.flipLR

  iterator items(t: (HSlice[int,int], int)): int =
    let (slice, step) = t
    if slice.a <= slice.b:
      for i in countup(slice.a, slice.b, step):
        yield i
    else:
      for i in countdown(slice.a, slice.b, step):
        yield i

  type BisectRes = object
    first, last, count: int
  func bisectRes(l, r: int): BisectRes =
    result.count = max(0, r - l)
    if result.count > 0:
      result.first = l
      result.last = r - 1
    else:
      result.first = -1
      result.last = -1
  func less[T](a: openArray[T], x: T): BisectRes =
    bisectRes(0, a.lowerBound(x))
  func lessEqual[T](a: openArray[T], x: T): BisectRes =
    bisectRes(0, a.upperBound(x))
  func greaterEqual[T](a: openArray[T], x: T): BisectRes =
    bisectRes(a.lowerBound(x), a.len)
  func greater[T](a: openArray[T], x: T): BisectRes =
    bisectRes(a.upperBound(x), a.len)
  func first(res: BisectRes): int {.inline.} = res.first
  func last(res: BisectRes): int {.inline.} = res.last
  func `[]`[T](a: openArray[T], res: BisectRes): seq[T] =
    if res.count == 0: return @[]
    return a[res.startIdx .. res.endIdx]

  template loop(n: int, body: untyped) =
    for _ in 1 .. n:
      body

  template forIt(s: untyped; op: untyped): untyped =
    collect(newSeq):
      for it {.inject.} in s:
        op
  template forAllIt(s: untyped; pred: untyped): bool =
    block:
      var f = true
      for it {.inject.} in s:
        if not (pred):
          f = false
          break
      f
  template forAnyIt(s: untyped; pred: untyped): bool =
    block:
      var f = false
      for it {.inject.} in s:
        if not (pred):
          f = true
          break
      f
  template forCountIt(s: untyped; pred: untyped): int =
    block:
      var c = 0
      for it {.inject.} in s:
        if (pred):
          inc c
      c

  macro sumOf(i, s, body: untyped): untyped =
    result = quote do:
      block:
        type T = typeof((
          block:
            var `i`: typeof(`s`.a)
            `body`
        ))
        var acc: T
        for `i` in `s`:
          acc += `body`
        acc
  macro minOf(i, s, body: untyped): untyped =
    result = quote do:
      block:
        type T = typeof((
          block:
            var `i`: typeof(`s`.a)
            `body`
        ))
        var acc: T
        acc = T.inf
        var isFirst = true
        for `i` in `s`:
          let v = `body`
          if isFirst:
            acc = v
            isFirst = false
          else:
            if v < acc: acc = v
        acc
  macro maxOf(i, s, body: untyped): untyped =
    result = quote do:
      block:
        type T = typeof((
          block:
            var `i`: typeof(`s`.a)
            `body`
        ))
        var acc: T
        acc = -T.inf
        var isFirst = true
        for `i` in `s`:
          let v = `body`
          if isFirst:
            acc = v
            isFirst = false
          else:
            if v > acc: acc = v
        acc
  proc replaceBreak(node: NimNode, labelSym: NimNode): NimNode =
    if node.kind == nnkBreakStmt and node[0].kind == nnkEmpty:
      return newTree(nnkBreakStmt, labelSym)
    if node.kind in {nnkForStmt, nnkWhileStmt, nnkBlockStmt}:
      return node
    result = copyNimNode(node)
    for child in node:
      result.add replaceBreak(child, labelSym)
  macro forElse(i, s, body: untyped): untyped =
    var loopBody = newStmtList()
    var elseBody = newStmtList()
    var foundElse = false
    let stmtList = if body.kind == nnkStmtList: body else: newStmtList(body)
    for node in stmtList:
      if not foundElse and node.kind == nnkCall and node[0].kind == nnkIdent and node[0].strVal == "Else":
        foundElse = true
        if node.len > 1 and node[1].kind == nnkStmtList:
          for child in node[1]: elseBody.add(child)
      elif foundElse:
        elseBody.add(node)
      else:
        loopBody.add(node)
    let successLabel = genSym(nskLabel, "successLabel")
    let modifiedBody = replaceBreak(loopBody, successLabel)
    result = quote do:
      block `successLabel`:
        for `i` in `s`:
          `modifiedBody`
        `elseBody`

  template mutable[T](x: T): var T =
    cast[ptr T](x.unsafeAddr)[]

  proc getcharUnlocked(): cint {.header: "<stdio.h>", importc: "getchar_unlocked".}
  proc validChar(): cint {.inline.} =
    while true:
      result = getcharUnlocked()
      if result notin {8 .. 13, 32}: break
  proc input(x: var int) {.inline.} =
    var
      ch = validChar()
      sgn = 1
    if ch == 45:
      sgn = -1
      ch = getcharUnlocked()
    x = 0
    while ch in 48 .. 57:
      x = x * 10 + (ch - 48)
      ch = getcharUnlocked()
    x *= sgn
  proc input(T: typedesc[int]): int =
    result.input
  proc input(x: var string) {.inline.} =
    var ch = validChar()
    x = ""
    while ch > 32:
      x.add(ch.char)
      ch = getcharUnlocked()
  proc input(T: typedesc[string]): string =
    result.input
  proc input(x: var float) =
    x = string.input.parseFloat
  proc input(T: typedesc[float]): float =
    result.input
  proc input[T](s: var seq[T]) =
    for i in 0 ..< s.len:
      s[i].input
  proc input[T](s: var seq[seq[T]]) =
    for i in 0 ..< s.len:
      s[i].input
  macro input(t: tuple, diff: static[int] = 0): untyped =
    let
      len = newDotExpr(t[0], ident("len")) 
      i = ident("i")
    var body = newStmtList()
    for s in t:
      let term = newTree(nnkBracketExpr, s, i)
      body.add newCall("input", term)
      if diff != 0:
        body.add newCall("inc", term, newLit(diff))
    result = quote do:
      for `i` in 0 ..< `len`: `body`

  template echoFloat(v: float) =
    echo v.formatFloat(ffDecimal, 20)
