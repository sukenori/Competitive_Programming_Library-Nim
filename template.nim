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

  iterator items(t: (HSlice[int,int], int)): int =
    let (slice, step) = t
    if slice.a <= slice.b:
      for i in countup(slice.a, slice.b, step):
        yield i
    else:
      for i in countdown(slice.a, slice.b, step):
        yield i

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
  template forCountIt(s: untyped; pred: untyped): int =
    block:
      var c = 0
      for it {.inject.} in s:
        if (pred):
          inc c
      c

  macro sumOf(i, r, body: untyped): untyped =
    result = quote do:
      block:
        type T = typeof((
          block:
            var `i`: typeof(`r`.a)
            `body`
        ))
        var acc: T
        for `i` in `r`:
          acc += `body`
        acc
  macro minOf(i, r, body: untyped): untyped =
    result = quote do:
      block:
        type T = typeof((
          block:
            var `i`: typeof(`r`.a)
            `body`
        ))
        var acc: T
        acc = T.inf
        var isFirst = true
        for `i` in `r`:
          let v = `body`
          if isFirst:
            acc = v
            isFirst = false
          else:
            if v < acc: acc = v
        acc
  macro maxOf(i, r, body: untyped): untyped =
    result = quote do:
      block:
        type T = typeof((
          block:
            var `i`: typeof(`r`.a)
            `body`
        ))
        var acc: T
        acc = -T.inf
        var isFirst = true
        for `i` in `r`:
          let v = `body`
          if isFirst:
            acc = v
            isFirst = false
          else:
            if v > acc: acc = v
        acc

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

  macro input(t: tuple): untyped=
    let
      len = newDotExpr(t[0], ident("len")) 
      i = ident("i")
    var body = newStmtList()
    for s in t:
      body.add newCall("input", newTree(nnkBracketExpr, s, i))
    result = quote do:
      for `i` in 0 ..< `len`: `body`

  template echo(v: float) =
    echo v.formatFloat(ffDecimal, 20)
