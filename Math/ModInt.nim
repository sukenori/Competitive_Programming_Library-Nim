when not declared(Library_ModInt):
  const Library_ModInt = true
  # modint.nim
  import macros, strutils, sequtils, math

  # ==========================================================
  #   Core Implementation
  # ==========================================================

  # --- Static ModInt (コンパイル時定数Mod) ---
  type
    ModInt*[M: static[int]] = object
      v*: int

  # --- Dynamic ModInt (実行時変数Mod) ---
  type
    DynamicModInt* = object
      v*: int
      m*: int # Modをメンバとして持つ

  # ==========================================================
  #   Constructors
  # ==========================================================

  # Static
  proc initModInt*(a: int, M: static[int]): ModInt[M] {.inline.} =
    var x = a
    if x < 0: x = x mod M + M
    elif x >= M: x = x mod M
    result.v = x

  template toMint*[M](a: int): ModInt[M] = initModInt(a, M)

  # Dynamic
  proc initModInt*(a: int, m: int): DynamicModInt {.inline.} =
    var x = a
    if x < 0: x = x mod m + m
    elif x >= m: x = x mod m
    result.v = x
    result.m = m

  # ==========================================================
  #   Basic Arithmetic (Static)
  # ==========================================================
  proc `+`*[M](a, b: ModInt[M]): ModInt[M] {.inline.} =
    var res = a.v + b.v
    if res >= M: res -= M
    result.v = res

  proc `-`*[M](a, b: ModInt[M]): ModInt[M] {.inline.} =
    var res = a.v - b.v
    if res < 0: res += M
    result.v = res

  proc `*`*[M](a, b: ModInt[M]): ModInt[M] {.inline.} =
    result.v = (a.v * b.v) mod M

  proc inv*[M](a: ModInt[M]): ModInt[M] {.inline.} =
    # Static版はFermiの小定理 (素数前提)
    var
      base = a
      exp = M - 2
    result = initModInt(1, M)
    while exp > 0:
      if (exp and 1) != 0: result = result * base
      base = base * base
      exp = exp shr 1

  proc `/`*[M](a, b: ModInt[M]): ModInt[M] {.inline.} = a * b.inv()

  # ==========================================================
  #   Basic Arithmetic (Dynamic)
  # ==========================================================
  # Modが一致しているかどうかのチェックは省略（競プロ的な割り切り）
  proc `+`*(a, b: DynamicModInt): DynamicModInt {.inline.} =
    var res = a.v + b.v
    if res >= a.m: res -= a.m
    result.v = res
    result.m = a.m

  proc `-`*(a, b: DynamicModInt): DynamicModInt {.inline.} =
    var res = a.v - b.v
    if res < 0: res += a.m
    result.v = res
    result.m = a.m

  proc `*`*(a, b: DynamicModInt): DynamicModInt {.inline.} =
    result.v = (a.v * b.v) mod a.m
    result.m = a.m

  proc pow*(a: DynamicModInt, p: int): DynamicModInt {.inline.} =
    var
      base = a
      exp = p
    result = initModInt(1, a.m)
    while exp > 0:
      if (exp and 1) != 0: result = result * base
      base = base * base
      exp = exp shr 1

  proc inv*(a: DynamicModInt): DynamicModInt {.inline.} =
    # Dynamic版も素数Mod前提ならFermiでOK。
    # 汎用なら拡張ユークリッド互除法が必要だが、ここでは簡単のためpowで実装
    a.pow(a.m - 2)

  proc `/`*(a, b: DynamicModInt): DynamicModInt {.inline.} = a * b.inv()

  # ==========================================================
  #   Utilities (Input, Output, Mix-ops)
  # ==========================================================
  proc `$`*[M](a: ModInt[M]): string = $a.v
  proc `$`*(a: DynamicModInt): string = $a.v

  # Intとの演算 (Static)
  proc `+`*[M](a: ModInt[M], b: int): ModInt[M] {.inline.} = a + initModInt(b, M)
  proc `-`*[M](a: ModInt[M], b: int): ModInt[M] {.inline.} = a - initModInt(b, M)
  proc `*`*[M](a: ModInt[M], b: int): ModInt[M] {.inline.} = a * initModInt(b, M)
  proc `/`*[M](a: ModInt[M], b: int): ModInt[M] {.inline.} = a / initModInt(b, M)

  # Intとの演算 (Dynamic)
  proc `+`*(a: DynamicModInt, b: int): DynamicModInt {.inline.} = a + initModInt(b, a.m)
  proc `-`*(a: DynamicModInt, b: int): DynamicModInt {.inline.} = a - initModInt(b, a.m)
  proc `*`*(a: DynamicModInt, b: int): DynamicModInt {.inline.} = a * initModInt(b, a.m)
  proc `/`*(a: DynamicModInt, b: int): DynamicModInt {.inline.} = a / initModInt(b, a.m)

  # 複合演算子
  template defineCompAssign(op, op_eq) =
    proc op_eq*[T](a: var T, b: T) {.inline.} = a = op(a, b)
    proc op_eq*[T](a: var T, b: int) {.inline.} = a = op(a, b)

  defineCompAssign(`+`, `+=`)
  defineCompAssign(`-`, `-=`)
  defineCompAssign(`*`, `*=`)
  defineCompAssign(`/`, `/=`)

  # ==========================================================
  #   Combination (Static / Dynamic)
  # ==========================================================

  # --- Static Table Management ---
  # グローバルキャッシュ（Modごとに別名で定義するのは煩雑なのでテーブルポインタ配列で管理も可だが、
  # 実戦的にはよく使うModに特化するのが最速）
  var fact_cache_998 {.threadvar.}: seq[ModInt[998244353]]
  var invFact_cache_998 {.threadvar.}: seq[ModInt[998244353]]

  # Static版 C関数
  proc C*(t: typedesc[ModInt[998244353]], n, k: int): ModInt[998244353] =
    if k < 0 or k > n: return initModInt(0, 998244353)
    
    if fact_cache_998.len <= n:
      let oldLen = fact_cache_998.len
      if oldLen == 0:
        fact_cache_998 = @[initModInt(1, 998244353), initModInt(1, 998244353)]
        invFact_cache_998 = @[initModInt(1, 998244353), initModInt(1, 998244353)]
      
      fact_cache_998.setLen(n + 1)
      invFact_cache_998.setLen(n + 1)
      
      for i in max(2, oldLen)..n:
        fact_cache_998[i] = fact_cache_998[i-1] * i
      
      invFact_cache_998[n] = fact_cache_998[n].inv()
      for i in countdown(n-1, max(2, oldLen)):
        invFact_cache_998[i] = invFact_cache_998[i+1] * (i + 1)
        
    return fact_cache_998[n] * invFact_cache_998[k] * invFact_cache_998[n-k]


  # --- Dynamic Table Management ---
  # DynamicModInt用のシングルトン管理（最後に使ったModをキャッシュ）
  var dyn_fact_cache {.threadvar.}: seq[int]
  var dyn_invFact_cache {.threadvar.}: seq[int]
  var dyn_last_mod {.threadvar.}: int

  proc C*(dummy: DynamicModInt, n, k: int): DynamicModInt =
    # dummyはMod情報を受け渡すためのインスタンス
    let m = dummy.m
    if k < 0 or k > n: return initModInt(0, m)

    # Modが変わったらテーブルリセット
    if dyn_last_mod != m:
      dyn_fact_cache = @[1, 1]
      dyn_invFact_cache = @[1, 1] # ダミー
      dyn_last_mod = m
    
    # テーブル拡張
    if dyn_fact_cache.len <= n:
      let oldLen = dyn_fact_cache.len
      dyn_fact_cache.setLen(n + 1)
      dyn_invFact_cache.setLen(n + 1)
      
      for i in oldLen..n:
        dyn_fact_cache[i] = (dyn_fact_cache[i-1] * i) mod m
      
      # 逆元計算
      let factN = initModInt(dyn_fact_cache[n], m)
      dyn_invFact_cache[n] = factN.inv().v
      
      for i in countdown(n-1, oldLen):
        dyn_invFact_cache[i] = (dyn_invFact_cache[i+1] * (i + 1)) mod m

    let val = (dyn_fact_cache[n] * dyn_invFact_cache[k]) mod m
    let res = (val * dyn_invFact_cache[n-k]) mod m
    return initModInt(res, m)