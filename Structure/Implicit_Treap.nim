type
  Operation[V]=proc(val_a,val_b:V):V
  IdentityOp[V]=proc():V
  Composition[L]=proc(newLazy,oldLazy:L):L
  IdentityComp[L]=proc():L
  Mapping[V,L]=proc(val:V,lazy:L):V
  CompareOp[V]=proc(val_a,val_b:V):int
  LazyState[L]=object
    lazy:L
    reverse:bool
  NoState=object
  ImplicitNode[V,S]=ref object
    val:V
    priority:int
    size:int
    accum:V
    state:S
    l,r:ImplicitNode[V,S]
proc nodeUpdate[V,S](node:ImplicitNode[V,S],op:Operation[V],idOp:IdentityOp[V])=
  if node==nil: return
  node.size=1+node.l.size+node.r.size
  let l_accum=if node.l!=nil: node.l.accum else: idOp()
  let r_accum=if node.r!=nil: node.r.accum else: idOp()
  node.accum=op(op(l_accum,node.val),r_accum)
proc propagate[V,S,L](node:ImplicitNode[V,S],map:Mapping[V,L],comp:Composition[L],idComp:IdentityComp[L])=
  when S is NoState: discard
  elif S is LazyState[L]:
    if node==nil: return
    if node.state.reverse:
      node.state.reverse=false
      swap(node.l,node.r)
      if node.l!=nil: node.l.state.reverse=not node.l.state.reverse
      if node.r!=nil: node.r.state.reverse=not node.r.state.reverse
    if node.state.lazy!=idComp():
      if node.l!=nil:
        node.l.state.lazy=comp(node.state.lazy,node.l.state.lazy)
        node.l.val=map(node.l.val,node.state.lazy)
        node.l.accum=map(node.l.accum,node.state.lazy)
      if node.r!=nil:
        node.r.state.lazy=comp(node.state.lazy,node.r.state.lazy)
        node.r.val=map(node.r.val,node.state.lazy)
        node.r.accum=map(node.r.accum,node.state.lazy)
    node.state.lazy=idComp()
proc splitByIndex[V,S,L](node:ImplicitNode[V,S],targetIndex:int,l,r:var ImplicitNode[V,S],op:Operation[V],idOp:IdentityOp[V],map:Mapping[V,L],comp:Composition[L],idComp:IdentityComp[L])=
  if node==nil:
    l=nil; r=nil
    return
  propagate(node,map,comp,idComp)
  let nodeIndex=(if node.l!=nil: node.l.size else:0)+1
  if targetIndex<nodeIndex:
    splitByIndex(node.l,targetIndex,l,node.l,op,idOp,map,comp,idComp)
    r=node
  else:
    splitByIndex(node.r,targetIndex,node.r,r,op,idOp,map,comp,idComp)
    l=node
  nodeUpdate(node,op,idOp)
proc splitByVal[V,S,L](node:ImplicitNode[V,S],targetVal:V,l,r:var ImplicitNode[V,S],op:Operation[V],idOp:IdentityOp[V],map:Mapping[V,L],comp:Composition[L],idComp:IdentityComp[L],cmp:CompareOp[V])=
  if node==nil:
    l=nil; r=nil
    return
  propagate(node,map,comp,idComp)
  let nodeIndex=(if node.l!=nil: node.l.size else:0)+1
  if cmp(node.val,targetVal)<0:
    splitByVal(node.r,targetVal,node.r,r,op,idOp,map,comp,idComp)
    l=node
  else:
    splitByVal(node.l,targetVal,l,node.l,op,idOp,map,comp,idComp)
    r=node
  nodeUpdate(node,op,idOp)
proc mergeTreap[V,S,L](node:var ImplicitNode[V,S],l,r:ImplicitNode[V,S],op:Operation[V],idOp:IdentityOp[V],map:Mapping[V,L],comp:Composition[L],idComp:IdentityComp[L])=
  if l==nil or r==nil:
    node=if l!=nil: l else: r
    return
  propagate(l,map,comp,idComp)
  propagate(r,map,comp,idComp)
  if l.priority>r.priority:
    mergeTreap(l.r,l.r,r,op,idOp,map,comp,idComp)
    node=l
  else:
    mergeTreap(r.l,l,r.l,op,idOp,map,comp,idComp)
    node=r
  nodeUpdate(node,op,idOp)
type
  ImplicitTreap[V,S,L]=object
    root:ImplicitNode[V,S]
    op:Operation[V]
    idOp:IdentityOp[V]
    comp:Composition[L]
    idComp:IdentityComp[L]
    map:Mapping[V,L]
    isSorted:bool
    cmp:CompareOp[V]
    isMulti:bool
proc initImplicitTreap[V,S,L](op:Operation[V],idOp:IdentityOp[V],comp:Composition[L],idComp:IdentityComp[L],map:Mapping[V,L],cmp:CompareOp[V]=system.cmp[V],isMulti=false):ImplicitTreap[V,S,L]=
  result.op=op
  result.idOp=idOp
  result.comp=comp
  result.idComp=idComp
  result.map=map
  result.cmp=cmp
  result.isMulti=isMulti
  result.root=nil
  randomize()
template checkImplicit[V,S,L](implicitTreap:ImplicitTreap[V,S,L])=
  if implicitTreap.isSorted:
    raise newException(ValueError,"requires implicit Treap")
template checkSorted[V,S,L](implicitTreap:ImplicitTreap[V,S,L])=
  if not implicitTreap.isSorted:
    raise newException(ValueError,"requires sorted Treap")
proc len[V,S,L](self:ImplicitTreap[V,S,L]):int=
  if self.root==nil: 0 else: self.root.size
proc lowerBound[V,S,L](self:ImplicitTreap[V,S,L],val:V):int=
  self.checkSorted()
  result=self.len 
  var idx=0 
  var node=self.root
  while node!=nil:
    propagate(node,self.map,self.comp,self.idComp)
    let left_size=if node.l!=nil: node.l.size else: 0
    if self.cmp(node.val,val)>=0:
      result=idx+left_size 
      node=node.l
    else:
      idx+=left_size+1 
      node=node.r
proc upperBound[V,S,L](self:ImplicitTreap[V,S,L],val:V):int=
  self.checkSorted()
  result=self.len
  var idx=0
  var node=self.root
  while node!=nil:
    propagate(node,self.map,self.comp,self.idComp)
    let left_size=if node.l!=nil: node.l.size else: 0
    if self.cmp(node.val,val)>0:
      result=idx+left_size
      node=node.l
    else:
      idx+=left_size+1
      node=node.r
proc count[V,S,L](self:var ImplicitTreap[V,S,L],val:V):int=
  return self.upperBound(val)-self.lowerBound(val)
proc insertByIndex[V,S,L](self:var ImplicitTreap[V,S,L],targetIndex:int,val:V)=
  self.checkImplicit()
  var l,r:ImplicitNode[V,S]
  splitByIndex(self.root,targetIndex,l,r,self.op,self.idOp,self.map,self.comp,self.idComp)
  var newNode=ImplicitNode[V,S](val:val,priority:rand(int.high),size:1,accum:val,state: when S is LazyState[L]:LazyState[L](lazy:self.idComp(),reverse:false) else: NoState(),l:nil,r:nil)
  mergeTreap(l,l,newNode,self.op,self.idOp,self.map,self.comp,self.idComp)
  mergeTreap(self.root,l,r,self.op,self.idOp,self.map,self.comp,self.idComp)
proc deleteByIndex[V,S,L](self:var ImplicitTreap[V,S,L],targetIndex:int)=
  self.checkImplicit()
  var l,r,mid:ImplicitNode[V,S]
  splitByIndex(self.root,targetIndex+1,l,r,self.op,self.idOp,self.map,self.comp,self.idComp)
  splitByIndex(l,targetIndex,l,mid,self.op,self.idOp,self.map,self.comp,self.idComp)
  mergeTreap(self.root,l,r,self.op,self.idOp,self.map,self.comp,self.idComp)
proc insertByVal[V,S,L](self:var ImplicitTreap[V,S,L],targetVal:V)=
  self.checkSorted()
  if not self.isMulti:
    if self.count(targetVal)>0: return
  self.insertByIndex(self.lowerBound(targetVal),targetVal)
proc deleteVal[V,S,L](self:var ImplicitTreap[V,S,L],targetVal:V)=
  self.deleteByIndex(self.lowerBound(targetVal))
proc queryRange[V,S,L](self:var ImplicitTreap[V,S,L],l_idx,r_idx:int):V=
  if l_idx>=r_idx: return self.idOp()
  var root_l,root_m,root_r:ImplicitNode[V,S]
  splitByIndex(self.root,r_idx,root_m,root_r,self.op,self.idOp,self.map,self.comp,self.idComp)
  splitByIndex(root_m,l_idx,root_l,root_m,self.op,self.idOp,self.map,self.comp,self.idComp)
  result = if root_m != nil: root_m.accum else: self.idOp()
  mergeTreap(root_m,root_l,root_m,self.op,self.idOp,self.map,self.comp,self.idComp)
  mergeTreap(self.root,root_m,root_r,self.op,self.idOp,self.map,self.comp,self.idComp)
proc applyMapping[V,S,L](self:var ImplicitTreap[V,S,L],l_idx,r_idx:int,lazy:L)=
  if l_idx>=r_idx: return
  var root_l,root_m,root_r:ImplicitNode[V,S]
  splitByIndex(self.root,r_idx,root_m,root_r,self.op,self.idOp,self.map,self.comp,self.idComp)
  splitByIndex(root_m,l_idx,root_l,root_m,self.op,self.idOp,self.map,self.comp,self.idComp)
  when S is LazyState[L]:
    if root_m!=nil:
      root_m.state.lazy=self.comp(lazy,root_m.state.lazy)
      root_m.val=self.map(root_m.val,lazy)
      root_m.accum=self.map(root_m.accum,lazy)
  mergeTreap(root_m,root_l,root_m,self.op,self.idOp,self.map,self.comp,self.idComp)
  mergeTreap(self.root,root_m,root_r,self.op,self.idOp,self.map,self.comp,self.idComp)
proc reverseTreap[V,S,L](self:var ImplicitTreap[V,S,L],l_idx,r_idx:int)=
  if l_idx>=r_idx: return
  var root_l,root_m,root_r:ImplicitNode[V,S]
  splitByIndex(self.root,r_idx,root_m,root_r,self.op,self.idOp,self.map,self.comp,self.idComp)
  splitByIndex(root_m,l_idx,root_l,root_m,self.op,self.idOp,self.map,self.comp,self.idComp)
  when S is LazyState[L]:
    if root_m!=nil:
      root_m.state.reverse=not root_m.state.reverse
  mergeTreap(root_m,root_l,root_m,self.op,self.idOp,self.map,self.comp,self.idComp)
  mergeTreap(self.root,root_m,root_r,self.op,self.idOp,self.map,self.comp,self.idComp)




proc traverseInorder[V,S,L](self: ImplicitTreap[V,S,L],
                            node: ImplicitNode[V,S],
                            yieldVal: proc (v: V) {.closure.}) =
  if node == nil: return
  # lazyありなら propagate してから子・自分の順に辿る
  when S is LazyState[L]:
    propagate(node, self.map, self.comp, self.idComp)
  traverseInorder(self, node.l, yieldVal)
  yieldVal(node.val)
  traverseInorder(self, node.r, yieldVal)

# ---- items ----
iterator items*[V,S,L](self: ImplicitTreap[V,S,L]): V =
  proc y(v: V) {.closure.} =
    yield v
  traverseInorder(self, self.root, y)

# ---- Map用: Pair[K,V] 前提 ----
type
  Pair[K,V] = object
    key: K
    val: V

# key, val に直接アクセスできるように when で型チェックしておくと安全
iterator pairs*[K,V,S,L](self: ImplicitTreap[Pair[K,V],S,L]): (K,V) =
  proc y(p: Pair[K,V]) {.closure.} =
    yield (p.key, p.val)
  traverseInorder(self, self.root, y)

iterator keys*[K,V,S,L](self: ImplicitTreap[Pair[K,V],S,L]): K =
  proc y(p: Pair[K,V]) {.closure.} =
    yield p.key
  traverseInorder(self, self.root, y)

iterator values*[K,V,S,L](self: ImplicitTreap[Pair[K,V],S,L]): V =
  proc y(p: Pair[K,V]) {.closure.} =
    yield p.val
  traverseInorder(self, self.root, y)


proc `==`*[V,S,L](a, b: ImplicitTreap[V,S,L]): bool =
  if a.len != b.len: return false
  # toSeq(items) で O(N) 比較
  return toSeq(a.items) == toSeq(b.items)

proc `<`*[V,S,L](a, b: ImplicitTreap[V,S,L]): bool =
  # 辞書順比較
  return toSeq(a.items) < toSeq(b.items)

proc `<=`*[V,S,L](a, b: ImplicitTreap[V,S,L]): bool =
  return not (b < a)



proc `==`*[K,V,S,L](a, b: ImplicitTreap[Pair[K,V],S,L]): bool =
  if a.len != b.len: return false
  return toSeq(a.items) == toSeq(b.items)

proc `<`*[K,V,S,L](a, b: ImplicitTreap[Pair[K,V],S,L]): bool =
  return toSeq(a.items) < toSeq(b.items)

proc `<=`*[K,V,S,L](a, b: ImplicitTreap[Pair[K,V],S,L]): bool =
  return not (b < a)






type 
  OpAggregates=tuple[sum:int,max:int,min:int,sz:int]
  AffineMapping=tuple[a:int,b:int]
proc infForIdentity[T]():T=
  when T is SomeInteger:
    result=int.inf
  elif T is SomeFloat:
    result=float.inf
  else:
    result=default(T) 
proc initAffineTreap(
#  isMulti = false,
# cmp: CompareOp[AllVal] = nil # 指定なければ system.cmp
#): ImplicitTreap[AllVal, Affine] =

  # 1. Operation (結合)
#  let op = proc(x, y: AllVal): AllVal = (
#    sum: x.sum + y.sum,
#    min: min(x.min, y.min),
#    max: max(x.max, y.max),
#    sz:  x.sz + y.sz
#  )

  # 2. Identity (単位元)
#  let idOp = proc(): AllVal = (sum: 0, min: infForIdentity(), max: -infForIdentity(), sz: 0)

  # 3. Composition (合成) g(f(x)) -> new(old(x))
##  let comp = proc(newOp, oldOp: Affine): Affine = (
##    a: newOp.a * oldOp.a,
##    b: newOp.a * oldOp.b + newOp.b
#  )

  # 4. ID Comp (恒等写像)
#  let idComp = proc(): Affine = (a: 1, b: 0)

  # 5. Mapping (作用) val * a + b
#  let map = proc(v: AllVal, f: Affine): AllVal = (
#    sum: v.sum * f.a + v.sz * f.b,
#    min: if v.sz == 0: infForIdentity() else: v.min * f.a + f.b,
#    max: if v.sz == 0: -infForIdentity() else: v.max * f.a + f.b,
#    sz:  v.sz
#  )
  
  # ※ アフィン倍率 a < 0 の時の min/max 反転対応を入れるならここでやる

  # 6. Compare (指定がなければデフォルト)
#  let actualCmp = if cmp != nil: cmp else: system.cmp[AllVal]

  # 本体作成
#  return initImplicitTreap(
#    op, idOp, comp, idComp, map, actualCmp, isMulti
#  )




# ---- おまけ: 配列化 (Dump) ----
proc toSeq*[V, L](self: var ImplicitTreap[V, L]): seq[V] =
  var res: seq[V] = @[]
  proc dfs(t: ImplicitNode[V, L]) =
    if t == nil: return
    propagate(t, self.map, self.comp, self.idComp) # ちゃんとpushしてから
    dfs(t.l)
    res.add(t.val)
    dfs(t.r)
  dfs(self.root)
  return res

# ---- サイズ取得 ----
proc len*[V, L](self: ImplicitTreap[V, L]): int =
  if self.root == nil: 0 else: self.root.size



# Mapのエントリー型を定義
type Pair[K, D] = object
  key: K
  val: D

# 暗黙的Treap定義
#type ImplicitTreap[V, L] = ...

# ---- Map専用の拡張API (V が Pair[K, D] のときだけ有効！) ----

proc `[]`*[K, D, L](self: var ImplicitTreap[Pair[K, D], L], k: K): D =
  # ダミーの検索用ペアを作る
  let searchKey = Pair[K, D](key: k, val: default(D))
  
  # 既存の lowerBoundIndex を利用！
  # (init時に cmp が key だけ比較するように設定されている前提)
  let idx = self.lowerBoundIndex(searchKey)
  
  if idx < self.len:
    let found = self.getAt(idx)
    if found.key == k: # キーが一致すればOK
      return found.val
      
  # なければエラーかデフォルト値
  raise newException(KeyError, "Key " & $k & " not found")

proc `[]=`*[K, D, L](self: var ImplicitTreap[Pair[K, D], L], k: K, v: D) =
  # 既存の deleteKey -> insertKey を利用
  let item = Pair[K, D](key: k, val: v)
  
  # ※Setモード(isMultiSet=false)なら、insertKeyだけで
  # 「あれば無視」ができるが、「更新（上書き）」したい場合は
  # 一度消す必要がある。
  self.deleteKey(item) 
  self.insertKey(item)


  

# ---- 便利メソッド (再掲) ----
# これらも定義しておくと完璧
#proc rangeAdd*(self: var ImplicitTreap[AllVal, Affine], l, r: int, val: int) =
#  self.applyMapping(l, r, (a: 1, b: val))

# ... (rangeUpdate, rangeMult, getSum, getMin ...)



proc count[K,V,L](self:var ImplicitTreap[Pair[K,V],L],key:K):int=
  return self.count((key:key,val:default(V)))