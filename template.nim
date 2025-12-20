{.warning[UnusedImport]:off.}
import math,lenientops,strutils,re,strformat,parseutils,sequtils,algorithm,sets,tables,deques,heapqueue,macros,bitops,rationals,random,sugar,atcoder
template inf(T:typedesc[int]):int=10**18
template inf(T:typedesc[float]):float=1e18
template ceilDiv[T:SomeSignedInt](a,b:T):int= -floorDiv(-a,b)
func `///`[T](n,d:T):Rational[T]=initRational(n,d)
macro defineOperators(opSets:untyped):untyped=
  result=newStmtList()
  for opSet in opSets:
    let typesList=opSet[0]; let opsList=opSet[1]
    for typePair in typesList[1]:
      let inT=typePair[0]; let outT=typePair[1]
      for opPair in opsList[1]:
        let name=opPair[0]; let body=opPair[1] 
        result.add newProc(ident(name[0].strVal),[outT,newNimNode(nnkIdentDefs).add(ident("x"),ident("y"),inT,newEmptyNode())],body)
        result.add newProc(ident((name[0].strVal)&"="),[newEmptyNode(),newNimNode(nnkIdentDefs).add(ident("x"),newNimNode(nnkVarTy).add(inT),newEmptyNode()),newNimNode(nnkIdentDefs).add(ident("y"),inT,newEmptyNode())],newAssignment(ident("x"),body))
defineOperators([(@[(int,int)],@[(`//`,x div y),(`%`,x mod y),(`**`,x^y),(`>>`,x shr y),(`<<`,x shl y)]),(@[(int,int),(bool,bool)],@[(`&`,x and y),(`|`,x or y),(`^`,x xor y)])])
template `~`(x:bool):bool=not x
template `~`(x:int):int=not x
template `~=`(x:var bool):void=x=not x
template `~=`(x:var int):void=x=not x
template `=-`(lhs,rhs:untyped)=
  lhs = -rhs
template `=@`(lhs,rhs:untyped)=
  lhs = @rhs
template `:=`(lhs,rhs:untyped)=
  var lhs = rhs
template `:=-`(lhs,rhs:untyped)=
  var lhs = -rhs
template `:=@`(lhs,rhs:untyped)=
  var lhs = @rhs
template `==-`(x,y:int):bool=x == -y
template `==@`[n,T](x:seq[T],y:array[n,T]):bool=x == @y
proc `<`[T](a,b:openArray[T]):bool=
  for i in 0..<min(a.len,b.len):
    if a[i]<b[i]: return true
    if a[i]>b[i]: return false
  return a.len<b.len
proc `<=`[T](a,b:openArray[T]):bool=
  return not b<a
proc `[]`(x:int,i:int):int=x shr i and 1
converter intToBool(x:int):bool=x!=0
proc `@`(x:char,a='a'):int=ord(x)-ord(a)
proc getcharUnlocked():cint {.header:"<stdio.h>",importc:"getchar_unlocked".}
proc validChar():cint {.inline.}=
  result=getcharUnlocked()
  while result in {8..13, 32}: result=getcharUnlocked()
proc input(x:var int){.inline.}=
  var c=validChar(); var s=1
  if c==45: s=-1; c=getcharUnlocked()
  x=0
  while c in 48..57: x=x*10+(c-48); c=getcharUnlocked()
  x*=s
proc input(x:var string){.inline.}=
  var c=validChar()
  x=""
  while c>32: x.add(c.char); c=getcharUnlocked()
proc input(x:var float)=
  var s:string; s.input; x=parseFloat(s)
proc input[T](s:var seq[T])=
  for i in 0..<s.len: s[i].input
proc input[T](s:var seq[seq[T]])=
  for i in 0..<s.len: s[i].input
proc input(T:typedesc[int]):int=
  result.input
proc input(T:typedesc[string]):string=
  result.input
proc input(T:typedesc[float]):float=
  result.input
macro input(t:tuple):untyped=
  let
    len=newDotExpr(t[0],ident("len")) 
    i=ident("i")
  var body=newStmtList()
  for s in t: body.add newCall("input",newTree(nnkBracketExpr,s,i))
  result=quote do:
    for `i` in 0..<`len`: `body`
template echo(v:float)=echo(fmt"{v:.20f}")
type InitSeq=object
const Seq=InitSeq()
template makeSeq[T](len:int;init:T):auto=newSeqWith(len,init)
template makeSeq(len:int;init:typedesc):auto=newSeq[init](len)
macro `[]`(s:InitSeq;args:varargs[untyped]):untyped=
  if args.len==1 and args[0].kind!=nnkExprColonExpr:
    return newCall(newTree(nnkBracketExpr,ident("newSeq"),args[0]))
  result = newCall(ident("makeSeq"),args[^1][0],args[^1][1])
  for i in countdown(args.len-2,0):
    result=newCall(ident("makeSeq"),args[i],result)
iterator items(t:(HSlice[int,int],int)):int=
  let (s,step)=t
  if s.a<=s.b:
    for i in countup(s.a,s.b,step): yield i
  else:
    for i in countdown(s.a,s.b,step): yield i
proc isqrt(n:int):int{.inline.}=
  var x=n; var nx=(x+1) div 2
  while x>nx: x=nx; nx=(x+n div x) div 2
  return x
proc chmax[T](a:var T,b:T):bool{.discardable,inline.}=
  if a<b: a=b; return true
  else: return false
proc chmin[T](a:var T,b:T):bool{.discardable,inline.}=
  if a>b: a=b; return true
  else: return false
template loop(n:int,body:untyped)=
  for _ in 1..n: body