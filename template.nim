include atcoder/header
#let a,b,c=stdin.readLine.split
#let k=stdin.readLine.split.map(parseInt)
proc `/`(x,y:int):float=x/y
proc `//`(x,y:int):int=x div y
proc `%`(x,y:int):int=x mod y
proc `**`(x,y:int):int=x^y
proc `~`(x:int):int=not x
proc `&`(x,y:int):int=x and y
proc `|`(x,y:int):int=x or y
proc `^`(x,y:int):int=x xor y
proc `>>`(x,y:int):int=x shr y
proc `<<`(x,y:int):int=x shl y

proc `~`(x:bool):bool=not x
proc `&`(x,y:bool):bool=x and y
proc `|`(x,y:bool):bool=x or y
proc `^`(x,y:bool):bool=x xor y
proc `/=`(x:var float,y:float):void=x=x/y
proc `//=`[T](x:var T,y:T):void=x=x div y
proc `%=`[T](x:var T,y:T):void=x=x mod y
proc `**=`[T](x:var T,y:T):void=x=x^y
proc `&=`[T](x:var T,y:T):void=x=x and y
proc `|=`[T](x:var T,y:T):void=x=x or y
proc `^=`[T](x:var T,y:T):void=x=x xor y
proc `<<=`[T](x:var T,y:int):void=x=x shl y
proc `>>=`[T](x:var T,y:int):void=x=x shr y