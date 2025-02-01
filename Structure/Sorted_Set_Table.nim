import atcoder/extra/structure/set_map
var s=initSortedSet[T](seq)
(*s.begin())
(*s.end().pred)

s.incl(x)
s.excl(x) #同一の複数要素が一度に消えてしまう
s.find(x)-s.begin()
s.lower_bound(x)-s.begin()
s.upper_bound(x)-s.begin()

var s=initSortedMultiSet[T]()

var t=initSortedMap[K,V]()

var t=initSortedMultiMap[K,V]()