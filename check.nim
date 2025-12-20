include "template.nim"
include "Structure/Implicit_Treap.nim"
var ms = initImplicitTreap[int]()

ms.insert(5)
ms.insert(2)
ms.insert(8)
ms.insert(5) # 重複許可
ms.insert(1)

# ソートされているはず: 1, 2, 5, 5, 8
for i in 0 ..< ms.len:
  stdout.write ms[i], " "
echo ""