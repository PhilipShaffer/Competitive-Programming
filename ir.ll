HIR generated successfully!
HBlock([HFunDecl(0, [1:int, 2:int], int, HBlock([HDeclare(3, int, HBinop(+, HVar(1:int), HVar(2:int), int)); HReturn(HVar(3:int))])); HDeclare(4, int, HInt(2)); HDeclare(5, int, HInt(3)); HDeclare(6, int, HFunCall(0, [HVar(4:int), HVar(5:int)], int)); HIf(HBinop(>, HVar(6:int), HInt(4), bool), HBlock([HDeclare(7, int, HInt(1))]), HBlock([HDeclare(8, int, HInt(2))]))])
