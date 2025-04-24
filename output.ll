ld: warning: ignoring duplicate libraries: '-lLLVMSupport'
ld: warning: reexported library with install name '/opt/homebrew/opt/llvm@18/lib/libunwind.1.dylib' found at '/opt/homebrew/Cellar/llvm@18/18.1.8/lib/libunwind.1.0.dylib' couldn't be matched with any parent library and will be linked directly
HIR generated successfully!
HBlock([HFunDecl(0, [1:int, 2:int], int, HBlock([HDeclare(3, int, HBinop(+, HVar(1:int), HVar(2:int), int)); HReturn(HVar(3:int))])); HDeclare(4, int, HInt(2)); HDeclare(5, int, HInt(3)); HDeclare(6, int, HFunCall(0, [HVar(4:int), HVar(5:int)], int)); HIf(HBinop(>, HVar(6:int), HInt(4), bool), HBlock([HDeclare(7, int, HInt(1))]), HBlock([HDeclare(8, int, HInt(2))]))])
