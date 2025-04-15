define _main():
L2:
  goto L3

L3:
  goto L1_inline1

L1_inline1:
  _t4 := 15
  goto _inline2

_inline2:
  call _print_int(15)

L5:
  return