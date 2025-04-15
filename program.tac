define _main():
L2:
  x := 5
  y := 10
  goto L3

L3:
  goto L1_inline1

L1_inline1:
  _t1_inline1 := x + y
  _t4 := _t1_inline1
  goto _inline2

_inline2:
  call _print_int(_t4)

L5:
  return

define add(x, y):
L1:
  _t1 := x + y
  return _t1