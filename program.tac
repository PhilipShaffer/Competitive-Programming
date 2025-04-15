define _main():
L2:
  x := 5
  y := 10
  if 1 goto L3 else goto L4

L4:
  call _print_int(y)
  goto L5

L3:
  _t4 := call add(x, y)
  call _print_int(_t4)

L5:
  return

define add(x, y):
L1:
  _t1 := x + y
  return _t1