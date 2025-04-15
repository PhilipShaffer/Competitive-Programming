define _main():
L1:
  goto L3

L3:
  x := 20
  goto L4

L4:
  call _print_int(x)
  goto L6

L6:
  x := 40
  goto L7

L7:
  call _print_int(x)
  return