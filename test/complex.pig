f(x: float, y: float) -> float := {
  z : float := x - y;
  return z
};

a : float := 2.0;
b : float := 3.0;
c : float := f(a, b);
if c > 4.0 {
  print 3
} else {
  print 4
}