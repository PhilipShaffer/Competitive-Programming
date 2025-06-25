f(a: float, b: float) -> float := {
  z : float := a - b;
  return z
};

a : float := 2.0;
b : float := 3.0;
c : float := f(7.0, a);
if c > 4.0 {
  print 3
} else {
  print 4
}