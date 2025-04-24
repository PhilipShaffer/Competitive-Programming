f(x: int, y: int) -> int := {
  z : int := x + y;
  return z
};

a : int := 2;
b : int := 3;
c : int := f(a, b);
if c > 4 then {
  k : int := 1
} else {
  p : int := 2
}