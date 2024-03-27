var
  (* переменные *)
  x: integer;
  y: real;
  b: boolean;
begin
  { операторы }
  x := 100;
  y := 2.5;
  z := x > y;
  if z and (x / y > 7.0) then
    while x > y do
      x := x - y
  else
    x := x * y;
end
