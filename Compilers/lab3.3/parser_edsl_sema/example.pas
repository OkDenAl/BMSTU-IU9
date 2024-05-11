{ Попробуйте внести сюда семантические ошибки и посмотреть реакцию }
var
  (* переменные *)
  x: integer;
  y: real;
  z: boolean;
begin
  { операторы }
  x := 100;
  y := 2.5;
  z := x > y;
  if z and not (x / y > 7.0) then
    while x > y do
      y := x - y
  else
    y := + x * y;
  for x := 1 to 10 do
    y := - y ** x;
end
