% Лабораторная работа № 1.1. Раскрутка самоприменимого компилятора
% 7 февраля 2023 г.
% Окутин Денис, ИУ9-61Б

# Цель работы
Целью данной работы является ознакомление с раскруткой самоприменимых 
компиляторов на примере модельного компилятора.

# Индивидуальный вариант
Компилятор BeRo. Добавить однострочный комментарий, начинающийся с символа `?`.
Т.е. суффикс строки программы, расположенный после символа `?`,
должен считаться комментарием.
 
*Уточнение: при этом поддержка привычного однострочного комментария `//` сохраняется.*


# Реализация

Различие между файлами `btpc64macOS.pas` и `btpc64macOS2.pas`:

```diff
--- btpc64macOS.pas	2024-02-07 17:00:28
+++ btpc64macOS2.pas	2024-02-07 19:29:21
@@ -785,16 +785,21 @@
   end;
   ReadChar;
   GetSymbol;
+ end else if CurrentChar='?' then begin
+  repeat
+   ReadChar;
+  until (CurrentChar=#10) or (CurrentChar=#0);
+  GetSymbol;
  end else if CurrentChar='/' then begin
-  ReadChar;
-  if CurrentChar='/' then begin
-   repeat
-    ReadChar;
-   until (CurrentChar=#10) or (CurrentChar=#0);
-   GetSymbol;
-  end else begin
-   Error(102);
-  end;
+   ReadChar;
+   if CurrentChar='/' then begin
+    repeat
+     ReadChar;
+    until (CurrentChar=#10) or (CurrentChar=#0);
+    GetSymbol;
+   end else begin
+    Error(102);
+   end;
  end else begin
   Error(102);
  end;
```

Различие между файлами `btpc64macOS2.pas` и `btpc64macOS3.pas`:

```diff
--- btpc64macOS2.pas	2024-02-07 19:29:21
+++ btpc64macOS3.pas	2024-02-07 19:42:55
@@ -2915,7 +2915,7 @@
     OCPopEBX;
     OCPopEAX;
     OCCmpEAXEBX;
-    EmitByte($0f); EmitByte($94); EmitByte($d0); // SETE AL
+    EmitByte($0f); EmitByte($94); EmitByte($d0); ? SETE AL
     LastOutputCodeValue:=locNone;
     OCMovzxEAXAL;
     OCPushEAX;
@@ -2924,7 +2924,7 @@
     OCPopEBX;
     OCPopEAX;
     OCCmpEAXEBX;
-    EmitByte($0f); EmitByte($95); EmitByte($d0); // SETNE AL
+    EmitByte($0f); EmitByte($95); EmitByte($d0); ? SETNE AL
     LastOutputCodeValue:=locNone;
     OCMovzxEAXAL;
     OCPushEAX;
@@ -2933,7 +2933,7 @@
     OCPopEBX;
     OCPopEAX;
     OCCmpEAXEBX;
-    EmitByte($0f); EmitByte($9c); EmitByte($d0); // SETL AL
+    EmitByte($0f); EmitByte($9c); EmitByte($d0); ? SETL AL
     LastOutputCodeValue:=locNone;
     OCMovzxEAXAL;
     OCPushEAX;
@@ -2942,7 +2942,7 @@
     OCPopEBX;
     OCPopEAX;
     OCCmpEAXEBX;
-    EmitByte($0f); EmitByte($9e); EmitByte($d0); // SETLE AL
+    EmitByte($0f); EmitByte($9e); EmitByte($d0); ? SETLE AL
     LastOutputCodeValue:=locNone;
     OCMovzxEAXAL;
     OCPushEAX;
@@ -2951,7 +2951,7 @@
     OCPopEBX;
     OCPopEAX;
     OCCmpEAXEBX;
-    EmitByte($0f); EmitByte($9f); EmitByte($d0); // SETG AL
+    EmitByte($0f); EmitByte($9f); EmitByte($d0); ? SETG AL
     LastOutputCodeValue:=locNone;
     OCMovzxEAXAL;
     OCPushEAX;
@@ -2960,13 +2960,13 @@
     OCPopEBX;
     OCPopEAX;
     OCCmpEAXEBX;
-    EmitByte($0f); EmitByte($9d); EmitByte($d0); // SETGE AL
+    EmitByte($0f); EmitByte($9d); EmitByte($d0); ? SETGE AL
     LastOutputCodeValue:=locNone;
     OCMovzxEAXAL;
     OCPushEAX;
    end;
    OPDupl:begin
-    EmitByte($ff); EmitByte($34); EmitByte($24); // PUSH DWORD PTR [ESP]
+    EmitByte($ff); EmitByte($34); EmitByte($24); ? PUSH DWORD PTR [ESP]
     LastOutputCodeValue:=locNone;
    end;
    OPSwap:begin
```

# Тестирование

Тестовый пример:

```pascal
program Lab1;

var
  numbers: array[1..5] of integer;
  i: integer;

begin
  ? новый комментарий
  // старый коммент
  ? ? ? //
  for i := 1 to 5 do
  begin
    numbers[i] := i * 10;
  end;

  ? вывод элементов массива
  for i := 1 to 5 do
  begin
    writeln('Element ?', i, ': ', numbers[i]);
  end;
end.

```

Вывод тестового примера на `stdout`

```
Element ?1: 10
Element ?2: 20
Element ?3: 30
Element ?4: 40
Element ?5: 50
```

Как видим, комментарии поддерживаются.

# Вывод
В ходе данной лабораторной работы было проведено ознакомление с раскруткой 
самоприменимых компиляторов на примере модельного компилятора BeRo,
получен опыт программирования на языке Pascal
(*даже небольшая ностальгия по первым написанным программам во время обучения в школе*)
и выполнена поставленная задача.

P.s Самоприменимые компиляторы по началу немного ломают мозг,
но когда удаётся это осознать, то факт их существования
и применения становится очень интересным.