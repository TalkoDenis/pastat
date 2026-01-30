unit uutils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, umemory;

procedure ParseLineToVector(const Line: String; var V: TFloatVector);

implementation

// Процедура: берет строку, ищет числа, складывает в наш Vector
procedure ParseLineToVector(const Line: String; var V: TFloatVector);
var
  StartPos, EndPos, Len: Integer;
  SubStr: String;
  Val: TFloat;
begin
  // Очищаем вектор перед записью (если он был использован)
  // Но память не освобождаем полностью, просто сбрасываем счетчик (оптимизация!)
  V.Count := 0; 
  
  Len := Length(Line);
  StartPos := 1;
  
  while StartPos <= Len do
  begin
    // 1. Пропускаем пробелы
    while (StartPos <= Len) and (Line[StartPos] <= ' ') do
      Inc(StartPos);
      
    if StartPos > Len then Break;
    
    // 2. Ищем конец слова
    EndPos := StartPos;
    while (EndPos <= Len) and (Line[EndPos] > ' ') do
      Inc(EndPos);
      
    // 3. Вырезаем кусок
    SubStr := Copy(Line, StartPos, EndPos - StartPos);
    
    // 4. Пробуем конвертировать
    if TryStrToFloat(SubStr, Val) then
    begin
      Vector_Push(V, Val);
    end;
    
    StartPos := EndPos;
  end;
end;

end.
