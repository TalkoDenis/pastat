unit umemory;

{$mode objfpc}{$H+}

interface

uses SysUtils; // Нужно для генерации исключений/ошибок

type
  TFloat = Double;
  PFloat = ^TFloat;

  TFloatVector = record
    Data: PFloat;
    Count: Integer;
    Capacity: Integer;
  end;
  
  PFloatVector = ^TFloatVector;

procedure Vector_Init(var V: TFloatVector; InitialCapacity: Integer = 4);
procedure Vector_Push(var V: TFloatVector; Value: TFloat);
function Vector_Get(const V: TFloatVector; Index: Integer): TFloat;
procedure Vector_Set(var V: TFloatVector; Index: Integer; Value: TFloat); // Добавим возможность менять значения
procedure Vector_Free(var V: TFloatVector);

implementation

procedure Vector_Init(var V: TFloatVector; InitialCapacity: Integer);
begin
  if InitialCapacity < 1 then InitialCapacity := 4; // Защита от глупости
  V.Count := 0;
  V.Capacity := InitialCapacity;
  GetMem(V.Data, V.Capacity * SizeOf(TFloat));
end;

procedure Vector_Push(var V: TFloatVector; Value: TFloat);
var
  NewCapacity: Integer;
begin
  if V.Count >= V.Capacity then
  begin
    NewCapacity := V.Capacity * 2;
    if NewCapacity < 4 then NewCapacity := 4;
    
    // Пытаемся перевыделить память
    ReallocMem(V.Data, NewCapacity * SizeOf(TFloat));
    
    // Проверка: дала ли система память? (В современных ОС редко бывает nil, но для порядка)
    if V.Data = nil then
    begin
      WriteLn('CRITICAL ERROR: Out of memory in Vector_Push');
      Halt(255);
    end;
    
    V.Capacity := NewCapacity;
  end;

  (V.Data + V.Count)^ := Value;
  Inc(V.Count);
end;

function Vector_Get(const V: TFloatVector; Index: Integer): TFloat;
begin
  // ВАЖНАЯ ПРОВЕРКА, которую ты просил
  if (Index < 0) or (Index >= V.Count) then
  begin
    // Выводим понятную ошибку и останавливаем программу
    WriteLn('ERROR: Vector index out of bounds!');
    WriteLn('Requested Index: ', Index);
    WriteLn('Vector Size: ', V.Count);
    Halt(1); // Аварийный выход
  end;

  Result := (V.Data + Index)^;
end;

// Новая процедура: Запись значения в существующую ячейку
procedure Vector_Set(var V: TFloatVector; Index: Integer; Value: TFloat);
begin
  if (Index < 0) or (Index >= V.Count) then
  begin
    WriteLn('ERROR: Vector_Set index out of bounds!');
    WriteLn('Index: ', Index, ', Size: ', V.Count);
    Halt(1);
  end;
  
  (V.Data + Index)^ := Value;
end;

procedure Vector_Free(var V: TFloatVector);
begin
  if V.Data <> nil then
  begin
    FreeMem(V.Data);
    V.Data := nil;
  end;
  V.Count := 0;
  V.Capacity := 0;
end;

end.
