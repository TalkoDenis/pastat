unit umath;

{$mode objfpc}{$H+}

interface

type
  TFloat = Double;

// Функция активации ReLU (отсекает отрицательные значения)
// Самая популярная функция для скрытых слоёв
function ReLU(X: TFloat): TFloat;

// Производная ReLU (нужна для обучения)
function ReLU_Derivative(X: TFloat): TFloat;

implementation

function ReLU(X: TFloat): TFloat;
begin  
  if X > 0.0 then Result := X else Result := 0.0;
end;

function ReLU_Derivative(X: TFloat): TFloat;
begin
  if X > 0.0 then Result := 1.0 else Result := 0.0;
end;

end.
