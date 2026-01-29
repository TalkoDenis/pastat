unit unetwork;

{$mode objfpc}{$H+}

interface

uses
  umemory, umath; // umath создадим следующим шагом, пока просто объявим

type
  TNeuron = record
    Weights: TFloatVector; // Вектор весов (по одному весу на каждый вход)
    Output: TFloat;        // Результат после активации
    // Позже добавим сюда градиенты для обучения
  end;
  
  PNeuron = ^TNeuron;

// Инициализация нейрона
// InputCount - сколько входов будет у нейрона (размер вектора весов)
procedure Neuron_Init(var N: TNeuron; InputCount: Integer);
procedure Neuron_Free(var N: TNeuron);

// Главная функция нейрона: "Подумать"
// Принимает входные данные, возвращает результат
procedure Neuron_Forward(var N: TNeuron; const Inputs: TFloatVector);

implementation

procedure Neuron_Init(var N: TNeuron; InputCount: Integer);
var
  i: Integer;
begin
  // Инициализируем вектор весов
  // Емкость ставим равной кол-ву входов, так как весов ровно столько же, сколько входов
  Vector_Init(N.Weights, InputCount);
  
  // Важный момент! Нейросеть не может учиться, если веса равны 0.
  // Ей нужна "асимметрия" для старта.
  // Обычно веса заполняют случайными числами от -1 до 1.
  Randomize;
  for i := 0 to InputCount - 1 do
  begin
    // Random возвращает 0..1.
    // (Random * 2) -> 0..2
    // (Random * 2) - 1 -> -1..1
    Vector_Push(N.Weights, (Random * 2.0) - 1.0);
  end;
  
  N.Output := 0.0;
end;

procedure Neuron_Free(var N: TNeuron);
begin
  Vector_Free(N.Weights);
  N.Output := 0.0;
end;

// ПРЯМОЕ РАСПРОСТРАНЕНИЕ (Forward Pass)
// Суть: Сумма(Вход[i] * Вес[i])
procedure Neuron_Forward(var N: TNeuron; const Inputs: TFloatVector);
var
  Sum: TFloat;
  i: Integer;
begin
  // Проверка на совместимость
  // Количество входов должно совпадать с количеством весов
  if Inputs.Count <> N.Weights.Count then
  begin
    WriteLn('ERROR: Input size (', Inputs.Count, ') does not match Weights size (', N.Weights.Count, ')');
    Halt(1);
  end;

  Sum := 0.0;
  
  // Взвешенная сумма
  for i := 0 to Inputs.Count - 1 do
  begin
    // Sum = Sum + (Input[i] * Weight[i])
    Sum := Sum + (Vector_Get(Inputs, i) * Vector_Get(N.Weights, i));
  end;

  // Здесь должна быть Функция Активации (например, Sigmoid или ReLU).
  // Пока сделаем "Линейную" активацию (то есть без изменений),
  // так как для предсказания чисел (регрессия) на выходе это нормально.
  N.Output := Sum;
end;

end.
