program NeuralNetStart;

{$mode objfpc}{$H+}

uses
  SysUtils, // Для StrToFloat
  umemory;  // Наш модуль памяти

var
  InputData: TFloatVector;
  i: Integer;
  Val: TFloat;
  Target: TFloat; // То самое "последнее число"

begin
  // 1. Инициализируем нашу память
  Vector_Init(InputData);

  // 2. Читаем аргументы командной строки
  // ParamCount возвращает кол-во аргументов.
  // ParamStr(i) возвращает строковое значение аргумента.
  try
    if ParamCount < 3 then
    begin
      WriteLn('Error: Provide at least 3 numbers.');
      WriteLn('Example: main.exe 1 2 3 4');
      Halt(1); // Выход с ошибкой
    end;

    // Цикл по всем аргументам
    for i := 1 to ParamCount do
    begin
      try
        // Пытаемся превратить строку в Double
        // Важно: в консоли разделитель может зависеть от локали (точка или запятая)
        // Для надежности лучше использовать настройки формата, но пока берем дефолт.
        Val := StrToFloat(ParamStr(i));
        
        // Кладем в наш "ручной" массив
        Vector_Push(InputData, Val);
      except
        WriteLn('Error: "', ParamStr(i), '" is not a valid number.');
        Halt(1);
      end;
    end;

    // 3. Логика разделения данных
    // Последний элемент — это цель (Target), остальные — обучение.
    // Получаем значение последнего элемента:
    Target := Vector_Get(InputData, InputData.Count - 1);
    
    // По сути, нам надо "забыть" про последний элемент в векторе для обучения.
    // Мы можем просто уменьшить Count на 1. Данные в памяти останутся, 
    // но логически массив станет короче.
    Dec(InputData.Count);

    // 4. Вывод диагностики
    WriteLn('--- Memory Structure Check ---');
    WriteLn('Inputs (Training):');
    for i := 0 to InputData.Count - 1 do
    begin
      Write(Vector_Get(InputData, i):0:4, ' ');
    end;
    WriteLn;
    WriteLn('Target (Expectation): ', Target:0:4);
    
    WriteLn('Memory Capacity Used: ', InputData.Count + 1, '/', InputData.Capacity);

  finally
    // Всегда чистим за собой
    Vector_Free(InputData);
  end;
end.
