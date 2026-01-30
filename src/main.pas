program NeuralNetStart;

{$mode objfpc}{$H+}

uses
  SysUtils,
  umemory,
  unetwork,
  uutils; // Наш парсер для работы с вводом

var
  InputData: TFloatVector;
  MyNeuron: TNeuron;
  InputLine: String;
  Target: TFloat;
  i: Integer;

begin
  // Настройка форматов
  DefaultFormatSettings.DecimalSeparator := '.';
  Randomize;
  
  // Инициализация памяти один раз
  Vector_Init(InputData);
  
  WriteLn('=== NEURAL NETWORK: HARDWARE DEBUG MODE ===');
  WriteLn('Enter data sequence (e.g. "1 2 3"). Last number is TARGET.');
  WriteLn('Type "exit" to quit.');
  
  // --- ГЛАВНЫЙ ЦИКЛ (REPL) ---
  while not Eof do
  begin
    WriteLn;
    Write('Input > ');
    ReadLn(InputLine);
    
    if (InputLine = '') then Continue;
    if (LowerCase(Trim(InputLine)) = 'exit') then Break;

    // 1. ПАРСИНГ
    // Вместо цикла по ParamStr мы используем наш парсер строки
    ParseLineToVector(InputLine, InputData);

    // Проверка на минимальное количество данных
    if InputData.Count < 3 then
    begin
      WriteLn('[!] Error: Provide at least 3 numbers.');
      Continue; // Идем на следующий круг, не закрывая программу
    end;

    // --- НАЧАЛО ТВОЕГО КОДА С ДИАГНОСТИКОЙ ---

    // Подготовка данных
    Target := Vector_Get(InputData, InputData.Count - 1);
    Dec(InputData.Count); // Скрываем цель

    // STEP 1
    WriteLn('========================================');
    WriteLn(' STEP 1: MEMORY & DATA PREPARATION');
    WriteLn('========================================');
    // Я добавил вывод адреса памяти, это очень наглядно при масштабировании
    WriteLn('Memory Address (RAM): ', PtrUInt(InputData.Data)); 
    WriteLn('Raw Vector Capacity : ', InputData.Capacity);
    WriteLn('Items in Memory     : ', InputData.Count + 1);
    WriteLn('Training Input Count: ', InputData.Count);
    
    Write('Training Data: [ ');
    for i := 0 to InputData.Count - 1 do
      Write(Vector_Get(InputData, i):0:2, ' ');
    WriteLn(']');
    
    WriteLn('Target Value : ', Target:0:4);
    WriteLn;

    // STEP 2
    WriteLn('========================================');
    WriteLn(' STEP 2: NEURON INITIALIZATION');
    WriteLn('========================================');
    
    Neuron_Init(MyNeuron, InputData.Count);

    Write('Initial Weights (Random): [ ');
    for i := 0 to MyNeuron.Weights.Count - 1 do
      Write(Vector_Get(MyNeuron.Weights, i):0:4, ' ');
    WriteLn(']');
    WriteLn;

    // STEP 3
    WriteLn('========================================');
    WriteLn(' STEP 3: CALCULATION (FORWARD PASS)');
    WriteLn('========================================');
    
    Neuron_Forward(MyNeuron, InputData);

    WriteLn('Neuron Prediction: ', MyNeuron.Output:0:4);
    WriteLn('Actual Target    : ', Target:0:4);
    WriteLn('Error (Delta)    : ', (Target - MyNeuron.Output):0:4);
    WriteLn('========================================');

    // Очистка только нейрона (вектор данных переиспользуется)
    Neuron_Free(MyNeuron);
  end;

  // Финальная очистка
  Vector_Free(InputData);
  WriteLn('Program finished.');
end.
