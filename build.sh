#!/bin/bash
mkdir -p bin
mkdir -p obj
# -FE: куда класть exe, -FU: куда класть .o, -Fu: где искать модули
fpc src/main.pas -FEbin -FUobj -Fusrc
echo "Build Complete. Run: ./bin/main"
