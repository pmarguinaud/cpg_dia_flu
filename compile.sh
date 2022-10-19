#!/bin/bash

./scripts/compile.pl \
  --arch cpu_intel --update --compile --list \
  Associate::resolveAssociates,Construct::changeIfStatementsInIfConstructs,Inline::inlineContainedSubroutines,FieldAPI::pointers2FieldAPIPtr,Loop::removeJlonLoops,DrHook::remove
