#!/bin/bash

./scripts/compile.pl \
  --arch cpu_intel --update --list \
  Associate::resolveAssociates,Construct::changeIfStatementsInIfConstructs,Inline::inlineContainedSubroutines,DrHook::remove,FieldAPI::pointers2FieldAPIPtr
