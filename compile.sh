#!/bin/bash

./scripts/compile.pl \
  --arch cpu_intel --update --compile --list \
  Associate::resolveAssociates,Construct::changeIfStatementsInIfConstructs,Inline::inlineContainedSubroutines,FieldAPI::pointers2FieldAPIPtr

# Associate::resolveAssociates,Construct::changeIfStatementsInIfConstructs,Inline::inlineContainedSubroutines,DrHook::remove,FieldAPI::pointers2FieldAPIPtr
