#!/bin/bash

cat -> list << EOF
Associate::resolveAssociates
Construct::changeIfStatementsInIfConstructs
Inline::inlineContainedSubroutines
FieldAPI::pointers2FieldAPIPtr
Loop::removeJlonLoops
Stack::addStack
DrHook::remove
EOF

set -x


./scripts/compile.pl \
  --arch cpu_intel --update --compile --transform-list file://list
