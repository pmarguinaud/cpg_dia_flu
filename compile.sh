#!/bin/bash

mkdir -p tmp

cat -> tmp/list << EOF
Associate::resolveAssociates
Construct::changeIfStatementsInIfConstructs
Inline::inlineContainedSubroutines
FieldAPI::pointers2FieldAPIPtr
Loop::removeJlonLoops
Stack::addStack
EOF

# DrHook::remove

set -x

cat -> tmp/list << EOF
Stack::addStack
EOF

./scripts/compile.pl \
  --arch cpu_intel --update --compile --transform-list file://tmp/list --external-drhook
