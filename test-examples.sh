#!/bin/sh

find doc/examples/ -type f | while read f; do ( ./dist/build/debts/debts "$f" > /dev/null 2>&1 ) || echo "FAILED: $f"; done
