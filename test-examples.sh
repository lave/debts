#!/bin/sh

find doc/examples/ -type f | while read f; do ( ./debts "$f" > /dev/null 2>&1 ) || echo "FAILED: $f"; done
