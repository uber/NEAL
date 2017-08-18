#! /usr/bin/env python

import ast
import json
import sys
from ast2json import ast2json

if len(sys.argv) > 1:
    with open(sys.argv[1]) as file:
        source = file.read()
else:
    source = sys.stdin.read()

absyn = ast.parse(source)
absyn_json = ast2json(absyn)
print json.dumps(absyn_json)
