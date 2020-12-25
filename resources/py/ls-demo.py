#import numpy as np
import sys
import json

task = json.loads(sys.argv[2])
data = json.loads(sys.argv[1])
x = data.get("_x")
ret = []
for s in x:
    ret.append(len(s.split("\n")))
print(json.dumps({"ToExchange": {"FileAmount":ret}}))