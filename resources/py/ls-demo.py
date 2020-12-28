## ---------------------------------------------------
## import what you want and need
import sys
import json

## ---------------------------------------------------
## 2nd arg is data:
data = json.loads(sys.argv[1])

## ---------------------------------------------------
## 3rd arg is task:
task = json.loads(sys.argv[2])

## ---------------------------------------------------
## data consist of _x, _t_start and _t_stop
x = data.get("_x")

## ---------------------------------------------------
## do whatever
ret = x.split("\n")
    
## ---------------------------------------------------
## print writes to standard out
## and should be json
print(json.dumps({"ToExchange": {"FilesVector": ret}}))
