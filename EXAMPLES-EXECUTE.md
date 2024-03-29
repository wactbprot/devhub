# EXECUTE examples

Set

```shell
H="Content-Type: application/json"
URL=http://localhost:9009/

```

and use it this way

```shell
curl -H "$H" -d "$D" -X POST $URL
```

```shell
D='{"Action":"EXECUTE","Cmd":"ls", "Wait":100 , "Repeat":2}'

## =>
## {
##    "_x":[
##       {
##          "exit":0,
##          "out":"CHANGELOG.md\ndoc\ndocs\nLICENSE\npre-commit.sh\nproject.clj\nREADME.md\nresources\nsrc\ntarget\ntest\n",
##          "err":""
##       },
##       {
##          "exit":0,
##          "out":"CHANGELOG.md\ndoc\ndocs\nLICENSE\npre-commit.sh\nproject.clj\nREADME.md\nresources\nsrc\ntarget\ntest\n",
##          "err":""
##       }
##    ],
##    "_t_start":[
##       "1608482700761",
##       "1608482700871"
##    ],
##    "_t_stop":[
##       "1608482700771",
##       "1608482700875"
##    ],
##    "_dt":[
##       10,
##       4
##    ]
## }
```


```shell
D='{"Action":"EXECUTE", "Cmd":"ls", "Wait":100, "Repeat":5, "PostScriptPy": "ls-demo"}'

## =>
## {"ToExchange":{"FileAmount":[12,12,12,12,12]}}
```

Read out the second channel of a Inficon VGC50x via a direct connected usb wire:

```shell
D='{"Action":"EXECUTE", "Cmd":"python3 resources/py/inficon-vgc.py 2 10 1"}'
```

With post processing

```shell
D='{"Action":"EXECUTE", "Cmd":"python3 py/inficon-vgc.py 2 3 0.1", "PostScript":"inf-vgc.read-out", "PostScriptInput":{"Type":"ind", "Unit":"mbar"}}'
```
