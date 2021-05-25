# VXI examples

Set

```shell
H="Content-Type: application/json"
URL=http://localhost:9009/
```

and use it this way

```shell
curl -H "$H" -d "$D" -X POST $URL
```

## SRG VM212
```shell
D='{"TaskName": "vxi-test", "Action":"VXI11", "Device":"gpib0,26", "Host":"e75465", "Value":"", "Wait":30000, "PostScript":"vm212.read-out", "PostScriptInput":{"Type":"srg"}, "Repeat":5}'
```

## QBS Ruska 7017
```shell
D='{"TaskName": "vxi-test", "Action":"VXI11", "Device":"gpib0,4", "Host":"e75416", "Value":"MEAS:PRES?\n", "Wait":1000, "Repeat":5}'
```
