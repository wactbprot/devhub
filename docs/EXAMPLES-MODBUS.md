Set

```shell
H="Content-Type: application/json"
URL=http://localhost:9009/

```

and use it this way

```shell
curl -H "$H" -d "D" -X POST $URL
```

Get the valve position:

```shell
D='{"Action": "MODBUS", "TaskName": "VS_SE3-get-valves-pos", "PostScript": "vs_se3.get-valves", "FunctionCode": "ReadHoldingRegisters","Address": 0, "Quantity": 9, "Host":"invalid"}'

## =>
## {"ToExchange":{"V6":{"Bool":false},
##                "V9":{"Bool":false},
##                ...
##                "registers":[1025,0,21760,0,1,0,1024,0,7]
##                ...}}
```

Read pressures from Modbus CDGs:

```shell
D='{"Action": "MODBUS", "TaskName": "Inficon_Modbus_CDG-read_out", "FunctionCode": "ReadInputRegisters","Address": 0, "Quantity": 68, "Host":"e75480"}'

## =>
## {"_x":[63,81,0,0,63,62,0,0,63,124,0,0,63,-86,0,1,
## 63,34,0,1,63,-127,0,1,63,-21,0,1,63,98,0,1,
## 63,-21,0,1,63,81,0,1,63,-89,0,1,63,-4,0,1,
## 63,115,0,1,63,30,0,1,63,-89,0,1,0,0,5,1,
## 0,0,0,0],"_t_start":"1609929639156","_t_stop":"1609929639158","_dt":2}
## -- ca. 100 Pa 
```

```shell
D='{"Action": "MODBUS", "TaskName": "VS_NEW_SE3-set-valve-pos", "FunctionCode": "writeSingleRegister","Address": 40003, "Host":"e75446", "PreScript":"vs_se3.set-valve", "PreInput": {"registers":[1029, 0, 4100, 0, 1300, 0, 21248, 0 ,83], "valve": "V1", "should": "open"}}'
```

```shell
D='{"Action": "MODBUS", "TaskName": "VS_SE3-get-valves-pos", "PostScript": "vs_se3.valves", "FunctionCode": "ReadHoldingRegisters","Address": 0, "Quantity": 9, "Host":"invalid"}'

## =>
## {"ToExchange":{"V6":{"Bool":false},
##                "V9":{"Bool":false},
##                ...
##                "registers":[1025,0,21760,0,1,0,1024,0,7]
##                ...}}
```

```shell
D='{"PostScript":"gn_se3.anybus-readout", "Action": "MODBUS", "TaskName": "Inficon_Modbus_CDG-read_out", "FunctionCode": "ReadInputRegisters","Address": 0, "Quantity": 64, "Host":"e75480", "Wait":100, "Repeat":3}'
```

```shell
D='{"PostScript":"gn_se3.anybus-readout", "PostScriptInput": {"Prefix": "", "Suffix": "-ind", "Unit": "Pa"}, "Action": "MODBUS", "TaskName": "Inficon_Modbus_CDG-read_out", "FunctionCode": "ReadInputRegisters","Address": 0, "Quantity": 64, "Host":"e75480", "Wait":100, "Repeat":3}'
```

```shell
D='{"PostScript":"gn_se3.anybus-add-ctrl", "PostScriptInput": {"TargetPressure":70, "TargetUnit": "Pa"}, "Action": "MODBUS", "TaskName": "Inficon_Modbus_CDG-read_out", "FunctionCode": "ReadInputRegisters","Address": 0, "Quantity": 64, "Host":"e75480", "Wait":100, "Repeat":3}'
```
 
