# TCP examples

Set

```shell
H="Content-Type: application/json"
URL=http://localhost:9009/

```

and use it this way

```shell
curl -H "$H" -d "$D" -X POST $URL --noproxy "*"
```

## Misc Examples

Switch off beep of Keithley 2700 via Prologix:

```shell
D='{"Action":"TCP", "Host":"e75456", "Port":"1234", "Value":"++addr 10\r++eot_enable 1\r++auto 1\r++eot_char 10\rSYSTem:BEEPer 0\r"}'
```

```shell
D='{"TaskName": "tcp-test", "Action":"TCP", "Port":5025, "Host":"e75496", "Value":"frs()\n"}'
```

```shell
D='{"TaskName": "tcp-test", "Action":"TCP", "Port":5025, "Host":"e75494", "Value":"ch1()\n"}'
```

```shell
 D='{"TaskName": "ind_low_range", "Action": "TCP","Repeat": 3,"Wait": "10000","Host": "e75421","Port": 5302,"Value": "val\r","PostProcessing": ["var _vec=_x.map(_.extractSRG3),","_res = _.vlStat(_.checkNumArr(_vec).Arr),","Result=[_.vlRes(\"ind\",_res.mv,\"DCR\", \"\", _res.sd, _res.N)];"]}'
```

Returns `error` on invalid host: 

```shell
D='{"Action":"TCP", "Host": "invalid", "Value":"IDN?", "Port":20}'
```

```shell
D='{"TaskName":"IM540-read_out", "Wait":1000 , "Repeat":10, "PostScript": "im540.pressure-rise", "PostScriptInput": {"Type": "rise"}, "Action":"TCP", "Host":"e75436", "Port":5303, "Value":"MES R\r"}'
```

```shell
D='{"TaskName":"ServoTest", "Wait":1000 , "Repeat":1, "PostScript": "servo-se3.meas-velo", "PostScriptInput": {"Motor": "2", "MinVelo:"5"}, "Action":"TCP", "Host":"e75443", "Port":5300, "Value":"2GN\r"}'
```

## CE3

```shell
D='{"Action":"TCP","Host":"192.168.98.134","Port":"9009",  "Value":"init\n"}'

D='{"Action":"TCP","Host":"192.168.98.134","Port":"9009",  "Value":"holdhours=112\n"}'

D='{"Action":"TCP","Host":"192.168.98.134","Port":"9009",  "Value":"run\n"}'
```

## FRS5 read out

```shell
D='{"Port": 5301, "Host": "e75436", "Value":"\u001BP", "Wait": 100, "Repeat": 20}'
```

## MKT50 (7Lines)

`NL` ... Number of lines to read

```shell
D='{"Action":"TCP", "Port":5300, "Host":"e75421", "NL":7, "Value":"GET DATA\r"}'
```
## Inficon bin prot (e.g. BCD450)

`NB` ... Number of bytes to read

```shell
D='{"Action":"TCP", "Port":5300, "Host":"e75421", "NB":20, "Value":"", "PostScript": "inf-bin.readout", "Repeat" :10, "Wait": 100}'
```

## TODO: EOT Example

`EOT` ... Termination sign (integer)
