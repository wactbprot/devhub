# UDP examples

Set

```shell
H="Content-Type: application/json"
URL=http://localhost:9009/
```

and use it this way

```shell
curl -H "$H" -d "$D" -X POST $URL
```

## 
```shell
D='{"Action":"UDP","Host":"_____","Port": _____,"Value":"Sw_off1<usr><pwd>"}'
```
