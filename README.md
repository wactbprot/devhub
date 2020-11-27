# devhub

Now a stub later a hub.


## curl examples

## version

```shell
curl http://localhost:9009/version
## {"version":"0.2.5"}
´´´


### matching action

```shell
curl -d '{"Action":"MODBUS"}' -H "Content-Type: application/json"  -X POST http://localhost:8008 -i
## HTTP/1.1 200 OK
## Date: Thu, 16 Apr 2020 11:54:46 GMT
## Content-Type: application/json;charset=utf-8
## Content-Length: 147
## Server: Jetty(9.4.12.v20180830)
##
## {"ToExchange":
## {"V1":
## {"Bool":1},
## "Vraw_block1":[1,0,1,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,1,0,1,0,0,0]},
## "t_start":"1587038086886",
## "t_stop":"1587038086916"}
```


### no taskname an action

```shell
curl -d '{"Missing":true}' -H "Content-Type: application/json"  -X POST http://localhost:8008 -i
## HTTP/1.1 404 Not Found
## Date: Thu, 16 Apr 2020 12:07:21 GMT
## Content-Type: application/json;charset=utf-8
## Content-Length: 70
## Server: Jetty(9.4.12.v20180830)
##
## {"error":"body don't contain a action, body don't contain a taskname"}
```

### no matches

```shell
curl -i -d '{"Action":"foo","TaskName":"bar"}' -H "Content-Type: application/json"  -X POST http://localhost:8008
#
## HTTP/1.1 404 Not Found
## Date: Thu, 16 Apr 2020 11:32:35 GMT
## Content-Type: application/json;charset=utf-8
## Content-Length: 61
## Server: Jetty(9.4.12.v20180830)
##
##{"error":"no edn for action: foo, no edn for task name: xyz"}
```
