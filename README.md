# devhub

Now a stub later a hub.

## installation

### vxi11

```
git clone https://github.com/wactbprot/jvxi11.git
cd jvxi11/src/jvxi11
./makerpc
```
See `:resource-paths` and  `:java-source-paths` in `devhub`s `project.clj`. 

[org/epics/pvioc/pdrv/vxi11/package-tree](http://epics-pvdata.sourceforge.net/docbuild/pvIOCJava/2.0-BETA/documentation/html/org/epics/pvioc/pdrv/vxi11/package-tree.html)

## modbus

Maven repo:

[com.github.kochedykov/jlibmodbus "1.2.9.0"](https://mvnrepository.com/artifact/com.github.kochedykov/jlibmodbus/1.2.9.0)

## curl examples

```shell
export H="Content-Type: application/json"
```
## version

```shell
curl http://localhost:9009/version

## =>
## {"version":"0.2.5"}
```


## echo

```shell
curl -H "$H" -d '{"TaskName": "echo-test"}' -X POST http://localhost:9009/echo

## =>
## {"TaskName": "echo-test"}
```

## prod

### tcp

```shell
curl -H "$H" -d '{"TaskName": "tcp-test", "Action":"TCP", "Port":5025, "Host":"e75496", "Value":"frs()\n"}' -X POST http://localhost:9009/prod

## =>
## {"_x":"23.742259584,0.0018344406506,10,ch101\n","t_start":"1606812399642","t_stop":"1606812408754"}
```

## stub

### matching action

```shell
curl -d '{"Action":"MODBUS"}' -H "$H"  -X POST http://localhost:9009/stub 

## =>
## {"ToExchange":
## {"V1":
## {"Bool":1},
## "Vraw_block1":[1,0,1,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,1,0,1,0,0,0]},
## "t_start":"1587038086886",
## "t_stop":"1587038086916"}
```

### no taskname an action

```shell
curl -d '{"Missing":true}' -H "$H"  -X POST http://localhost:9009/stub

## =>
## {"error":"body don't contain a action, body don't contain a taskname"}
```

### no matches

```shell
curl -d '{"Action":"foo","TaskName":"bar"}' -H "$H"  -X POST http://localhost:9009/stub

## =>
##{"error":"no edn for action: foo, no edn for task name: xyz"}
```
