# documentation

what todo next:
* post proc.: legacy js, first class citizen clojure, later python
* same for pre proc.

* [API](./api)

## stub

**devhub** allows the configuration of predefined responses depending on the `POST`ed
`TaskName`. These responses are stored in the `resources/stub-response.edn` file.

The default configuration for the `stub` endpoint is:

```clojure
:stub {
     :mode :rand
     ;; ...
}
```


### curl examples

No `TaskName` means `:missing` is selected in `resources/stub-response.edn`.

```shell
curl -H "$H" -d '{"Wait":1 , "Repeat":10}' -X POST http://localhost:9009/stub
```


```shell
export H="Content-Type: application/json"
```
## version [POST /version]

### curl examples

```shell
curl http://localhost:9009/version

## =>
## {"version":"0.2.5"}
```

## echo [POST /echo]

### curl examples
```shell
curl -H "$H" -d '{"TaskName": "echo-test"}' -X POST http://localhost:9009/echo

## =>
## {"TaskName": "echo-test"}
```

## production [POST /]

### curl examples
#### tcp

```shell
curl -H "$H" -d '{"TaskName": "tcp-test", "Action":"TCP", "Port":5025, "Host":"e75496", "Value":"frs()\n"}' -X POST http://localhost:9009/

## =>
## {"_x":"23.742259584,0.0018344406506,10,ch101\n","t_start":"1606812399642","t_stop":"1606812408754"}
```


## installation

```shell
git clone git@github.com:wactbprot/devhub.git
cd devhub
lein deps
```

### tcp

The `TCP` action works out of the box.

### vxi11

```
git clone https://github.com/wactbprot/jvxi11.git
cd jvxi11/src/jvxi11
./makerpc
```
See `:resource-paths` and  `:java-source-paths` in `devhub`s `project.clj`. 

[org/epics/pvioc/pdrv/vxi11/package-tree](http://epics-pvdata.sourceforge.net/docbuild/pvIOCJava/2.0-BETA/documentation/html/org/epics/pvioc/pdrv/vxi11/package-tree.html)

### modbus

`modbus` works out of the box. The used library is:

[jlibmodbus](https://mvnrepository.com/artifact/com.github.kochedykov/jlibmodbus/1.2.9.0)


### javascript post processing (js-pp)

The js-pp of vacom gauges depend on crc module:

```shell
npm install crc 
```
