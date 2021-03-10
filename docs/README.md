**devhub** is a web service that abstracts the protocols: [tcp](#tcp),
[vxi](#vxi) and [modbus](#modbus) to read out measurement devices. It
also [executes](#execute) shell commands.  Requests are *POST*ed via
*http* in json format. Measurement data is returned in json
format. **devhub** may act as a [stub](#stub-post-stub) and returns
predefined responses. The repository contains a pre-compiled
standalone version that runs on BSD, Linux, MacOS and
Windows. **devhub** may be used with [elasticsearch (els)](#elasticsearch-els)
as a log database.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Features](#features)
- [Data flow](#data-flow)
- [Code documentation](#code-documentation)
- [examples for the production endpoint [POST /]](#examples-for-the-production-endpoint-post-)
- [special endpoints](#special-endpoints)
    - [stub [POST /stub]](#stub-post-stub)
    - [version [POST /version]](#version-post-version)
    - [echo [POST /echo]](#echo-post-echo)
- [pre processing](#pre-processing)
    - [:PreScript](#prescript)
    - [:PreProcessing](#preprocessing)
    - [:PreScriptPy](#prescriptpy)
- [post processing](#post-processing)
    - [:PostScript](#postscript)
    - [:PostProcessing](#postprocessing)
    - [:PostScriptPy](#postscriptpy)
- [Installation](#installation)
    - [Standalone version](#standalone-version)
    - [Development version](#development-version)
    - [tcp](#tcp)
    - [vxi11](#vxi11)
    - [modbus](#modbus)
    - [javascript post processing (js-pp)](#javascript-post-processing-js-pp)
- [µlog](#µlog)
    - [notes](#notes)

<!-- markdown-toc end -->


# Features

* Supported protocols:
    * `TCP`
    * `VXI11`
    * `MODBUS`
        * `:ReadHoldingRegisters` 
        * `:ReadInputRegisters`
        * `:ReadCoils`
        * `:ReadDiscreteInputs`
        * `:writeSingleRegister` 
    * `EXECUTE`
* endpoints:
    * `/` (production)
    * `/echo`
    * `/version`
    * `/stub` 
        * `first`
        * `last`
        * `rand`
* `:Value: "IDN?"` or `:Value: ["PR1?" "<ENQ>"]` 
* pre-processing
    * `javascript`
    * `python3`
    * `clojure`
* post-processing
    * `javascript`
    * `python3`
    * `clojure`
* [Searchable logs](#µlog) (elasticsearch, [kibana](#kibana)) 
* linux, windows and macOS support

# Data flow

![request-response](./req-res.svg)

# Code documentation

* [API](./api)
* [coverage](./coverage)


# examples for the production endpoint [POST /]

* [TCP Examples](./EXAMPLE-TCP.md)
* [VXI11 Examples](./EXAMPLE-VXI11.md)
* [EXECUTE Examples](./EXAMPLE-EXECUTE.md)
* [MODBUS Examples](./EXAMPLE-MODBUS.md)

# special endpoints

## stub [POST /stub]

**devhub** allows the configuration of predefined responses depending on the `POST`ed
`TaskName`. These responses are stored in the `resources/stub-response.edn` file.

The default configuration for the `stub` endpoint is:

```clojure
:stub {
     :mode :rand
     ;; ...
}
```

No `TaskName` means `:missing` is selected in `resources/stub-response.edn`.

```shell
curl -H "$H" -d '{"Wait":1 , "Repeat":10}' -X POST http://localhost:9009/stub
```

```shell
D = '{"TaskName":"IM540-read_out", "Wait":1 , "Repeat":10}'
curl -H "$H" -d "$D" -X POST http://localhost:9009/stub
```

## version [POST /version]

Returns the current **devhub** version.

```shell
curl http://localhost:9009/version

## =>
## {"version":"0.2.5"}
```

## echo [POST /echo]

You get your request echoed. 

```shell
curl -H "$H" -d '{"TaskName": "echo-test"}' -X POST http://localhost:9009/echo

## =>
## {"TaskName": "echo-test"}
```

# pre processing

The pre-processing of the `task` `POST`ed to the **devhub** server
can be managed with the following techniques:

* `:PreScript`: `clojure` functions placed in the
    `src/devhub/pp_scripts` folder. Function signature is `(fn-name
    task)`. Should return the task as a `map`.
* `:PreProcessing`: `javascript` code given as an array of source
    lines. 
* `:PreScriptPy`: `python` scripts placed in the `resources/py`
    folder.  The scripts receive the json encoded `task` as 2nd 
    
Use the key `:PreInput` to provide data structures to work on.

## :PreScript

## :PreProcessing

## :PreScriptPy

```shell
D='{"Action":"EXECUTE","Cmd":"ls","PostScriptPy":"ls-demo"}'
curl -H "$H" -d "D" -X POST http://localhost:9009/
## =>
## {"ToExchange":{"FilesVector":["CHANGELOG.md",
##                               "doc",
##                               "docs",
##                               "LICENSE",
##                               "pre-commit.sh",
##                               "project.clj",
##                               "README.md",
##                               "resources",
##                               "src",
##                               "target",
##                               "test",""]}}
```

# post processing

The post-processing of the `data` returned by a *devices*  or by the  *stub* interface
can be managed with the help of

* `:PostScript`: `clojure` functions placed in the `src/devhub/pp_scripts` 
    folder. Function signature is `(fn-name task data)`. Should return a `map`
* `:PostProcessing`: `javascript` code given as an array of source
    lines. The strings `_x`, `_t_start` and `_t_stop` are replaced on
    string level. The resulting string is evaluated. Should return valid json.
* `:PostScriptPy`: `python` scripts placed in the `resources/py`
    folder.  The scripts receive the json encoded `task` as 2nd and
    the json encoded `data` as 3rd argument. Should return valid json.

## :PostScript

See [MODBUS Examples](./EXAMPLE-MODBUS.md).

## :PostProcessing

Note: The **non json standard** key encoding with a single quote is not supported. So:

```json
PostProcessing:["{'A':100}"]
```

will throw a exception. Use the valid:

```json
PostProcessing:["{\"A\":100}"]
```

or switch to first class [:PostScript](#postscript).

## :PostScriptPy

# Installation

## Standalone version

A Standalone version of **devhub** is generated with:

```shell
git clone git@github.com:wactbprot/devhub.git
cd devhub
lein uberjar

## =>
## Compiling 37 source files to /home/wact/clojure/devhub/target/uberjar/classes
## Compiling devhub.execute
## Compiling devhub.js-pp
## Compiling devhub.modbus
## Compiling devhub.pp-scripts.core
## Compiling devhub.pp-scripts.utils
## Compiling devhub.pp-scripts.vs_se3
## Compiling devhub.py-pp
## Compiling devhub.safe
## Compiling devhub.server
## Compiling devhub.stub
## Compiling devhub.tcp
## Compiling devhub.utils
## Compiling devhub.vxi11
## Created /home/wact/clojure/devhub/target/uberjar/devhub-x.y.z.jar
## Created /home/wact/clojure/devhub/target/uberjar/devhub-x.y.z-standalone.jar
```

Distribute `devhub-x.y.z-standalone.jar` and run with:

```shell
java -jar devhub-0.8.0-standalone.jar
## =>
##                    __                           
##                    \ \                          
##                     \ \                         
##                      > \                        
##                     / ^ \                       
##                    /_/ \_\                      
##      _                  _               _       
##   __| |   ___  __   __ | |__    _   _  | |__    
##  / _` |  / _ \ \ \ / / | '_ \  | | | | | '_ \   
## | (_| | |  __/  \ V /  | | | | | |_| | | |_) |  
##  \__,_|  \___|   \_/   |_| |_|  \__,_| |_.__/   
```


## Development version

```shell
git clone git@github.com:wactbprot/devhub.git
cd devhub
lein deps
```

## tcp

The `TCP` action works out of the box.

## vxi11

```
git clone https://github.com/wactbprot/jvxi11.git
cd jvxi11/src/jvxi11
./makerpc
```
See `:resource-paths` and  `:java-source-paths` in `devhub`s `project.clj`. 

[org/epics/pvioc/pdrv/vxi11/package-tree](http://epics-pvdata.sourceforge.net/docbuild/pvIOCJava/2.0-BETA/documentation/html/org/epics/pvioc/pdrv/vxi11/package-tree.html)

## modbus

`modbus` works out of the box. The used library is:

[jlibmodbus](https://mvnrepository.com/artifact/com.github.kochedykov/jlibmodbus/1.2.9.0)


## javascript post processing (js-pp)

The js-pp of vacom gauges depend on crc module:

```shell
npm install crc 
```

# µlog

* https://github.com/BrunoBonacci/mulog
* configuration:

```clojure
 :mulog {:type :multi
         :publishers[ 
                     ;; send events to the stdout
                     ;; {:type :console
                     ;;  :pretty? true}
                     ;; send events to a file
                     ;; {:type :simple-file
                     ;;  :filename "/tmp/mulog/events.log"}
                     ;; send events to ELS
                     {:type :elasticsearch
                      :url  "http://localhost:9200/"
                      :els-version  :v7.x
                      :publish-delay 1000
                      :data-stream  "vl-log-stream"
                      :name-mangling false
                      }]}
```

* see [vl-log-stream](https://github.com/wactbprot/vl-log-stream)

## notes

* `[clojure-interop/java.nio "1.0.5"]`
* https://cljdoc.org/d/clojure-interop/java.nio/1.0.5

