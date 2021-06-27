# devhub
***

**devhub** is a web service that abstracts the protocols: [tcp](#tcp),
[vxi](#vxi) and [modbus](#modbus) to read out measurement devices. It
[executes](#execute) shell commands.  Requests are *POST*ed via *http*
in json format. Measurement data is returned in json
format. **devhub** acts as a [stub](#stub-post-stub) and returns
predefined responses. The repository contains instructions to build a
standalone version that runs on BSD, Linux, MacOS and
Windows. **devhub** can be configured to use [elasticsearch
(els)](#elasticsearch-els) as a log database.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [devhub](#devhub)
- [Features](#features)
- [Data flow](#data-flow)
- [Code documentation](#code-documentation)
- [Install instructions](#install-instructions)
- [examples for production endpoint](#examples-for-production-endpoint)
- [special endpoints](#special-endpoints)
    - [stub [POST /stub]](#stub-post-stub)
    - [version](#version)
    - [echo](#echo)
    - [notes](#notes)
- [Next up](#next-up)

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

The [devhub documentation](https://wactbprot.github.io/devhub/) is
build on `push` events with [github actions](https://docs.github.com/en/actions).

# Install instructions

See [install instructions](./INSTALL.md).


# production endpoint

* [TCP Examples](./EXAMPLE-TCP.md)
* [VXI11 Examples](./EXAMPLE-VXI11.md)
* [EXECUTE Examples](./EXAMPLE-EXECUTE.md)
* [MODBUS Examples](./EXAMPLE-MODBUS.md)

# special endpoints

For convenience set:

```shell
H="Content-Type: application/json"
URL=http://localhost:9009/
```

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
Examples for stub endpoint are:

No `TaskName` means `:missing` is selected in `resources/stub-response.edn`.

```shell
curl -H "$H" -d '{"Wait":1 , "Repeat":10}' -X POST http://localhost:9009/stub
```

```shell
D = '{"TaskName":"IM540-read_out", "Wait":1 , "Repeat":10}'
curl -H "$H" -d "$D" -X POST http://localhost:9009/stub
```

## version

Returns the current **devhub** version.

```shell
curl http://localhost:9009/version

## =>
## {"version":"0.2.5"}
```

## echo

You get your request echoed.

```shell
curl -H "$H" -d '{"TaskName": "echo-test"}' -X POST http://localhost:9009/echo

## =>
## {"TaskName": "echo-test"}
```
## notes

* `[clojure-interop/java.nio "1.0.5"]`
* https://cljdoc.org/d/clojure-interop/java.nio/1.0.5
*  overcome `SSL peer shut down incorrectly` error by:
```shell
export JAVA_TOOL_OPTIONS=-Dhttps.protocols=TLSv1,TLSv1.1,TLSv1.2
```


# Next up

* turn to polylith arch
* generate aliases for tcp, vxi and combinations only
* vxi libs when uberjar
* Update docs
