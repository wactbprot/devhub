# devhub
***

**devhub** is a web service that abstracts the protocols: [udp](#udp), [tcp](#tcp),
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
- [production endpoint](#production-endpoint)
- [special endpoints](#special-endpoints)
    - [stub [POST /stub]](#stub-post-stub)
    - [version](#version)
    - [echo](#echo)
    - [notes](#notes)
- [Next up](#next-up)

<!-- markdown-toc end -->


# Features

* Supported protocols:
    * `UDP`
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

* [UDP Examples](./EXAMPLES-UDP.md)
* [TCP Examples](./EXAMPLES-TCP.md)
* [VXI11 Examples](./EXAMPLES-VXI11.md)
* [EXECUTE Examples](./EXAMPLES-EXECUTE.md)
* [MODBUS Examples](./EXAMPLES-MODBUS.md)

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
curl -H "$H" -d '{"Wait": 100 , "Repeat":10}' -X POST http://localhost:9009/stub
```

Returns:

```json
{
  "req-id": "1654439253784",
  "Repeat": 10,
  "stub": true,
  "_x": [
    "MEAS 23.1 C",
    "2e-3 Pa",
    "DCR +1.98779E-4",
    "MEAS 23.1 C",
    "foo",
    "MEAS 23.1 C",
    "#<;;:_-",
    "#<;;:_-",
    "123",
    "foo"
  ],
  "Value": [
    "no-value"
  ],
  "_t_start": [
    "1654439253784",
    "1654439253885",
    "1654439253985",
    "1654439254086",
    "1654439254186",
    "1654439254287",
    "1654439254387",
    "1654439254488",
    "1654439254588",
    "1654439254688"
  ],
  "Wait": 100,
  "select": "missing",
  "_t_stop": [
    "1654439253784",
    "1654439253885",
    "1654439253985",
    "1654439254086",
    "1654439254186",
    "1654439254287",
    "1654439254387",
    "1654439254488",
    "1654439254588",
    "1654439254689"
  ]
}
```

after 10x100ms=1s.


```shell
D = '{"TaskName":"IM540-read_out", "Wait":1 , "Repeat":10}'
curl -H "$H" -d "$D" -X POST http://localhost:9009/stub
```

## version

Returns the current **devhub** version.

```shell
curl http://localhost:9009/version

## =>
## {"version":"0.6.375"}
```

whereby the last number gives the amount of commits on current branch.

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
