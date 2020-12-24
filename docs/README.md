<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [documentation](#documentation)
    - [stub [POST /stub]](#stub-post-stub)
        - [curl examples](#curl-examples)
    - [version [POST /version]](#version-post-version)
        - [curl examples](#curl-examples-1)
    - [echo [POST /echo]](#echo-post-echo)
        - [curl examples](#curl-examples-2)
    - [production [POST /]](#production-post-)
        - [tcp](#tcp)
            - [curl examples](#curl-examples-3)
        - [EXECUTE](#execute)
            - [curl examples](#curl-examples-4)
        - [vxi](#vxi)
            - [curl examples](#curl-examples-5)
        - [modbus](#modbus)
            - [curl examples](#curl-examples-6)
- [installation](#installation)
    - [tcp](#tcp-1)
    - [vxi11](#vxi11)
    - [modbus](#modbus-1)
    - [javascript post processing (js-pp)](#javascript-post-processing-js-pp)
- [µlog](#µlog)
    - [kibana](#kibana)
    - [mapping](#mapping)
    - [notes](#notes)

<!-- markdown-toc end -->

# documentation


* [API](./api)


For the examples in the following sections, the following *environment
variable* is useful:

```shell
export H="Content-Type: application/json"
```
It is used as follows:

```shell
curl -H "$H" ...
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

### curl examples

No `TaskName` means `:missing` is selected in `resources/stub-response.edn`.

```shell
curl -H "$H" -d '{"Wait":1 , "Repeat":10}' -X POST http://localhost:9009/stub
```

## version [POST /version]

Returns the current **devhub** version.

### curl examples

```shell
curl http://localhost:9009/version

## =>
## {"version":"0.2.5"}
```

## echo [POST /echo]

You get your request echoed. 

### curl examples

```shell
curl -H "$H" -d '{"TaskName": "echo-test"}' -X POST http://localhost:9009/echo

## =>
## {"TaskName": "echo-test"}
```

## production [POST /]

### tcp

#### curl examples
```shell
curl -H "$H" -d '{"TaskName": "tcp-test", "Action":"TCP", "Port":5025, "Host":"e75496", "Value":"frs()\n"}' -X POST http://localhost:9009/

## =>
## {"_x":"23.742259584,0.0018344406506,10,ch101\n","t_start":"1606812399642","t_stop":"1606812408754"}
```
### EXECUTE

#### curl examples
```shell
curl -H "$H" -d '{"Action":"EXECUTE","Cmd":"ls", "Wait":100 , "Repeat":2}' -X POST http://localhost:9009/

## =>
## {
##    "_x":[
##       {
##          "exit":0,
##          "out":"CHANGELOG.md\ndoc\ndocs\nLICENSE\npre-commit.sh\nproject.clj\nREADME.md\nresources\nsrc\ntarget\ntest\n",
##          "err":""
##       },
##       {
##          "exit":0,
##          "out":"CHANGELOG.md\ndoc\ndocs\nLICENSE\npre-commit.sh\nproject.clj\nREADME.md\nresources\nsrc\ntarget\ntest\n",
##          "err":""
##       }
##    ],
##    "_t_start":[
##       "1608482700761",
##       "1608482700871"
##    ],
##    "_t_stop":[
##       "1608482700771",
##       "1608482700875"
##    ],
##    "_dt":[
##       10,
##       4
##    ]
## }
```

### vxi

#### curl examples

### modbus

#### curl examples


# installation

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

## kibana

* http://localhost:5601/app/discover
* mapping: stack management > dev tools
* index pattern: stack management > kibana > index pattern
* search: discover

## mapping

```PUT /devhub-stream```
```json
{
  "mappings": {
    "properties": {
        "@timestamp": {
          "type": "date"
        },
        "PreProcessing": {"type": "text"},
        "PreScript": {"type": "text"},
        "PreScriptPy": {"type": "text"},
        "raw-result-str":{"type": "text"},
        "Action": {
          "type": "text",
          "fields": {
            "keyword": {
              "type": "keyword",
              "ignore_above": 256
            }
          }
        },
        "app-name": {
          "type": "text",
          "fields": {
            "keyword": {
              "type": "keyword",
              "ignore_above": 256
            }
          }
        },
        "env": {
          "type": "text",
          "fields": {
            "keyword": {
              "type": "keyword",
              "ignore_above": 256
            }
          }
        },
        "mulog/event-name": {
          "type": "text",
          "fields": {
            "keyword": {
              "type": "keyword",
              "ignore_above": 256
            }
          }
        },
        "mulog/namespace": {
          "type": "text",
          "fields": {
            "keyword": {
              "type": "keyword",
              "ignore_above": 256
            }
          }
        },
        "mulog/trace-id": {
          "type": "text",
          "fields": {
            "keyword": {
              "type": "keyword",
              "ignore_above": 256
            }
          }
        },
        "req-id": {
          "type": "text",
          "fields": {
            "keyword": {
              "type": "keyword",
              "ignore_above": 256
            }
          }
        },
        "stub": {
          "type": "boolean"
        },
        "version": {
          "type": "text",
          "fields": {
            "keyword": {
              "type": "keyword",
              "ignore_above": 256
            }
          }
        }
      }
    }
}
```

## notes

* `[clojure-interop/java.nio "1.0.5"]`

