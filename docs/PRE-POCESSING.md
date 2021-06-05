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
