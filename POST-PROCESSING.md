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

TODO: Example needed
