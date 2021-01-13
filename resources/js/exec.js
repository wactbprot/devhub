/**
 * Executes legacy `PostProcessing` scripts.
 *
 * @author wactbprot
 */
const fs = require("fs");
const vm = require("vm");

const js_path = process.argv.slice(2)[0];
const exec_script = process.argv.slice(2)[1];
const data = process.argv.slice(2)[2];

var ctx = {"_" : {}};

try {
    fs.readFile(data, 'utf8', (err, data) => {
	if (err) {
	    console.log({"error": err.toString()});
	} else {
	    const d = JSON.parse(data);
	    ctx["_x"]       = d["_x"];
	    ctx["_t_start"] = d["_t_start"];
	    ctx["_t_stop"]  = d["_t_stop"];
	}});
} catch(err) {
    console.log(JSON.stringify({"error": err.toString()}));
}

fs.readdirSync(js_path ? js_path : ".").forEach(file => {
    if(file.startsWith('add')) {
	var ms = require("./" + file);
	for (m in ms) {
	    ctx._[m] = ms[m];
	}
    }	
});

fs.readFile(exec_script, 'utf8', (err, s) => {
    if (err) {
	console.log({"error": err.toString()});
    } else {
	try {
	    vs = vm.createScript(s);
	    vs.runInNewContext(ctx);
	    var ret = {};
	    for (v in ctx) {
		if(!v.startsWith("_")) {
		    ret[v] = ctx[v];
		}
	    }
	    console.log(JSON.stringify(ret))
	} catch(err) {
	    console.log(JSON.stringify({"error": err.toString()}));
	}
    }
});
