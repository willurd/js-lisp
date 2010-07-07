})((typeof(window) != "undefined") ? window : global); // For compatibility with node.js

// Set this library up to work with node.js
if ((typeof(window) == "undefined") &&
	(typeof(global) == "object") && global && // Make sure it isn't null
	(typeof(require) == "function") &&
	(typeof(exports) == "object") && exports) {
	
	var sys = require("sys"),
		fs  = require("fs"),
		path = require("path");
	
	lisp.log = sys.puts;
	
	lisp.load = function (filepath) {
		exports.eval(path.normalize(fs.readFileSync(filepath)));
	};
	
	// We are probably running in node.js now.
	// FIXME: Find a better way to tell we're running in node.js
	for (var key in lisp) {
		if (lisp.hasOwnProperty(key)) {
			exports[key] = lisp[key];
		}
	}
}
