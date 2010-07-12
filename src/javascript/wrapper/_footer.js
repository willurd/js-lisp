})((typeof(window) != "undefined") ? window : global); // For compatibility with node.js

// Set this library up to work with node.js
if ((typeof(window) == "undefined") &&
	(typeof(global) == "object") && global && // Make sure it isn't null
	(typeof(require) == "function") &&
	(typeof(exports) == "object") && exports) {
	// We are probably running in node.js now.
	// FIXME: Find a better way to tell we're running in node.js
	
	var sys = require("sys"),
		fs  = require("fs"),
		path = require("path");
	
	lisp.log = sys.puts;
	
	function FileNotFound (message) {
		this.toString = function () {
			return "FileNotFound: " + message;
		};
	}

	lisp.load = function (filepath, paths) {
		paths = paths || require.paths;
		paths.unshift(""); // A way to check filepath on it's own (with the
		                   // least amount of extra code).
		for (var i = 0; i < paths.length; i++) {
			var p = path.normalize(path.join(paths[i], filepath));
			var contents = null;
			try {
				contents = fs.readFileSync(p);
			} catch (e) {
				if (e instanceof FileNotFound) {
					throw e;
				}
			}
			if (contents != null) {
				return lisp.eval(contents);
			}
		}
		throw new FileNotFound("File '" + filepath + "' not found");
	};
	
	for (var key in lisp) {
		if (lisp.hasOwnProperty(key)) {
			exports[key] = lisp[key];
		}
	}
}
