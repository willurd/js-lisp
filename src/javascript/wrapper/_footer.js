})(this);

// Set this library up to work with node.js
if (typeof(window) == "undefined" &&
	typeof(global) == "object" &&
	typeof(require) == "function" &&
	typeof(exports) == "object") {
	// FIXME: Find a better way to tell we're running in node.js
	for (var key in lisp) {
		if (lisp.hasOwnProperty(key)) {
			exports[key] = lisp[key];
		}
	}
	
	lisp.log = require("sys").puts;
}
