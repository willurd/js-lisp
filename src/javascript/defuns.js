var DEFUNS = {
	"t": true,
	"true": true,
	"false": false,
	"nil": null,
	"null": null,
	"undefined": undefined,
	
	/**
	 * Performs a logical negation on the given value.
	 */
	"not": function (value) {
		if (arguments.length != 1) {
			throw new Error("(not) requires 1 argument");
		}
		return !value;
	},
	
	/**
	 * Returns the given arguments as a list.
	 */
	"list": function () {
		return argsToArray(arguments);
	},
	
	/**
	 * Creates a JavaScript object using the given arguments as an
	 * association list.
	 */
	"object": function () {
		var args = argsToArray(arguments);
		var object = {};
		
		if (args.length % 2 !== 0) {
			throw new Error("Invalid number of arguments to (object): " + args.length);
		}
		
		for (var i = 0; i < args.length; i += 2) {
			object[args[i]] = args[i+1];
		}
		
		return object;
	},
	
	/**
	 * Returns an instance of the given class, initialized with
	 * the rest of the given arguments.
	 */
	"new": function (Class) {
		if (arguments.length < 1) {
			throw new Error("(new) requires at least 1 argument");
		}
		var argnames = [];
		// This is the only way I could figure out how to make the
		// passing of arguments to new Class(...) dynamic.
		for (var i = 1; i < arguments.length; i++) {
			var argname = "arg" + i;
			eval("var " + argname + " = " + arguments[i]);
			argnames.push(argname);
		}
		return eval("new Class(" + argnames.join(",") + ")");
	},
	
	/**
	 * Returns a value from an object given a key (will work with
	 * array indices as well).
	 */
	"getkey": function (key, object) {
		if (arguments.length !== 2) {
			throw new Error("(getkey) requires 2 arguments (got " +
				arguments.length + ")");
		}
		return object[key];
	},
	
	/**
	 * Sets a value on the given object using the given key.
	 */
	"setkey": function (key, object, value) {
		if (arguments.length !== 3) {
			throw new Error("(setkey) requires 3 arguments (got " +
				arguments.length + ")");
		}
		return object[key] = value;
	},
	
	/**
	 * Prints the given arguments to the console.
	 */
	"puts": function () {
		// Do not remove this. This is not a debug statement.
		lisp.log.apply(null, arguments);
	},
	
	/**
	 * Joins the given arguments together into one string.
	 */
	"concat": function () {
		return argsToArray(arguments).join("");
	},
	
	/**
	 * Joins the given arguments together into one string, using
	 * the first argument as the separator.
	 */
	"join": function () {
		var args = argsToArray(arguments);
		var sep  = args[0];
		var list = args.slice(1).reduce(function (a, b) { return a.concat(b); });
		return list.join(sep);
	},
	
	/**
	 * Returns the type of the given value.
	 */
	"typeof": function (value) {
		if (arguments.length !== 1) {
			throw new Error("(typeof) requires 1 argument (got " +
				arguments.length + ")");
		}
		return typeof(value);
	},
	
	/**
	 * Converts the given value to a string.
	 */
	"to-string": function (value) {
		if (arguments.length !== 1) {
			throw new Error("(to-string) requires 1 argument (got " +
				arguments.length + ")");
		}
		return String(value);
	},
	
	/**
	 * Converts the given value to a number.
	 */
	"to-number": function (value) {
		if (arguments.length !== 1) {
			throw new Error("(to-number) requires 1 argument (got " +
				arguments.length + ")");
		}
		return Number(value);
	},
	
	/**
	 * Converts the given value to a number.
	 */
	"to-boolean": function (value) {
		if (arguments.length !== 1) {
			throw new Error("(to-boolean) requires 1 argument (got " +
				arguments.length + ")");
		}
		return Boolean(value);
	},
	
	/**
	 * Reduces the given arguments on the / operator.
	 */
	"/": function () {
		return argsToArray(arguments).reduce(function (a, b) {
			return a / b;
		});
	},
	
	/**
	 * Reduces the given arguments on the * operator.
	 */
	"*": function () {
		return argsToArray(arguments).reduce(function (a, b) {
			return a * b;
		});
	},
	
	/**
	 * Reduces the given arguments on the + operator.
	 */
	"+": function () {
		return argsToArray(arguments).reduce(function (a, b) {
			return a + b;
		});
	},
	
	/**
	 * Reduces the given arguments on the - operator.
	 */
	"-": function () {
		return argsToArray(arguments).reduce(function (a, b) {
			return a - b;
		});
	},
	
	/**
	 * Adds 1 to the given value.
	 */
	"1+": function (value) {
		if (arguments.length > 1) {
			throw new Error("(1+) requires 1 argument (got " +
				arguments.length + ")");
		}
		return Number(value) + 1;
	},
	
	/**
	 * Calls sprintf (found in the vendor section) with the
	 * supplied arguments.
	 */
	"format": function (print, format) {
		if (arguments.length < 2) {
			throw new Error("(format) expects at least 2 arguments (got " +
				arguments.length + ")");
		}
		if (typeof(format) != "string") {
			throw new Error("(format) expects a string format");
		}
		var output = sprintf.apply(null, argsToArray(arguments).slice(1));
		if (print) {
			lisp.log(output);
			return null;
		} else {
			return output;
		}
	},
	
	/**
	 * Converts the given string to uppercase.
	 */
	"to-upper": function (value) {
		if (arguments.length !== 1) {
			throw new Error("(to-upper) requires 1 argument (got " +
				arguments.length + ")");
		}
		if (typeof(value) != "string") {
			throw new Error("(to-upper) requires a string argument");
		}
		return value.toUpperCase();
	},
	
	/**
	 * Converts the given string to uppercase.
	 */
	"to-lower": function (value) {
		if (arguments.length !== 1) {
			throw new Error("(to-lower) requires 1 argument (got " +
				arguments.length + ")");
		}
		if (typeof(value) != "string") {
			throw new Error("(to-lower) requires a string argument");
		}
		return value.toLowerCase();
	}
};
