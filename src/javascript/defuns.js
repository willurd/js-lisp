/**
 * Returns an instance of the given class, initialized with
 * the rest of the given arguments.
 */
defun("new", function (Class) {
	if (arguments.length === 0) {
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
});

/**
 * Returns the given arguments as a list.
 */
defun("list", function () {
	return argsToArray(arguments);
});

/**
 * Creates a JavaScript object using the given arguments as a
 * property list.
 */
defun("object", function () {
	var args = argsToArray(arguments);
	var object = {};
	
	if (args.length % 2 !== 0) {
		throw new Error("Invalid number of arguments to (object): " + args.length);
	}
	
	for (var i = 0; i < args.length; i += 2) {
		object[args[i]] = args[i+1];
	}
	
	return object;
});

/**
 * Returns the given arguments as a list.
 */
defun("array", function () {
	return argsToArray(arguments);
});

/**
 * Returns a value from an object given a key (will work with
 * array indices as well).
 */
defun("getkey", function (key, object) {
	if (arguments.length !== 2) {
		throw new Error("(getkey) requires 2 arguments (got " +
			arguments.length + ")");
	}
	return object[key];
});

/**
 * Sets a value on the given object using the given key.
 */
defun("setkey", function (key, object, value) {
	if (arguments.length !== 3) {
		throw new Error("(setkey) requires 3 arguments (got " +
			arguments.length + ")");
	}
	return object[key] = value;
});

/**
 * Prints the given arguments to the console.
 */
defun("print", function () {
	// Do not remove this. This is not a debug statement.
	lisp.log.apply(null, arguments);
});

/**
 * Joins the given arguments together into one string.
 * 
 * @concat
 */
defun("concat", function () {
	return argsToArray(arguments).join("");
});

/**
 * Joins the given arguments together into one string, using
 * the first argument as the separator.
 * 
 * @tested
 * 
 * @example
 *   > (join ", " (list "one" "two"))
 *   "one, two"
 * @example
 *   > (let ((l1 (list "one" "two"))
 *           (l2 (list "three")))
 *       (join ", " l1 l2))
 *   "one, two, three"
 */
defun("join", function () {
	if (arguments.length === 0) {
		throw new Error("(join) requires at least 1 argument");
	}
	var args = argsToArray(arguments);
	var sep  = args[0];
	var rest = args.slice(1);
	for (var i = 0; i < rest.length; i++) {
		var arg = rest[i];
		if (!(arg instanceof Array)) {
			throw new Error("(join) got an invalid argument: '" +
			    String(arg) + "' is not a list");
		}
	}
	var list = rest.reduce(function (a, b) { return a.concat(b); });
	return list.join(sep);
});

/**
 * Returns the type of the given value.
 * 
 * @tested
 */
defun("typeof", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(typeof) requires 1 argument (got " +
			arguments.length + ")");
	}
	return typeof(value);
});

/**
 * Converts the given value to a string.
 * 
 * @tested
 */
defun("to-string", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(to-string) requires 1 argument (got " +
			arguments.length + ")");
	}
	return String(value);
});

/**
 * Converts the given value to a number.
 * 
 * @tested
 */
defun("to-number", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(to-number) requires 1 argument (got " +
			arguments.length + ")");
	}
	return Number(value);
});

/**
 * Converts the given value to a number.
 * 
 * @tested
 */
defun("to-boolean", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(to-boolean) requires 1 argument (got " +
			arguments.length + ")");
	}
	return Boolean(value);
});

/**
 * Converts the given string to uppercase.
 * 
 * @tested
 */
defun("to-upper", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(to-upper) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (typeof(value) != "string") {
		throw new Error("(to-upper) requires a string argument");
	}
	return value.toUpperCase();
});

/**
 * Converts the given string to uppercase.
 * 
 * @tested
 */
defun("to-lower", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(to-lower) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (typeof(value) != "string") {
		throw new Error("(to-lower) requires a string argument");
	}
	return value.toLowerCase();
});

/**
 * Reduces the given arguments on the / operator.
 * 
 * @tested
 */
defun("/", function () {
	if (arguments.length === 0) {
		throw new Error("(/) requires at least 1 argument");
	}
	var args = argsToArray(arguments);
	// This is to emulate common lisp, where dividing one number
	// is the same as dividing 1 by that number.
	if (args.length === 1) {
		args = [1].concat(args);
	}
	return args.reduce(function (a, b) {
		return a / b;
	});
});

/**
 * Reduces the given arguments on the * operator.
 * 
 * @tested
 */
defun("*", function () {
	var args = argsToArray(arguments);
	if (args.length === 0) {
		args = [1];
	}
	return args.reduce(function (a, b) {
		return a * b;
	});
});

/**
 * Reduces the given arguments on the + operator.
 * 
 * @tested
 */
defun("+", function () {
	var args = argsToArray(arguments);
	if (args.length === 0) {
		args = [0];
	}
	return args.reduce(function (a, b) {
		return a + b;
	});
});

/**
 * Reduces the given arguments on the - operator.
 * 
 * @tested
 */
defun("-", function () {
	if (arguments.length === 0) {
		throw new Error("(-) requires at least 1 argument");
	}
	var args = argsToArray(arguments);
	if (args.length === 1) {
		args = [0].concat(args);
	}
	return args.reduce(function (a, b) {
		return a - b;
	});
});

/**
 * Reduces the given arguments on the % operator.
 * 
 * @tested
 */
defun("%", function () {
	if (arguments.length !== 2) {
		throw new Error("(%) requires 2 arguments (got " +
			arguments.length + ")");
	}
	return argsToArray(arguments).reduce(function (a, b) {
		return a % b;
	});
});

/**
 * Adds 1 to the given value.
 * 
 * @tested
 */
defun("1+", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(1+) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (typeof(value) != "number") {
		throw new Error("(1+) requires a number argument (got " +
			value);
	}
	return Number(value) + 1;
});

/**
 * Subtracts 1 from the given value.
 * 
 * @tested
 */
defun("1-", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(1-) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (typeof(value) != "number") {
		throw new Error("(1-) requires a number argument (got " +
			value);
	}
	return Number(value) - 1;
});

/**
 * Calls sprintf (found in the vendor section) with the
 * supplied arguments.
 * 
 * @tested
 */
defun("format", function (print, format) {
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
});
