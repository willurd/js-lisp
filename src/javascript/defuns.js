/**
 * Functions that are defined for the lisp environment.
 * 
 * @namespace
 */
lisp.functions = {};
delete lisp.functions; // Delete it because it's just for documentation

/**
 * Returns an instance of the given class, initialized with
 * the rest of the given arguments.
 * 
 * @function
 * @name new
 * @memberOf lisp.functions
 * 
 * @param {Class} Class
 *     The class to create a new instance of.
 * @param {[mixed]} rest
 *     The arguments to be passed to the class constructor.
 * 
 * @returns The new class instance.
 * 
 * @example Instantiate a class
 *     >> (new MyClass)
 *     => ; the MyClass instance
 *
 * @example Instantiate a class with constructor arguments
 *     >> (new Error "My error message")
 *     => ; the Error instance
 */
defun("new", function (Class /*, ... */) {
	if (arguments.length === 0) {
		throw new Error("(new) requires at least 1 argument");
	}
	var args = argsToArray(arguments).slice(1);
	var argnames = args.map(function (item, i, thisObject) { return "args[" + i + "]"; });
	return eval("new Class(" + argnames.join(",") + ")");
});

/**
 * Throws the given object, or new Error() if no object is
 * provided.
 * 
 * @function
 * @name throw
 * @memberOf lisp.functions
 * 
 * @param {mixed} object The object to throw. Defaults to new Error().
 * 
 * @returns Nothing. After throw'ing the stack is unwided to the
 *          nearest 'catch' block.
 * 
 * @tested
 * 
 * @example Basic Error
 *   >> (throw) ; Throws "new Error()"
 * 
 * @example Custom Error
 *   >> (throw (new Error "My Custom Error"))
 * 
 * @example Anything else you can normally throw
 *   >> (throw "a string")
 *   >> (throw 12)
 *   >> (throw (object :type "MyError"))
 *   >> (throw (array 1 2 3))
 */
defun("throw", function (object) {
	if (arguments.length > 1) {
		throw new Error("(throw) accepts 1 optional argument (got " +
			arguments.length + ")");
	}
	object = object || new Error(); // Throw "new Error()" if nothing is provided
	throw object;
});

/**
 * Returns the given arguments as a list.
 * 
 * @function
 * @name list
 * @memberOf lisp.functions
 */
defun("list", function (/* ... */) {
	return argsToArray(arguments);
});

/**
 * Creates a JavaScript object using the given arguments as a
 * property list to initialize the object. There must be an even
 * number of arguments -- one value for every key.
 * 
 * @function
 * @name object
 * @memberOf lisp.functions
 * 
 * @returns The new object.
 * 
 * @tested
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
 * Creates an array from the given arguments.
 * 
 * @function
 * @name array
 * @memberOf lisp.functions
 * 
 * @returns The new array.
 */
defun("array", function () {
	return argsToArray(arguments);
});

/**
 * Returns a value from an object given a key (will work with
 * array indices as well).
 * 
 * @function
 * @name getkey
 * @memberOf lisp.functions
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
 * 
 * @function
 * @name setkey
 * @memberOf lisp.functions
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
 * 
 * @function
 * @name print
 * @memberOf lisp.functions
 * 
 * @returns nil.
 * 
 * @tested
 */
defun("print", function () {
	// Do not remove this. This is not a debug statement.
	lisp.log.apply(null, arguments);
	return null;
});

/**
 * Joins the given arguments together into one string.
 * 
 * @function
 * @name concat
 * @memberOf lisp.functions
 * 
 * @returns The string result of the joined arguments.
 * 
 * @tested
 */
defun("concat", function () {
	return argsToArray(arguments).join("");
});

/**
 * Joins the given arguments together into one string, using
 * the first argument as the separator.
 * 
 * @function
 * @name join
 * @memberOf lisp.functions
 * 
 * @returns The string result of the joined arguments.
 * 
 * @tested
 * 
 * @example Join items from a single list
 *   >> (join ", " (list "one" "two"))
 *   => "one, two"
 * 
 * @example Join items from multiple lists
 *   >> (let ((l1 (list "one" "two"))
 *            (l2 (list "three")))
 *        (join ", " l1 l2))
 *   => "one, two, three"
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
 * @function
 * @name typeof
 * @memberOf lisp.functions
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
 * @function
 * @name to-string
 * @memberOf lisp.functions
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
 * @function
 * @name to-number
 * @memberOf lisp.functions
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
 * @function
 * @name to-boolean
 * @memberOf lisp.functions
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
 * Converts the given value to a json representation of that value.
 * 
 * @function
 * @name to-json
 * @memberOf lisp.functions
 */
defun("to-json", function (object) {
	if (arguments.length !== 1) {
		throw new Error("(to-json) requires 1 argument (got " +
			arguments.length + ")");
	}
	return toJSON(object);
});

/**
 * Converts the given string to uppercase.
 * 
 * @function
 * @name to-upper
 * @memberOf lisp.functions
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
 * @function
 * @name to-lower
 * @memberOf lisp.functions
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
 * @function
 * @name /
 * @memberOf lisp.functions
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
 * @function
 * @name *
 * @memberOf lisp.functions
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
 * @function
 * @name +
 * @memberOf lisp.functions
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
 * @function
 * @name -
 * @memberOf lisp.functions
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
 * @function
 * @name %
 * @memberOf lisp.functions
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
 * @function
 * @name 1+
 * @memberOf lisp.functions
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
			value + ")");
	}
	return Number(value) + 1;
});

/**
 * Subtracts 1 from the given value.
 * 
 * @function
 * @name 1-
 * @memberOf lisp.functions
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
			value + ")");
	}
	return Number(value) - 1;
});

/**
 * Calls {@link sprintf} (found in the vendor section) with the
 * supplied arguments.
 * 
 * @function
 * @name format
 * @memberOf lisp.functions
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
