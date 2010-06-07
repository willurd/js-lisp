/**
 * <p>Functions that are defined for the lisp environment.
 * 
 * @name lisp.functions
 * @namespace
 */
var functions = {}; // This is just for documentation. It doesn't get used.

/**
 * <li>TODO: Test me
 * <li>TODO: Document me
 * <li>TODO: Add examples
 * 
 * @function
 * @name jseval
 * @member lisp.functions
 */
defun("jseval", function () {
	return eval.apply(null, arguments);
});

/**
 * <p>Returns an instance of the given class, initialized with
 * the rest of the given arguments.
 * 
 * <li>TODO: Test me
 * 
 * @function
 * @name new
 * @member lisp.functions
 * 
 * @param {function} cls
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
 *     >> (to-string (new Error "My error message"))
 *     => "Error: My error message"
 */
defun("new", function (cls /*, ... */) {
	if (arguments.length === 0) {
		throw new Error("(new) requires at least 1 argument");
	}
	var args = argsToArray(arguments).slice(1);
	var argnames = args.map(function (item, i, thisObject) { return "args[" + i + "]"; });
	return eval("new cls(" + argnames.join(",") + ")");
});

/**
 * <li>TODO: Test me
 * <li>TODO: Document me
 * <li>TODO: Add examples
 * 
 * @function
 * @name instanceof
 * @member lisp.functions
 */
defun("instanceof", function (object, cls) {
	if (arguments.length !== 2) {
		throw new Error("(instanceof) requires 2 arguments (got " +
			arguments.length + ")");
	}
	return object instanceof cls;
});

/**
 * <p>Throws the given object, or "new Error()" if no object is provided.
 * 
 * @tested
 * 
 * @function
 * @name throw
 * @member lisp.functions
 * 
 * @param {mixed} object The object to throw. Defaults to new Error().
 * 
 * @returns Nothing. After throw'ing the stack is unwided to the
 *          nearest 'catch' block.
 * 
 * @example Basic Error
 *     >> (throw) ; Throws "new Error()"
 * 
 * @example Custom Error
 *     >> (throw (new Error "My Custom Error"))
 * 
 * @example Anything else you can normally throw
 *     >> (throw "a string")
 *     >> (throw 12)
 *     >> (throw (object :type "MyError"))
 *     >> (throw (array 1 2 3))
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
 * <p>Creates an array from the given arguments.
 * 
 * <li>TODO: Test me
 * 
 * @function
 * @name array
 * @alias list
 * @member lisp.functions
 * 
 * @returns The new array.
 * 
 * @example An empty array
 *     >> (array)
 *     => []
 * 
 * @example An array with some elements
 *     >> (array 1 2 "three")
 *     => [1, 2, "three"]
 * 
 * @example A compound array
 *     >> (array (array 1 2) (array 3 4))
 *     => [[1, 2], [3, 4]]
 */
defun("array", function () {
	return argsToArray(arguments);
});

/**
 * <p>Returns the given arguments as an array (this is an alias
 * for (array)).
 * 
 * <li>TODO: Test me
 * 
 * @function
 * @name list
 * @member lisp.functions
 * 
 * @returns The new array.
 */
defun("list", function (/* ... */) {
	return argsToArray(arguments);
});

/**
 * <p>Creates a JavaScript object using the given arguments as a
 * property list to initialize the object. There must be an even
 * number of arguments -- one value for every key.
 * 
 * @tested
 * 
 * @function
 * @name object
 * @member lisp.functions
 * 
 * @returns The new object.
 * 
 * @example An empty object
 *     >> (object)
 *     => {}
 * 
 * @example An object using keywords as keys
 *     >> (object :start 0 :end 10)
 *     => {start: 0, end: 10}
 * 
 * @example An object using strings as keys
 *     >> (object "start" 0 "end" 10)
 *     => {start: 0, end: 10}
 * 
 * @example A compound object
 *     >> (object :name "Joe"
 *                :parents (array (object :name "John")
 *                                (object :name "Jane")))
 *     => {name: "Joe",
 *         parents: [{name: "John"},
 *                   {name: "Jane"}]}
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
 * <p>Returns a value from an object given a key (will work with
 * array indices as well).
 * 
 * <li>TODO: Test me
 * <li>TODO: Add examples
 * 
 * @function
 * @name getkey
 * @member lisp.functions
 */
defun("getkey", function (key, object) {
	if (arguments.length !== 2) {
		throw new Error("(getkey) requires 2 arguments (got " +
			arguments.length + ")");
	}
	return object[key];
});

/**
 * <p>Sets a value on the given object using the given key.
 * 
 * <li>TODO: Test me
 * <li>TODO: Add examples
 * 
 * @function
 * @name setkey
 * @member lisp.functions
 */
defun("setkey", function (key, object, value) {
	if (arguments.length !== 3) {
		throw new Error("(setkey) requires 3 arguments (got " +
			arguments.length + ")");
	}
	return object[key] = value;
});

/**
 * <p>Prints the given arguments to the console.
 * 
 * @tested
 * 
 * @function
 * @name print
 * @member lisp.functions
 * 
 * @returns nil.
 * 
 * @example Basic (print) expression
 *     >> (print "Hello" "Lisp")
 *     Hello Lisp
 *     => nil
 */
defun("print", function () {
	// Do not remove this. This is not a debug statement.
	lisp.log.apply(null, arguments);
	return null;
});

/**
 * <pre>
 * Joins the given arguments together into one string.
 * </pre>
 * 
 * @tested
 * 
 * @function
 * @name concat
 * @member lisp.functions
 * 
 * @returns The string result of the joined arguments.
 * 
 * @example Concatenate a single object (if you really want to)
 *     >> (concat "Hello")
 *     => "Hello"
 * 
 * @example Concatenate many objects
 *     >> (let ((name "Lisp"))
 *          (concat "Hello: " name))
 *     => "Hello: Lisp"
 */
defun("concat", function () {
	return argsToArray(arguments).join("");
});

/**
 * <pre>
 * Joins the given arguments together into one string, using
 * the first argument as the separator.
 * </pre>
 * 
 * @tested
 * 
 * @function
 * @name join
 * @member lisp.functions
 * 
 * @returns The string result of the joined arguments.
 * 
 * @example Join items from a single list
 *     >> (join ", " (list "one" "two"))
 *     => "one, two"
 * 
 * @example Join items from multiple lists
 *     >> (let ((l1 (list "one" "two"))
 *              (l2 (list "three")))
 *          (join ", " l1 l2))
 *     => "one, two, three"
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
 * <pre>
 * Returns the type of the given value (the result of
 * "typeof(value)").
 * </pre>
 * 
 * <li>TODO: Add examples
 * 
 * @tested
 * 
 * @function
 * @name typeof
 * @member lisp.functions
 */
defun("typeof", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(typeof) requires 1 argument (got " +
			arguments.length + ")");
	}
	return typeof(value);
});

/**
 * <pre>
 * Converts the given value to a string.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @function
 * @name to-string
 * @member lisp.functions
 */
defun("to-string", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(to-string) requires 1 argument (got " +
			arguments.length + ")");
	}
	return String(value);
});

/**
 * <pre>
 * Converts the given value to a number.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @function
 * @name to-number
 * @member lisp.functions
 */
defun("to-number", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(to-number) requires 1 argument (got " +
			arguments.length + ")");
	}
	return Number(value);
});

/**
 * <pre>
 * Converts the given value to a number.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @function
 * @name to-boolean
 * @member lisp.functions
 */
defun("to-boolean", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(to-boolean) requires 1 argument (got " +
			arguments.length + ")");
	}
	return Boolean(value);
});

/**
 * <pre>
 * Converts the given value to a json representation of that value.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @function
 * @name to-json
 * @member lisp.functions
 */
defun("to-json", function () {
	return toJSON.apply(null, arguments);
});

/**
 * <pre>
 * Converts the given string to uppercase.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @function
 * @name to-upper
 * @member lisp.functions
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
 * <pre>
 * Converts the given string to uppercase.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @function
 * @name to-lower
 * @member lisp.functions
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
 * <pre>
 * Reduces the given arguments on the / operator.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @function
 * @name /
 * @member lisp.functions
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
 * <pre>
 * Reduces the given arguments on the * operator.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @function
 * @name *
 * @member lisp.functions
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
 * <pre>
 * Reduces the given arguments on the + operator.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @function
 * @name +
 * @member lisp.functions
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
 * <pre>
 * Reduces the given arguments on the - operator.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @function
 * @name -
 * @member lisp.functions
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
 * <pre>
 * Reduces the given arguments on the % operator.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @function
 * @name %
 * @member lisp.functions
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
 * <pre>
 * Adds 1 to the given value.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @function
 * @name 1+
 * @member lisp.functions
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
 * <pre>
 * Subtracts 1 from the given value.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @function
 * @name 1-
 * @member lisp.functions
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
 * <pre>
 * Calls {@link sprintf} (found in the vendor section) with the
 * supplied arguments.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @function
 * @name format
 * @member lisp.functions
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

/**
 * <pre>
 * Run each of the items in the given list through the given
 * function and returns a new list with the given return values.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @function
 * @name map
 * @member lisp.functions
 */
defun("map", function (func, list) {
	if (arguments.length !== 2) {
		throw new Error("(map) requires 2 arguments (got " +
			arguments.length + ")");
	}
	if (typeof(func) != "function") {
		throw new Error("(map) requires a function as its first argument (got " +
			String(func) + ")");
	}
	if (!(list instanceof Array)) {
		throw new Error("(map) requires a list as its second argument (got " +
			String(list) + ")");
	}
	var newlist = [];
	for (var i = 0; i < list.length; i++) {
		newlist.push(func(list[i]));
	}
	return newlist;
});

/**
 * <pre>
 * Returns an object containing the values of each property
 * in the given list on the given object.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @function
 * @name props
 * @member lisp.functions
 */
defun("props", function (object, list) {
	if (arguments.length !== 2) {
		throw new Error("(props) requires 2 arguments (got " +
			arguments.length + ")");
	}
	if (!(list instanceof Array)) {
		throw new Error("(props) requires a list as its second argument " +
			"(got " + String(list) + ")");
	}
	var newObject = {};
	for (var i = 0; i < list.length; i++) {
		var key = list[i];
		newObject[key] = object[key];
	}
	return newObject;
});
