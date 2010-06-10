/**
 * <p>Functions that are defined for the lisp environment.
 * 
 * @name lisp.functions
 * @namespace
 */
var functions = {}; // This is just for documentation. It doesn't get used.

/**
 * <pre>
 * Applies the given arguments to the JavaScript eval function.
 * </pre>
 * 
 * @tested
 * 
 * @name jseval
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {[mixed]} rest
 *     The arguments to be applied to the JavaScript eval function.
 * @rest rest
 * 
 * @example eval some JSON
 *     >> (setq some-json "{'key1': 1, 'key2': 2}")
 *     => "{'key1': 1, 'key2': 2}"
 *     >> (jseval (concat "(" some-json ")"))
 *     => {
 *       "key1": 1,
 *       "key2": 2
 *     }
 * 
 * @example eval anything
 *     >> (setq my-1+ (jseval "new Function('x', 'return x + 1')"))
 *     => function anonymous(x) { ... }
 *     >> (my-1+ 2)
 *     => 3
 */
defun("jseval", function (/* &rest */) {
	return eval.apply(null, arguments);
});

defun("gensym", function () {
	if (arguments.length > 0) {
		throw new Error("(gensym) takes no arguments (got " +
			arguments.length + ")");
	}
	return gensym();
});

/**
 * <pre>
 * Returns an instance of the given class, initialized with
 * the rest of the given arguments.
 * </pre>
 * 
 * @tested
 * 
 * @name new
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {function} cls
 *     The class to create a new instance of.
 * @param {[mixed]} rest
 *     The arguments to be passed to the class constructor.
 * @rest rest
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
defun("new", function (cls /*, &rest */) {
	if (arguments.length === 0) {
		throw new Error("(new) requires at least 1 argument");
	}
	if (typeof(cls) != "function") {
		throw new Error("(new) requires a function as its first argument " +
			"(got " + String(cls) + ")");
	}
	var args = argsToArray(arguments).slice(1);
	var argnames = args.map(function (item, i, thisObject) { return "args[" + i + "]"; });
	return eval("new cls(" + argnames.join(",") + ")");
});

/**
 * <pre>
 * Returns the value of "value instanceof cls".
 * </pre>
 * 
 * @tested
 * 
 * @name instanceof
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The value of "value instanceof cls".
 * 
 * @param {mixed} value
 *     The left side of the instanceof operator.
 * @param {function} cls
 *     The right side of the instanceof operator.
 * 
 * @example Basic example
 *     >> (instanceof (new Error) Error)
 *     => t
 *     >> (instanceof "not an error" Error)
 *     => f
 */
defun("instanceof", function (value, cls) {
	if (arguments.length !== 2) {
		throw new Error("(instanceof) requires 2 arguments (got " +
			arguments.length + ")");
	}
	if (typeof(cls) != "function") {
		throw new Error("(instanceof) requires a function as its second " +
			"argument (got " + String(cls) + ")");
	}
	return (value instanceof cls);
});

/**
 * <pre>
 * Throws the given object, or "new Error()" if no object is provided.
 * </pre>
 * 
 * @tested
 * 
 * @name throw
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} [value=new Error()]
 *     The value to throw.
 * @optional value
 * 
 * @returns Nothing. After throw'ing the stack is unwound to the
 *          nearest 'catch' block (or to the top).
 * 
 * @example Default Error
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
defun("throw", function (value) {
	if (arguments.length > 1) {
		throw new Error("(throw) accepts 1 optional argument (got " +
			arguments.length + ")");
	}
	value = value || new Error(); // Throw "new Error()" if nothing is provided
	throw value;
});

/**
 * <pre>
 * Creates an array from the given arguments.
 * </pre>
 * 
 * @tested
 * 
 * @name array
 * @alias list
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The new array.
 * 
 * @param {[mixed]} rest
 *     The objects to join into an array.
 * @rest rest
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
defun("array", function (/* &rest */) {
	return argsToArray(arguments);
});

/**
 * <pre>
 * Returns the given arguments as an array (this is an alias
 * for (array)).
 * </pre>
 * 
 * @tested
 * 
 * @name list
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The new array.
 * 
 * @param {[mixed]} rest
 *     The objects to join into an array.
 * @rest rest
 */
defun("list", function (/* &rest */) {
	return argsToArray(arguments);
});

/**
 * <pre>
 * Creates a JavaScript object, using the given arguments as a
 * property list to initialize the object. There must be an even
 * number of arguments -- one value for every key.
 * </pre>
 * 
 * @tested
 * 
 * @name object
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The new object.
 * 
 * @param {[mixed]} rest
 *     The property list from which to make the object.
 * @rest rest
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
defun("object", function (/* &rest */) {
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
 * <pre>
 * Returns the function that the given expression evaluates to.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name function
 * @lisp
 * @function
 * @member lisp.macros
 */
defun("function", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(function) requires 1 argument (got " +
			arguments.length + ")");
	}
	var object = resolve(value);
	if (typeof(object) == "function") {
		return object;
	} else if (object instanceof Macro) {
		return object.callable;
	}
	throw new Error("'" + toLisp(value) + "' is not callable");
});

/**
 * <pre>
 * Returns a value from an object given a key (will work with
 * array indices as well).
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name getkey
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The value of "object[key]".
 * 
 * @param {mixed} key
 *     The key to access on the given object.
 * @param {object} object
 *     The object on which to access the given key.
 */
defun("getkey", function (key, object) {
	if (arguments.length !== 2) {
		throw new Error("(getkey) requires 2 arguments (got " +
			arguments.length + ")");
	}
	return object[key];
});

/**
 * <pre>
 * Sets a value on the given object using the given key.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name setkey
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The given value.
 * 
 * @param {mixed} key
 *     The key to set on the given object.
 * @param {object} object
 *     The object on which to set the given key.
 * @param {mixed} value
 *     The value to set to the given key on the given object.
 */
defun("setkey", function (key, object, value) {
	if (arguments.length !== 3) {
		throw new Error("(setkey) requires 3 arguments (got " +
			arguments.length + ")");
	}
	object[key] = value;
	return value;
});

/**
 * <pre>
 * Prints the given arguments to the console.
 * </pre>
 * 
 * @tested
 * 
 * @name print
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns nil.
 * 
 * @param {[mixed]} rest
 *     The objects to print.
 * @rest rest
 * 
 * @example Basic (print) expression
 *     >> (print "Hello" "Lisp")
 *     Hello Lisp
 *     => nil
 */
defun("print", function (/* &rest */) {
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
 * @name concat
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The string result of the joined arguments.
 * 
 * @param {[mixed]} rest
 *     The objects to join together into the returning string.
 * @rest rest
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
defun("concat", function (/* &rest */) {
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
 * @name join
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The string result of the joined arguments.
 * 
 * @param {mixed} sep
 *     The value that separates the objects in the returning string.
 * @param {[mixed]} rest
 *     The objects to join together into the returning string,
 *     separated by 'sep'.
 * @rest rest
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
defun("join", function (sep /*, &rest */) {
	if (arguments.length === 0) {
		throw new Error("(join) requires at least 1 argument");
	}
	var args = argsToArray(arguments);
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
 * Returns the type of the given value (the result of "typeof(value)").
 * </pre>
 * 
 * <li>TODO: Add examples
 * 
 * @tested
 * 
 * @name typeof
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} value
 *     The value whose type is returned.
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
 * @name to-string
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} value
 *     The value to turn into a string.
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
 * @name to-number
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} value
 *     The value to turn into a number.
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
 * @name to-boolean
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} value
 *     The value to turn into a boolean.
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
 * @name to-json
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} value
 *     The value whose json representation is returned.
 * @param {boolean} pretty
 *     Whether to pretty-print the resulting json string.
 * @param {boolean} levels
 *     The amount of levels deep to go before stopping.
 * @optional pretty
 * @optional levels
 */
defun("to-json", function (value, pretty, levels) {
	return toJSON(value, pretty, levels);
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name lisp-string
 * @lisp
 * @function
 * @member lisp.functions
 */
defun("lisp-string", function (value) {
	return toLisp(value);
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
 * @name to-upper
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of "string.toUpperCase()".
 * 
 * @param {string} string
 *     The string to convert to uppercase.
 */
defun("to-upper", function (string) {
	if (arguments.length !== 1) {
		throw new Error("(to-upper) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (typeof(string) != "string") {
		throw new Error("(to-upper) requires a string argument (got " +
			String(string) + ")");
	}
	return string.toUpperCase();
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
 * @name to-lower
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of "string.toLowerCase()".
 * 
 * @param {string} string
 *     The string to convert to lowercase.
 */
defun("to-lower", function (string) {
	if (arguments.length !== 1) {
		throw new Error("(to-lower) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (typeof(string) != "string") {
		throw new Error("(to-lower) requires a string argument (got " +
			String(string) + ")");
	}
	return string.toLowerCase();
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
 * @name /
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of reducing all of the given arguments
 *          on the division operator.
 * 
 * @param {[number]} rest
 *     The numbers to divide into each other.
 * @rest rest
 */
defun("/", function (/* &rest */) {
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
 * @name *
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of reducing all of the given arguments
 *          on the multiplication operator.
 * 
 * @param {[number]} rest
 *     The numbers to multiply by each other.
 * @rest rest
 */
defun("*", function (/* &rest */) {
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
 * @name +
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of reducing all of the given arguments
 *          on the addition operator.
 * 
 * @param {[number]} rest
 *     The numbers to add to each other.
 * @rest rest
 */
defun("+", function (/* &rest */) {
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
 * @name -
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of reducing all of the given arguments
 *          on the subtraction operator.
 * 
 * @param {[number]} rest
 *     The numbers to subtract by each other.
 * @rest rest
 */
defun("-", function (/* &rest */) {
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
 * @name %
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of reducing all of the given arguments
 *          on the modulo operator.
 * 
 * @param {[number]} rest
 *     The numbers on which to use the modulo operator.
 * @rest rest
 */
defun("%", function () {
	if (arguments.length < 2) {
		throw new Error("(%) requires at least 2 arguments (got " +
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
 * @name 1+
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of adding 1 to the given number.
 * 
 * @param {number} number
 *     The number to add 1 to.
 */
defun("1+", function (number) {
	if (arguments.length !== 1) {
		throw new Error("(1+) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (typeof(number) != "number") {
		throw new Error("(1+) requires a number argument (got " +
			number + ")");
	}
	return number + 1;
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
 * @name 1-
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of subtracting 1 from the given number.
 * 
 * @param {number} number
 *     The number to subtract 1 from.
 */
defun("1-", function (number) {
	if (arguments.length !== 1) {
		throw new Error("(1-) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (typeof(number) != "number") {
		throw new Error("(1-) requires a number argument (got " +
			number + ")");
	}
	return number - 1;
});

/**
 * <pre>
 * Calls sprintf (found in the vendor section) with the supplied arguments.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name format
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of applying the given arguments to sprintf
 *          (in the vendor section) if print evaluates to true,
 *          otherwise nil.
 * 
 * @param {boolean} print
 *     Whether to print the result, or return it.
 * @param {string} format
 *     The string format to which to apply the given arguments.
 * @param {[mixed]} rest
 *     The arguments to apply to the given format
 * @rest rest
 */
defun("format", function (print, format /*, &rest */) {
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
 * Calls apply on the given function and passes in the given
 * list as arguments.
 * </pre>
 * 
 * @name apply
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of applying the given list as the arguments to
 *          the given function.
 * 
 * @param {function} func
 *     The function to apply the list of arguments to.
 * @param {Array} list
 *     The list to give as arguments to the given function.
 * 
 * @example Add a list of numbers together
 *     >> (apply + '(1 2 3))
 *     => 6
 * 
 * @example Print a list of values
 *     >> (apply print '("arg1", "arg2", '("a" "list")))
 *     arg1 , arg2 , quote,a,list
 *     => nil
 */
defun("apply", function (func, list) {
	if (arguments.length !== 2) {
		throw new Error("(apply) requires 2 arguments (got " +
			arguments.length + ")");
	}
	if (typeof(func) != "function") {
		throw new Error("(apply) requires a function as its " +
			"first argument (got " + String(func) + ")");
	}
	if (!(list instanceof Array)) {
		throw new Error("(apply) requires a list as its second " +
			"argument (got " + String(list) + ")");
	}
	return func.apply(null, list);
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
 * @name map
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of mapping the given list to the given function.
 * 
 * @param {function} func
 *     The function which gets passed each value in the given list.
 * @param {Array} list
 *     The list of values to pass to the given function.
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
 * @name props
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns A new object containing only the values on the given
 *          object specified by the list of keys.
 * 
 * @param {object} object
 *     The object containing the values to return (specified by the
 *     given list of keys).
 * @param {mixed} value
 *     The list of keys (or single key) specifying which values to
 *     return in the resulting object.
 */
defun("props", function (object, list) {
	if (arguments.length !== 2) {
		throw new Error("(props) requires 2 arguments (got " +
			arguments.length + ")");
	}
	if (!(list instanceof Array)) {
		list = [list];
	}
	function makeObject (fromobj, toobj, keyset) {
		var value;
		var first;
		for (var i = 0; i < keyset.length; i++) {
			try {
				value = keyset[i];
				if (value instanceof Array && value.length == 1) {
					value = value[0];
				}
				if (value instanceof Array) {
					if (value.length === 0) {
						continue;
					}
					first = value[0];
					toobj[first] = toobj[first] || {};
					makeObject(fromobj[first], toobj[first], value.slice(1));
				} else {
					toobj[value] = fromobj[value];
				}
			} catch (e) {
				if (e instanceof TypeError) {
					return toobj;
				}
				throw e;
			}
		}
		return toobj;
	}
	return makeObject(object, {}, list.map(function (k) { return String(k).split("."); }));
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name items
 * @lisp
 * @function
 * @member lisp.functions
 */
defun("items", function (object) {
	if (arguments.length !== 1) {
		throw new Error("(items) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (!(object instanceof Object)) {
		throw new Error("(items) requires an object as its argument " +
			"(got " + String(object) + ")");
	}
	
	var items = [];
	for (var key in object) {
		items.push([key, object[key]]);
	}
	return items;
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name first
 * @lisp
 * @function
 * @member lisp.functions
 */
defun("nth", function (list, index) {
	if (arguments.length !== 2) {
		throw new Error("(nth) requires 2 arguments (got " +
			arguments.length + ")");
	}
	if (!(list instanceof Array)) {
		throw new Error("(nth) requires an Array as its first argument " +
			"(got " + toLisp(list) + ")");
	}
	if (typeof(index) != "number") {
		throw new Error("(nth) requires a number as its second argument " +
			"(got " + toLisp(index) + ")");
	}
	if (list.length === 0) {
		return null;
	}
	return list[index];
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name first
 * @lisp
 * @function
 * @member lisp.functions
 */
defun("first", function (list) {
	if (arguments.length !== 1) {
		throw new Error("(first) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (!(list instanceof Array)) {
		throw new Error("(first) requires an Array as its argument " +
			"(got " + String(list) + ")");
	}
	if (list.length === 0) {
		return null;
	}
	return list[0];
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name second
 * @lisp
 * @function
 * @member lisp.functions
 */
defun("second", function (list) {
	if (arguments.length !== 1) {
		throw new Error("(second) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (!(list instanceof Array)) {
		throw new Error("(second) requires an Array as its argument " +
			"(got " + String(list) + ")");
	}
	if (list.length < 2) {
		return null;
	}
	return list[1];
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name third
 * @lisp
 * @function
 * @member lisp.functions
 */
defun("third", function (list) {
	if (arguments.length !== 1) {
		throw new Error("(third) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (!(list instanceof Array)) {
		throw new Error("(third) requires an Array as its argument " +
			"(got " + String(list) + ")");
	}
	if (list.length < 3) {
		return null;
	}
	return list[2];
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name push
 * @lisp
 * @function
 * @member lisp.functions
 */
defun("push", function (list, value) {
	if (arguments.length !== 2) {
		throw new Error("(push) requires 2 arguments (got " +
			arguments.length + ")");
	}
	if (!(list instanceof Array)) {
		throw new Error("(push) requires an Array as its first " +
			"argument (got " + String(list) + ")");
	}
	list.push(value);
	return list;
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name sort!
 * @lisp
 * @function
 * @member lisp.functions
 */
defun("sort!", function (list) {
	if (arguments.length !== 1) {
		throw new Error("(sort!) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (!(list instanceof Array)) {
		throw new Error("(sort!) requires an Array as its first " +
			"argument (got " + String(list) + ")");
	}
	return list.sort();
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name sort
 * @lisp
 * @function
 * @member lisp.functions
 */
var _function_sort;

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name length
 * @lisp
 * @function
 * @member lisp.functions
 */
defun("length", function (object) {
	if (arguments.length !== 1) {
		throw new Error("(length) requires 1 argument (got " +
			arguments.length + ")");
	}
	return object.length;
});
