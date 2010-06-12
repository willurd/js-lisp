/**
 * <p>Functions that are defined for the lisp environment.
 * 
 * @name lisp.functions
 * @namespace
 */
var functions = {}; // This is just for documentation. It doesn't get used.

/**
 * <pre>
 * Evaluates the given expression in JavaScript.
 * </pre>
 * 
 * @tested
 * 
 * @name jseval
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} expression
 *     The expression to be evaluated by JavaScript.
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
defun("jseval", function (expression) {
	return eval(expression);
});

/**
 * <pre>
 * Raises new Error(errorMessage) if assertion evaluates to false.
 * </pre>
 * 
 * @tested
 * 
 * @name assert
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} assertion
 *     The expression to evaluate in a boolean context.
 * @param {mixed} [errorMessage]
 *     The option message to be passed to new Error().
 * 
 * @example Asserting a true expression
 *     >> (assert t "Oh no! t is false!")
 *     => nil
 * 
 * @example Asserting a false expression
 *     >> (assert f "f is definitely false")
 *     Error: f is definitely false
 */
defun("assert", function (assertion, errorMessage) {
	// Input validation
	assert(arguments.length > 0, "(assert) requires at least 1 argument");
	assert(arguments.length <= 2, "Too many arguments given to (assert). " +
		"Expected no more than 2 (got " + arguments.length + ")");
	
	// This is the assert the user is making
	assert(assertion, errorMessage);
	
	return null;
});

/**
 * <pre>
 * Returns a symbol that is <em>likely</em> to be unique in your environment.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name gensym
 * @lisp
 * @function
 * @member lisp.functions
 */
defun("gensym", function () {
	// Input validation
	assert(arguments.length === 0, "(gensym) takes no arguments (got " +
		arguments.length + ")");
	
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
	// Input validation
	assert(arguments.length > 0, "(new) requires at least 1 argument");
	assert(typeof(cls) === "function", "(new) requires a function as its first argument " +
		"(got " + toLisp(cls) + ")");
	
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
	// Input validation
	assert(arguments.length === 2, "(instanceof) requires 2 arguments (got " +
		arguments.length + ")");
	assert(typeof(cls) === "function", "(instanceof) requires a function as its second " +
		"argument (got " + toLisp(cls) + ")");
	
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
	// Input validation
	assert(arguments.length <= 1, "(throw) accepts 1 optional argument (got " +
		arguments.length + ")");
	
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
	// Input validation
	assert(arguments.length % 2 === 0, "(object) expects and even number of " +
		"arguments (got " + arguments.length + ")");
	
	var args = argsToArray(arguments);
	var object = {};
	
	for (var i = 0; i < args.length; i += 2) {
		object[args[i]] = args[i+1];
	}
	
	return object;
});

/**
 * <pre>
 * Returns the function that the given expression evaluates to.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name function
 * @lisp
 * @function
 * @member lisp.macros
 */
defun("function", function (value) {
	// Input validation
	assert(arguments.length === 1, "(function) requires 1 argument (got " +
		arguments.length + ")");
	
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
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name getkey
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The value of object[key].
 * 
 * @param {object} object
 *     The object on which to access the given key.
 * @param {mixed} key
 *     The key to access on the given object.
 */
defun("getkey", function (object, key) {
	// Input validation
	assert(arguments.length === 2, "(getkey) requires 2 arguments (got " +
		arguments.length + ")");
	
	return object[key];
});

/**
 * <pre>
 * Sets a value on the given object using the given key.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name setkey
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The given value.
 * 
 * @param {object} object
 *     The object on which to set the given value.
 * @param {mixed} key
 *     The key to set on the given object.
 * @param {mixed} value
 *     The value to set to the given key on the given object.
 */
defun("setkey", function (object, key, value) {
	// Input validation
	assert(arguments.length === 3, "(setkey) requires 3 arguments (got " +
		arguments.length + ")");
	
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
	// Input validation
	assert(arguments.length > 0, "(join) requires at least 1 argument");
	
	var args = argsToArray(arguments);
	var rest = args.slice(1);
	
	for (var i = 0; i < rest.length; i++) {
		var arg = rest[i];
		assert(arg instanceof Array, "(join) got an invalid argument: '" +
		    toLisp(arg) + "' is not a list");
	}
	
	var list = rest.reduce(function (a, b) { return a.concat(b); });
	return list.join(sep);
});

/**
 * <pre>
 * Returns the type of the given value (the result of "typeof(value)").
 * 
 * TODO: Add examples
 * </pre>
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
	// Input validation
	assert(arguments.length === 1, "(typeof) requires 1 argument (got " +
		arguments.length + ")");
	
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
	// Input validation
	assert(arguments.length === 1, "(to-string) requires 1 argument (got " +
		arguments.length + ")");
	
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
	// Input validation
	assert(arguments.length === 1, "(to-number) requires 1 argument (got " +
		arguments.length + ")");
	
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
	// Input validation
	assert(arguments.length === 1, "(to-boolean) requires 1 argument (got " +
		arguments.length + ")");
	
	return Boolean(value);
});

/**
 * <pre>
 * Converts the given value to a json representation of that value.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
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
	// Input validation
	assert(arguments.length > 0, "(to-json) requires at least 1 argument");
	assert(arguments.length <= 3, "Too many arguments given to (to-json). " +
		"Expected no more than 3 (got " + arguments.length + ")");
	
	return toJSON(value, pretty, levels);
});

/**
 * <pre>
 * Returns a string that is the lisp representation of the given value.
 * </pre>
 * 
 * @tested
 * 
 * @name lisp-string
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} value
 *     The value whose lisp representation is returned.
 * 
 * @example Numbers
 *     >> (lisp-string 12)
 *     => "12"
 * 
 * @example Strings
 *     >> (lisp-string "hello")
 *     => "\"hello\""
 * 
 * @example Arrays
 *     >> (lisp-string [1, 2, [3, 4], 5, 6])
 *     => (1 2 (3 4) 5 6)
 * 
 * @example Objects
 * Note: The current (lisp-string) representation of objects can't
 * then be interpreted again in js-lisp.
 *     >> (lisp-string (object :one 2 :three 4))
 *     => "{
 *       \"one\": 2, 
 *       \"three\": 4
 *     }"
 */
defun("lisp-string", function (value) {
	// Input validation
	assert(arguments.length == 1, "(lisp-string) requires 1 argument " +
		"(got " + arguments.length + ")");
	
	return toLisp(value);
});

/**
 * <pre>
 * Converts the given string to uppercase.
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
 * 
 * @example Basic usage
 *     >> (to-upper "hello")
 *     => "HELLO"
 * 
 * @example Practical usage
 *     >> (defun yell (message)
 *     ..   (concat (to-upper message) "!"))
 *     => function () { ... }
 *     >> (yell "hello")
 *     => "HELLO!"
 */
defun("to-upper", function (string) {
	// Input validation
	assert(arguments.length === 1, "(to-upper) requires 1 argument (got " +
		arguments.length + ")");
	assert(typeof(string) === "string", "(to-upper) requires a string argument (got " +
		toLisp(string) + ")");
	
	return string.toUpperCase();
});

/**
 * <pre>
 * Converts the given string to lowercase.
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
 * 
 * @example Basic usage
 *     >> (to-lower "HELLO")
 *     => "hello"
 * 
 * @example Practical usage
 *     >> (defun whisper (message)
 *     ..   (setq message (message.replace "!" ""))
 *     ..   (concat "(" (to-lower message) ")"))
 *     => function () { ... }
 *     >> (whisper "HELLO!")
 *     => "(hello)"
 */
defun("to-lower", function (string) {
	// Input validation
	assert(arguments.length === 1, "(to-lower) requires 1 argument (got " +
		arguments.length + ")");
	assert(typeof(string) === "string", "(to-lower) requires a string argument (got " +
		toLisp(string) + ")");
	
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
	// Input validation
	assert(arguments.length > 0, "(/) requires at least 1 argument");
	
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
	// Input validation
	assert(arguments.length > 0, "(-) requires at least 1 argument");
	
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
	// Input validation
	assert(arguments.length >= 2, "(%) requires at least 2 arguments (got " +
		arguments.length + ")");
	
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
	// Input validation
	assert(arguments.length === 1, "(1+) requires 1 argument (got " +
		arguments.length + ")");
	assert(typeof(number) === "number", "(1+) requires a number argument (got " +
		toLisp(number) + ")");
	
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
	// Input validation
	assert(arguments.length === 1, "(1-) requires 1 argument (got " +
		arguments.length + ")");
	assert(typeof(number) === "number", "(1-) requires a number argument (got " +
		toLisp(number) + ")");
	
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
	// Input validation
	assert(arguments.length >= 2, "(format) expects at least 2 arguments (got " +
		arguments.length + ")");
	assert(typeof(format) === "string", "(format) expects a string format (got " +
		toLisp(format) + ")");
	
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
 * @tested
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
	// Input validation
	assert(arguments.length === 2, "(apply) requires 2 arguments (got " +
		arguments.length + ")");
	assert(typeof(func) === "function", "(apply) requires a function as its " +
		"first argument (got " + toLisp(func) + ")");
	assert(list instanceof Array, "(apply) requires a list as its second " +
		"argument (got " + toLisp(list) + ")");
	
	return func.apply(null, list);
});

/**
 * <pre>
 * Run each of the items in the given list through the given
 * function and returns a new list with the given return values.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
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
	// Input validation
	assert(arguments.length === 2, "(map) requires 2 arguments (got " +
		arguments.length + ")");
	assert(typeof(func) === "function", "(map) requires a function as its first " +
		"argument (got " + toLisp(func) + ")");
	assert(list instanceof Array, "(map) requires a list as its second argument (got " +
		toLisp(list) + ")");
	
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
 * TODO: Add examples
 * </pre>
 * 
 * @tested
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
	// Input validation
	assert(arguments.length === 2, "(props) requires 2 arguments (got " +
		arguments.length + ")");
	
	// Allow the user to supply a single key that's not in a list. This
	// is simply for convenience.
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
 * Creates and returns a list of (key value) pairs from the given object.
 * </pre>
 * 
 * @tested
 * 
 * @name items
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {object} object
 *     The object from which to create the (key value) pairs.
 * 
 * @example Basic objects
 *     >> (items (object :one 2 :three 4 :five 6))
 *     => (("one" 2) ("three" 4) ("five" 6))
 * 
 * @example Arrays
 *     >> (items '(one two three))
 *     => (("0" one) ("1" two) ("2" three))
 */
defun("items", function (object) {
	// Input validation
	assert(arguments.length === 1, "(items) requires 1 argument (got " +
		arguments.length + ")");
	assert(object instanceof Object, "(items) requires an object as its argument " +
		"(got " + toLisp(object) + ")");
	
	var items = [];
	for (var key in object) {
		items.push([key, object[key]]);
	}
	return items;
});

/**
 * <pre>
 * Returns the element at the given index in the given array.
 * 
 * FIXME: Should this accept any object that has a concept of positional
 *        elements, like strings?
 * FIXME: Should this accept a list of indexes as well? e.g. (nth a '(1 3))
 * </pre>
 * 
 * @name nth
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {Array} array
 *     The array that contains the value to return.
 * @param {Number} index
 *     The index to return on the given array.
 * 
 * @example Basic usage
 *     >> (nth '(one two three) 1)
 *     => two
 */
defun("nth", function (array, index) {
	// Input validation
	assert(arguments.length === 2, "(nth) requires 2 arguments (got " +
		arguments.length + ")");
	assert(array instanceof Array, "(nth) requires an Array as its first argument " +
		"(got " + toLisp(array) + ")");
	assert(typeof(index) === "number", "(nth) requires a number as its second argument " +
		"(got " + toLisp(index) + ")");
	
	if (array.length === 0) {
		return null;
	}
	
	return array[index];
});

/**
 * <pre>
 * Returns the first element in the given array.
 * 
 * TODO: Document me
 * TODO: Add examples
 * 
 * FIXME: This should be a macro: `(nth ,list 0)
 * </pre>
 * 
 * @tested
 * 
 * @name first
 * @lisp
 * @function
 * @member lisp.functions
 */
defun("first", function (list) {
	// Input validation
	assert(arguments.length === 1, "(first) requires 1 argument (got " +
		arguments.length + ")");
	assert(list instanceof Array, "(first) requires an Array as its argument " +
		"(got " + toLisp(list) + ")");
	
	if (list.length === 0) {
		return null;
	}
	
	return list[0];
});

/**
 * <pre>
 * Returns the second element in the given array.
 * 
 * TODO: Document me
 * TODO: Add examples
 * 
 * FIXME: This should be a macro: `(nth ,list 1)
 * </pre>
 * 
 * @tested
 * 
 * @name second
 * @lisp
 * @function
 * @member lisp.functions
 */
defun("second", function (list) {
	// Input validation
	assert(arguments.length === 1, "(second) requires 1 argument (got " +
		arguments.length + ")");
	assert(list instanceof Array, "(second) requires an Array as its argument " +
		"(got " + toLisp(list) + ")");
	
	if (list.length < 2) {
		return null;
	}
	
	return list[1];
});

/**
 * <pre>
 * Returns the third element in the given array.
 * 
 * TODO: Document me
 * TODO: Add examples
 * 
 * FIXME: This should be a macro: `(nth ,list 2)
 * </pre>
 * 
 * @tested
 * 
 * @name third
 * @lisp
 * @function
 * @member lisp.functions
 */
defun("third", function (list) {
	// Input validation
	assert(arguments.length === 1, "(third) requires 1 argument (got " +
		arguments.length + ")");
	assert(list instanceof Array, "(third) requires an Array as its argument " +
		"(got " + toLisp(list) + ")");
	
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
	// Input validation
	assert(arguments.length === 2, "(push) requires 2 arguments (got " +
		arguments.length + ")");
	assert(list instanceof Array, "(push) requires an Array as its first " +
		"argument (got " + toLisp(list) + ")");
	
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
	// Input validation
	assert(arguments.length === 1, "(sort!) requires 1 argument (got " +
		arguments.length + ")");
	assert(list instanceof Array, "(sort!) requires an Array as its first " +
		"argument (got " + toLisp(list) + ")");
	
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
	// Input validation
	assert(arguments.length === 1, "(length) requires 1 argument (got " +
		arguments.length + ")");
	
	return object.length;
});
