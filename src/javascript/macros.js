/**
 * <pre>
 * Macros that are defined for the lisp environment.
 * </pre>
 * 
 * @name lisp.macros
 * @namespace
 */
var macros = {}; // This is just for documentation. It doesn't get used.

/**
 * <pre>
 * Takes a single lisp expression (s-expression) and returns it
 * unevaluated.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name quote
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @returns The given argument, unevaluated.
 */
defmacro("quote", function (expression) {
	if (arguments.length !== 1) {
		throw new Error("(quote) requires 1 argument (got " +
			arguments.length + ")");
	}
	return expression;
});

/**
 * <pre>
 * Creates an anonymous function with the first (required)
 * expression as its arglist and which executes the rest of the
 * expressions when called.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @name lambda
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @returns The created function.
 */
defmacro("lambda", function (arglist /*, ... */) {
	var env  = new Env(lisp.env);
	var args = argsToArray(arguments);
	
	if (arguments.length > 0 && !(arglist instanceof Array)) {
		throw new Error("(lambda) requires a list as its first expression " +
			"(got " + String(arglist) + ")");
	}
	
	return (function (env, args) {
		var body = args.slice(1);
		return function () {
			if (args.length < 2) {
				return null; // This function does nothing
			}
			var largs = argsToArray(arguments);
			var tempEnv = lisp.env;
			var i;
			lisp.env = env;
			lisp.env.let("this", this);
			for (i = 0; i < arglist.length; i++) {
				var argname = arglist[i];
				if (argname == "&rest") {
					if (i == arglist.length - 1) {
						throw new Error("No argument name after &rest identifier");
					}
					if (arglist.length > i + 2) {
						throw new Error("Unexpected arguments (" +
							arglist.slice(i+1).join(" ") + ") after &rest argument");
					}
					lisp.env.let(arglist[i+1], largs.slice(i));
					break;
				} else {
					lisp.env.let(argname, largs[i]);
				}
			}
			var ret = null;
			for (i = 0; i < body.length; i++) {
				ret = resolve(body[i]);
			}
			lisp.env = tempEnv;
			return ret;
		};
	})(env, args);
});

/**
 * <pre>
 * Defines a function. This is shorthand for (setq name (lambda ...)).
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name defun
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("defun", function (name, arglist /*, ... */) {
	if (arguments.length === 0) {
		throw new Error("(defun) requires at least 1 argument.");
	}
	if (!(name instanceof Symbol)) {
		throw new Error("(defun) requires a symbol as its first argument (got " +
			String(name) + ")");
	}
	if (arguments.length > 1 && !(arglist instanceof Array)) {
		throw new Error("(defun) requires a list of symbols as its second " +
			"expression (got " + String(arglist) + ")");
	}
	var body = argsToArray(arguments).slice(2);
	var lambda = [_S("lambda"), arglist].concat(body);
	resolve([_S("setq"), name, lambda]);
	return null;
});

/**
 * <pre>
 * Provides JavaScript's try/catch/finally feature to the lisp
 * environment.
 * 
 * Note: (catch) and (finally) must be the last expressions in the
 * (try) expression, and (catch) must come before (finally). The only
 * valid uses of (catch) and (finally) in a (try) expression are:
 * 
 *     - (try ...) ; Not using either of them
 *     - (try ... (catch ...))
 *     - (try ... (finally ...))
 *     - (try ... (catch ...) (finally ...))
 * </pre>
 * 
 * @tested
 * 
 * @name try
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @returns The return value of the last evaluated body expression
 *          (i.e. non-catch, non-finally expression).
 * 
 * @example Empty try block
 *   >> (try)
 *   => nil
 * 
 * @example Empty catch block (silences the error)
 *   >> (try
 *          (throw (new Error))
 *        (catch))
 *   => nil
 * 
 * @example Multiple expressions with a full catch block
 *   >> (try
 *          (print "This will print")
 *          (throw (new Error "This cuts the expression short"))
 *          (print "This will not print")
 *        (catch (e)
 *          (format t "This will print when the error is thrown: %s" e)))
 *   => (no return value)
 * 
 * @example try/finally
 *   >> (try
 *          (throw (new Error))
 *        (finally
 *          (print "This always runs")))
 *   => (no return value) ; Due to the error that is thrown
 * 
 * @example try/catch/finally
 *   >> (try
 *          "hello" ; This will get returned because it's the last evaluated expression
 *          (throw (new Error))
 *        (catch (e)
 *          (print "This will get called"))
 *        (finally
 *          (print "This always runs")))
 *   => "hello" ; The error is caught, so there is a return value
 * 
 * @example catch/finally symbols as keywords
 *   >> (try
 *          (throw (new Error))
 *        (:catch) ; This will work
 *        (:finally)) ; And so will this
 *   => nil
 */
defmacro("try", function () {
	var args = argsToArray(arguments);
	var checkIndex = args.length-1;
	
	// Grab the catch and finally expressions if they are there.
	var check = function (symbol) {
		if (checkIndex < 0) {
			return null; // There are no expressions to check
		}
		var expr = args[checkIndex];
		if ((expr instanceof Array) && // The "catch" expression must be a list
			(expr.length > 0) && // It must at least have the symbol specifying the block type
			((expr[0] instanceof Symbol) || (expr[0] instanceof Keyword)) &&
			(expr[0].value == symbol)) {
			args = args.slice(0, -1);
			checkIndex--;
			return expr;
		}
		return null;
	};
	var finallyExpression = check("finally");
	var catchExpression = check("catch");
	
	var ret = null;
	var i;
	
	try {
		// Evaluate all of the expressions in the body, saving the return
		// value of the last expression for returning from this expression.
		for (i = 0; i < args.length; i++) {
			ret = resolve(args[i]);
		}
	} catch (e) {
		// Evaluate the `catch` expression if there is one.
		if (catchExpression) {
			var expression = [_S("lambda")].concat(catchExpression.slice(1));
			if (expression.length === 1) { // Add an arglist if there isn't one
				expression.push([]);
			}
			var callback = resolve(expression);
			callback(e);
		} else {
			// If there is no catch expression, throw the error for something
			// else to catch it (or not).
			throw e;
		}
	} finally {
		// Evaluate all expressions in the `finally` expression if there
		// is one.
		if (finallyExpression) {
			for (i = 0; i < finallyExpression.length; i++) {
				resolve(finallyExpression[i]);
			}
		}
	}
	
	return ret;
});

/**
 * <pre>
 * Returns the function that the given symbol points to.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name getfunc
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("getfunc", function (symbol) {
	if (arguments.length !== 1) {
		throw new Error("(getfunc) requires 1 argument (got " +
			arguments.length + ")");
	}
	var object = lisp.env.get(symbol);
	if (typeof(object) == "function") {
		return object;
	} else if (object instanceof Macro) {
		return object.callable;
	}
	throw new Error("'" + symbol + "' is not a function or macro");
});

/**
 * <pre>
 * Takes an object and a dotpath and calls the dotpath as a function
 * on the given object (with the given arguments).
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name funcall
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("funcall", function (object, dotpath) {
	if (arguments.length < 2) {
		throw new Error("(funcall) requires at least 2 arguments " +
			"(got " + arguments.length + ")");
	}
	// Grab the object.
	object = resolve(object);
	// Make sure we can get a string dotpath from the supplied argument.
	if (dotpath instanceof Symbol || dotpath instanceof Keyword) {
		dotpath = dotpath.value;
	} else if (typeof(dotpath) != "string") {
		throw new Error("Unknown function key in (funcall): " + String(dotpath));
	}
	// Resolve the object down to the second-to-last part of the dot path.
	var parts = String(dotpath).split(".");
	for (var i = 0; i < parts.length-1; i++) {
		object = object[parts[i]];
	}
	// Make sure what's being "called" is actually a function.
	var funckey = parts[parts.length-1];
	if (typeof(object[funckey]) != "function") {
		throw new Error(String(dotpath) + " on " + object + " is not a function");
	}
	var args = argsToArray(arguments).slice(2).map(resolve);
	return object[funckey].apply(object, args);
});

/**
 * <pre>
 * FIXME: This method sucks (actually, env.let sucks)
 * TODO: Test me more
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name let
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("let", function () {
	var args = argsToArray(arguments);
	var letset = args[0];
	var i;
	lisp.env = new Env(lisp.env);
	args = args.slice(1);
	
	for (i = 0; i < letset.length; i++) {
		var symbol = letset[i][0];
		var value = resolve(letset[i][1]);
		lisp.env.let(symbol, value);
	}
	
	var ret = null;
	for (i = 0; i < args.length; i++) {
		ret = resolve(args[i]);
	}
	lisp.env = lisp.env.parent;
	return ret;
});

/**
 * <pre>
 * TODO: Test me more
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name setq
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("setq", function () {
	var args = argsToArray(arguments);
	var symbol = args[0];
	var value  = resolve(args[1]);
	lisp.env.set(symbol, value);
	return value;
});

/**
 * <pre>
 * Simply executes all of the given expressions in order. This
 * is mainly for being able to execute multiple expressions inside
 * of places in other macros/functions where only one expression
 * can go.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name progn
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @returns The return value of the last expression, or nil if there
 *          are no expression.
 */
defmacro("progn", function (/* .. */) {
	var ret = null;
	for (var i = 0; i < arguments.length; i++) {
		ret = resolve(arguments[i]);
	}
	return ret;
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name cond
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @returns The value of the last evaluated expression, or nil.
 */
defmacro("cond", function () {
	for (var i = 0; i < arguments.length; i++) {
		var clause = arguments[i];
		if (clause.length === 0) {
			throw new Error("(cond) clauses must contain an expression to evaluate");
		}
		var condition = clause[0];
		if (!!resolve(condition)) {
			var ret;
			for (var j = 1; j < clause.length; j++) {
				ret = resolve(clause[j]);
			}
			return ret;
		}
	}
	return null;
});

// TODO: Create (case) or (switch) macro

/**
 * <pre>
 * If the first expression evaluates to true in a boolean context,
 * this macro evaluates and returns the result of the second
 * expression, otherwise it evaluates all of the remaining expression
 * and returns the return value of the last one.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name if
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @returns The return value of either the second or last expression, or
 *          nil if testExpression evaluates to false and there are no
 *          remaining expressions to evaluate.
 */
defmacro("if", function (testExpression, ifTrueExpression /*, ... */) {
	if (arguments.length < 2) {
		throw new Error("(if) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	if (!!resolve(testExpression)) { // testExpression evaluates to true
		return resolve(ifTrueExpression);
	} else { // Evaluate all of the expressions after ifTrueExpression
		var ret = null;
		var i = 2; // Start at the 3rd expression
		for (; i < arguments.length; i++) {
			ret = resolve(arguments[i]);
		}
		return ret;
	}
	return null; // This will never happen
});

/**
 * <pre>
 * Executes the rest of the arguments if the first argument
 * is true.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name when
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @returns The return value of the last expression.
 */
defmacro("when", function () {
	if (arguments.length === 0) {
		throw new Error("(when) requires at least 1 argument " +
			"(got " + arguments.length + ")");
	}
	if (!!resolve(arguments[0])) {
		var args = argsToArray(arguments).slice(1).map(resolve);
		return args[args.length-1];
	}
	return null;
});

/**
 * <pre>
 * Performs a logical negation on the given value.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name not
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("not", function (value) {	
	if (arguments.length === 0) {
		throw new Error("(not) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return !value;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name or
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("or", function () {
	for (var i = 0; i < arguments.length; i++) {
		if (resolve(arguments[i])) {
			return true;
		}
	}
	return false;
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name and
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("and", function () {
	if (arguments.length === 0) {
		return true;
	}
	return predicate(arguments, function (value) {
		return !!value;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name equal
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("equal", function () {
	if (arguments.length < 2) {
		throw new Error("(equal) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return equal(a, b);
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name not-equal
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("not-equal", function () {
	if (arguments.length < 2) {
		throw new Error("(not-equal) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return !equal(a, b);
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name ==
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("==", function () {
	if (arguments.length < 2) {
		throw new Error("(==) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a == b;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name ===
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("===", function () {
	if (arguments.length < 2) {
		throw new Error("(===) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a === b;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name !=
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("!=", function () {
	if (arguments.length < 2) {
		throw new Error("(!=) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a != b;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name !==
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("!==", function () {
	if (arguments.length < 2) {
		throw new Error("(!==) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a !== b;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * </pre>
 * 
 * @name <
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @example Comparing two arguments
 *     >> (< 3 4)
 *     => t
 * 
 * @example Comparing many arguments
 *     >> (< 3 4 5 1)
 *     => false
 */
defmacro("<", function () {
	if (arguments.length < 2) {
		throw new Error("(<) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a < b;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * </pre>
 * 
 * @name >
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @example Comparing two arguments
 *     >> (> 4 3)
 *     => t
 * 
 * @example Comparing many arguments
 *     >> (> 4 3 2 8)
 *     => false
 */
defmacro(">", function () {
	if (arguments.length < 2) {
		throw new Error("(>) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a > b;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * </pre>
 * 
 * @name <=
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @example Comparing two arguments
 *     >> (<= 3 4 4)
 *     => t
 * 
 * @example Comparing many arguments
 *     >> (<= 3 4 4 3)
 *     => false
 */
defmacro("<=", function () {
	if (arguments.length < 2) {
		throw new Error("(<=) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a <= b;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * </pre>
 * 
 * @name >=
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @example Comparing two arguments
 *     >> (>= 4 3 3)
 *     => t
 * 
 * @example Comparing many arguments
 *     >> (>= 4 3 3 4)
 *     => false
 */
defmacro(">=", function () {
	if (arguments.length < 2) {
		throw new Error("(>=) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a >= b;
	});
});

/**
 * <pre>
 * Returns true if the given values === true.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-true
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-true", function () {
	if (arguments.length === 0) {
		throw new Error("(is-true) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return value === true;
	});
});

/**
 * <pre>
 * Returns true if the given values === false.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-false
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-false", function () {
	if (arguments.length === 0) {
		throw new Error("(is-false) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return value === false;
	});
});

/**
 * <pre>
 * Returns true if the given values === null.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-null
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-null", function () {
	if (arguments.length === 0) {
		throw new Error("(is-null) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return value === null;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-undefined
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-undefined", function () {
	if (arguments.length === 0) {
		throw new Error("(is-undefined) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return value === undefined;
	});
});

/**
 * <pre>
 * Returns true if the given values are strings.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-string
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-string", function () {
	if (arguments.length === 0) {
		throw new Error("(is-string) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return typeof(value) == "string";
	});
});

/**
 * <pre>
 * Returns true if the given values are numbers.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-number
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-number", function () {
	if (arguments.length === 0) {
		throw new Error("(is-number) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return typeof(value) == "number";
	});
});

/**
 * <pre>
 * Returns true if the given values are booleans.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-boolean
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-boolean", function () {
	if (arguments.length === 0) {
		throw new Error("(is-boolean) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return typeof(value) == "boolean";
	});
});

/**
 * <pre>
 * Returns true if the given values are functions.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-function
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-function", function () {
	if (arguments.length === 0) {
		throw new Error("(is-function) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return typeof(value) == "function";
	});
});

/**
 * <pre>
 * Returns true if the given values are objects.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-object
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-object", function () {
	if (arguments.length === 0) {
		throw new Error("(is-object) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return typeof(value) == "object";
	});
});

/**
 * <pre>
 * Returns true if the given values are arrays.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-array
 * @alias is-list
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-array", function () {
	if (arguments.length === 0) {
		throw new Error("(is-array) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return value instanceof Array;
	});
});

/**
 * <pre>
 * Returns true if the given values are arrays (this is an alias
 * for (is-array)).
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-list
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-list", function () {
	if (arguments.length === 0) {
		throw new Error("(is-list) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return value instanceof Array;
	});
});

/**
 * <pre>
 * An expression for basic iteration over a list.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @name dolist
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("dolist", function (arglist /*, ... */) {
	if (arguments.length === 0) {
		throw new Error("(dolist) requires at least 1 argument (got " +
			arguments.length + ")");
	}
	if (!(arglist instanceof Array)) {
		throw new Error("(dolist) got invalid argument list: " + String(arglist));
	}
	if (arglist.length < 2 || arglist.length > 3) {
		throw new Error("(dolist) got invalid argument list. Requires at least " +
			"2 arguments and no more than 3 (got " + arglist.length + ")");
	}
	
	var itemName = arglist[0];
	var list     = resolve(arglist[1]);
	
	if (!(itemName instanceof Symbol)) {
		throw new Error("(dolist) got invalid argument list. First argument " +
			"must be a symbol (got " + String(itemName) + ")");
	}
	if (!(list instanceof Array)) {
		throw new Error("(dolist) got invalid argument list. Second argument " +
			"must be a list (got " + String(list) + ")");
	}
	
	lisp.env = new Env(lisp.env);
	var body = argsToArray(arguments).slice(1);
	var ret;
	for (var i = 0; i < list.length; i++) {
		lisp.env.let(itemName, list[i]);
		for (var j = 0; j < body.length; j++) {
			ret = resolve(body[j]);
		}
	}
	lisp.env = lisp.env.parent;
	
	if (arglist.length === 3) {
		return resolve(arglist[2]);
	} else {
		return ret;
	}
});

// TODO: Write macro (dotimes)

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name foreach
 * @lisp
 * @function
 * @macro
 * @member lisp.macros
 * 
 * @example Basic usage
 *     >> (let ((set (object :one 1 :two 2 :three 3)))
 *          (foreach (item set)
 *            (print (first item))))
 *     one
 *     two
 *     three
 *     => nil
 */
defmacro("foreach", function (arglist /*, &rest */) {
	if (arguments.length === 0) {
		throw new Error("(foreach) requires at least 1 argument");
	}
	if (!(arglist instanceof Array)) {
		throw new Error("(foreach) requires a list as its first " +
			" argument (got " + String(arglist) + ")");
	}
	if (arglist.length !== 2) {
		throw new Error("(foreach) got invalid argument list. Requires " +
			"2 arguments (got " + arglist.length + ")");
	}
	
	var itemName = arglist[0];
	var object   = resolve(arglist[1]);
	
	if (!(itemName instanceof Symbol)) {
		throw new Error("(foreach) got invalid argument list. First argument " +
			"must be a symbol (got " + String(itemName) + ")");
	}
	if (!(object instanceof Object)) {
		throw new Error("(foreach) got invalid argument list. Second argument " +
			"must be an object (got " + String(object) + ")");
	}
	
	var body = argsToArray(arguments).slice(1);
	
	lisp.env = new Env(lisp.env);
	var ret = null;
	var value;
	for (var key in object) {
		value = object[key];
		lisp.env.let(itemName, [key, value]);
		for (var i = 0; i < body.length; i++) {
			ret = resolve(body[i]);
		}
	}
	lisp.env = lisp.env.parent;
	
	return ret;
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * 
 * FIXME: This should really be defined in lisp (/src/lisp/core.lisp),
 *        after (defmacro) is created.
 * </pre>
 * 
 * @name collect
 * @lisp
 * @function
 * @macro
 * @member lisp.macros
 * 
 * @translation
 *     (let ((itemName (first arglist)))
 *       `(let ((set '()))
 *          (foreach ,arglist
 *            (when (progn ,rest)
 *              (push set ,itemName)))
 *          set))
 * 
 * @example Basic usage
 *     >> (let ((obj (object :name "js-lisp" :age 0)))
 *          (collect (item obj)
 *            (is-number (second item))))
 *     => (("age" 0)) ; Returns every (key,value) pair where the last 
 *                    ; expression of the body evaluates to true.
 */
defmacro("collect", function (arglist /*, &rest */) {
	var itemName = ((arglist instanceof Array) &&
					(arglist.length > 0)) ? arglist[0] : null;
	var body = argsToArray(arguments).slice(1);
	
	// This is really nasty, and already not fun to debug. (defmacro)
	// needs to get made asap.
	return resolve(
		[_S("let"), [[_S("set"), []]],
			[_S("foreach"), arglist,
				[_S("when"), [_S("progn")].concat(body),
					[_S("set.push"), itemName]]],
			_S("set")]);
});
