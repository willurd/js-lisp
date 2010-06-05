/**
 * Takes a single lisp expression (s-expression) and returns it unevaluated.
 * 
 * TODO: Test me
 * 
 * @return The given argument, unevaluated.
 */
defmacro("quote", function (expression) {
	if (arguments.length !== 1) {
		throw new Error("(quote) requires 1 argument (got " +
			arguments.length + ")");
	}
	return expression;
});

/**
 * Creates an anonymous function with the first (required) expression
 * as its arglist and which executes the rest of the expressions
 * when called.
 * 
 * TODO: Test me
 * 
 * @return The created function.
 */
defmacro("lambda", function (arglist /*, ... */) {
	var env  = new Env(lisp.env);
	var args = argsToArray(arguments);
	return (function (env, args) {
		var body = args.slice(1);
		return function () {
			var tempEnv = lisp.env;
			var i;
			lisp.env = env;
			lisp.env.let("this", this);
			for (i = 0; i < arglist.length; i++) {
				lisp.env.let(arglist[i], arguments[i]);
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
 * Defines a function.
 * 
 * TODO: Test me
 * TODO: Reuse (lambda) for this.
 */
defmacro("defun", function (name, arglist /*, ... */) {
	var body = argsToArray(arguments).slice(2);
	
	lisp.env.set(name, function () {
		var args = argsToArray(arguments);
		var i;
		lisp.env = new Env(lisp.env);
		for (i = 0; i < arglist.length; i++) {
			var argname = arglist[i];
			if (argname == "&rest") {
				if (i == arglist.length - 1) {
					throw new Error("No rest argument after &rest identifier in (defun) arglist");
				}
				if (arglist.length > i + 2) {
					throw new Error("Unexpected arguments (" + arglist.slice(i+1).join(" ") +
						") after &rest argument");
				}
				lisp.env.let(arglist[i+1], args.slice(i));
				break;
			} else {
				lisp.env.let(argname, args[i]);
			}
		}
		var ret = null;
		for (i = 0; i < body.length; i++) {
			ret = resolve(body[i]);
		}
		lisp.env = lisp.env.parent;
		return ret;
	});
	
	return null;
});

/**
 * Provides JavaScript's try/catch feature to the lisp environment.
 * 
 * Note: (catch) and (finally) must be the last expressions in the
 * (try) expression, and (catch) must come before (finally). The only
 * valid uses of (catch) and (finally) in a (try) expression are:
 * 
 *     - (try ...) ; Not using either of them
 *     - (try ... (catch ...))
 *     - (try ... (finally ...))
 *     - (try ... (catch ...) (finally ...))
 * 
 * @return The return value of the last evaluated expression.
 * 
 * @tested
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
			var expression = [new Symbol("lambda")].concat(catchExpression.slice(1));
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
 * Returns the function that the given symbol points to.
 * 
 * TODO: Test me
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
 * Takes an object and a dotpath and calls the dotpath as a function
 * on the given object (with the given arguments).
 * 
 * TODO: Test me
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
 * TODO: Test me
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
 * TODO: Test me
 */
defmacro("setq", function () {
	var args = argsToArray(arguments);
	var symbol = args[0];
	var value  = resolve(args[1]);
	lisp.env.set(symbol, value);
	return value;
});

/**
 * Simply executes all of the given expressions in order. This
 * is mainly for being able to execute multiple expressions inside
 * of places in other macros/functions where only one expression
 * can go.
 * 
 * @return The return value of the last expression, or nil if there
 *         are no expression.
 * 
 * @tested
 */
defmacro("progn", function (/* .. */) {
	var ret = null;
	for (var i = 0; i < arguments.length; i++) {
		ret = resolve(arguments[i]);
	}
	return ret;
});

/**
 * @return The value of the evaluated expression, or nil.
 * 
 * TODO: Test me
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
 * If the first expression evaluates to true in a boolean context,
 * this macro evaluates and returns the result of the second
 * expression, otherwise it evaluates all of the remaining expression
 * and returns the return value of the last one.
 * 
 * @return The return value of either the second or last expression, or
 *         nil if testExpression evaluates to false and there are no
 *         remaining expressions to evaluate.
 * 
 * @tested
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
 * Executes the rest of the arguments if the first argument
 * is true.
 * 
 * @return The return value of the last expression.
 * 
 * @tested
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
 * Performs a logical negation on the given value.
 * 
 * @tested
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
 * TODO: Test me
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
 * TODO: Test me
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
 * TODO: Test me
 */
defmacro("equal", function () {
	if (arguments.length < 2) {
		throw new Error("(equal) requires at least 2 arguments (got "
			+ arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return equal(a, b);
	});
});

/**
 * TODO: Test me
 */
defmacro("not-equal", function () {
	if (arguments.length < 2) {
		throw new Error("(not-equal) requires at least 2 arguments (got "
			+ arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return !equal(a, b);
	});
});

/**
 * TODO: Test me
 */
defmacro("==", function () {
	if (arguments.length < 2) {
		throw new Error("(==) requires at least 2 arguments (got "
			+ arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a == b;
	});
});

/**
 * TODO: Test me
 */
defmacro("===", function () {
	if (arguments.length < 2) {
		throw new Error("(===) requires at least 2 arguments (got "
			+ arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a === b;
	});
});

/**
 * TODO: Test me
 */
defmacro("!=", function () {
	if (arguments.length < 2) {
		throw new Error("(!=) requires at least 2 arguments (got "
			+ arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a != b;
	});
});

/**
 * TODO: Test me
 */
defmacro("!==", function () {
	if (arguments.length < 2) {
		throw new Error("(!==) requires at least 2 arguments (got "
			+ arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a !== b;
	});
});

/**
 * TODO: Test me
 * 
 * Examples:
 *    * (< x y)
 *    * (< -1 0 1 2 3)
 */
defmacro("<", function () {
	if (arguments.length < 2) {
		throw new Error("(<) requires at least 2 arguments (got "
			+ arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a < b;
	});
});

/**
 * TODO: Test me
 *
 * Examples:
 *    * (> x y)
 *    * (> 3 2 1 0 -1)
 */
defmacro(">", function () {
	if (arguments.length < 2) {
		throw new Error("(>) requires at least 2 arguments (got "
			+ arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a > b;
	});
});

/**
 * TODO: Test me
 *
 * Examples:
 *    * (<= x y)
 *    * (<= 1 1 2 3 4)
 */
defmacro("<=", function () {
	if (arguments.length < 2) {
		throw new Error("(<=) requires at least 2 arguments (got "
			+ arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a <= b;
	});
});

/**
 * TODO: Test me
 *
 * Examples:
 *    * (>= x y)
 *    * (>= 4 3 2 2 1)
 */
defmacro(">=", function () {
	if (arguments.length < 2) {
		throw new Error("(>=) requires at least 2 arguments (got "
			+ arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a >= b;
	});
});

/**
 * Returns true if the given values === true.
 * 
 * TODO: Test me
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
 * Returns true if the given values === false.
 * 
 * TODO: Test me
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
 * Returns true if the given values === null.
 * 
 * TODO: Test me
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
 * TODO: Test me
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
 * Returns true if the given values are strings.
 * 
 * TODO: Test me
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
 * Returns true if the given values are numbers.
 * 
 * TODO: Test me
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
 * Returns true if the given values are booleans.
 * 
 * TODO: Test me
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
 * Returns true if the given values are functions.
 * 
 * TODO: Test me
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
 * Returns true if the given values are objects.
 * 
 * TODO: Test me
 */
defmacro("is-object", function () {
	if (arguments.length === 0) {
		throw new Error("(is-object) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return typeof(value) == "object";
	});
});
