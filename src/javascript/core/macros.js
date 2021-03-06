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
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name quote
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @returns The given argument, unevaluated.
 */
defmacro("quote", function (expression) {
	// Input validation
	assert(arguments.length === 1, "(quote) requires 1 argument (got " +
		arguments.length + ")");
	
	return expression;
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * 
 * FIXME: At some point macros will be expanded by the parser, so
 *        this will be unnecessary.
 * </pre>
 * 
 * @name backquote
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("backquote", function (expression) {
	expression = checkResolve(expression);
	expression = checkExplode(expression);
	return expression;
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name resolve
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("resolve", function (expression) {
	// Input validation
	assert(arguments.length === 1, "(resolve) requires 1 argument (got " +
		arguments.length + ")");
	
	return resolve(expression);
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name explode
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("explode", function (expression) {
	// Input validation
	assert(arguments.length === 1, "(explode) requires 1 argument (got " +
		arguments.length + ")");
	
	return [_S("explode"), expression];
});

/**
 * <pre>
 * TODO: Test me more
 * TODO: Document me
 * TODO: Add examples
 * 
 * FIXME: While this "solution" works for the limited amount of tests
 *        i've throw at it, it is FAR from elegant...in fact I might
 *        go so far as to say it makes me nauseous. That being said,
 *        I'm happy there is at least a preliminary working version.
 * </pre>
 * 
 * @tested
 * 
 * @name defmacro
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("defmacro", function (name, arglist /*, &rest */) {
	// Input validation
	assert(arguments.length >= 1, "(defmacro) requires at least 1 argument");
	assert(name instanceof Symbol, "(defmacro) requires a symbol as its first " +
		"argument (got " + toLisp(name) + ")");
	assert(arguments.length <= 1 || arglist instanceof Array, "(defmacro) " +
		"requires an Array as its second argument (got " + toLisp(arglist) + ")");
	
	arglist = arglist || [];
	
	function setargs (arglist, args, top) {
		if (top === undefined) {
			top = true;
		}
		assert(args instanceof Array, "Error while parsing arguments to macro " + toLisp(name) + ".\n" +
			"\tInvalid arguments to:\n" +
			"\t  " + toLisp(arglist) + "\n" +
			"\tExpected a list but got '" + toLisp(args) + "'");
		
		var i;
		var j = 0;
		var arg;
		for (i = 0; i < arglist.length; i++) {
			arg = arglist[i];
			j++;
			if (arg instanceof Array) {
				setargs(arg, args[i], false);
			} else {
				if (String(arg) == "&") {
					assert(i != arglist.length - 1,
						"No argument name after rest identifier");
					assert(arglist.length <= i + 2, "Unexpected arguments (" +
						arglist.slice(i+1).join(" ") + ") after rest argument");
					
					lisp.env.let(arglist[i+1], args.slice(i));
					j = args.length;
					i = args.length;
					break;
				} else {
					lisp.env.let(arg, args[i]);
				}
			}
		}
		
		assert(i == j, "Error while parsing arguments to macro " + toLisp(name) + ".\n" +
			"\tNot enough arguments to:\n" +
			"\t  " + toLisp(arglist) + "\n" +
			"\tGot:\n" +
			"\t  " + toLisp(args));
	}
	
	var env  = new Env(lisp.env);
	var args = argsToArray(arguments);
	
	// FIXEME: This is a way better way of "unquoting" an expression:
	//         http://www.gigamonkeys.com/book/macros-defining-your-own.html#generating-the-expansion
	
	var documentation = null;
	if (args.length >= 3 && typeof(args[2]) === "string") {
		documentation = args[2];
	}
	
	return (function (env, args) {
		var macro = new Macro(function () {
			if (args.length < 3) {
				return null; // This macro does nothing
			}
			args = deepCopyArray(args);
			var tempEnv = lisp.env;
			var ret = null;
			
			try {
				lisp.env = new Env(lisp.env);
				//lisp.env.let("this", this); // Is this necessary or even wanted?
				lisp.env.let("arguments", arguments);
				setargs(arglist, argsToArray(arguments));
				var body = deepCopyArray(args.slice(2));
				for (var i = 0; i < body.length; i++) {
					ret = resolve(body[i]);
				}
				ret = resolve(ret); // FIXME: This extra resolve is a hack
			} finally {
				lisp.env = tempEnv;
			}
			
			return ret;
		});
		macro.documentation = documentation;
		lisp.env.set(name, macro);
		return macro;
	})(env, args);
});

/**
 * <pre>
 * Creates an anonymous function with the first (required)
 * expression as its arglist and which executes the rest of the
 * expressions when called.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name lambda
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @returns The created function.
 */
defmacro("lambda", function (arglist /*, &rest */) {
	// Input validation
	assert(arguments.length === 0 || arglist instanceof Array, "(lambda) requires " +
		"a list as its first expression (got " + toLisp(arglist) + ")");
	
	arglist = arglist || [];
	
	var env  = new Env(lisp.env);
	var args = argsToArray(arguments);
	var restname;
	
	for (i = 0; i < arglist.length; i++) {
		if (arglist[i] == "&") {
			assert(i != arglist.length - 1,
				"No argument name after rest identifier");
			assert(!(arglist.length > i + 2), "Unexpected arguments (" +
				arglist.slice(i+2).join(" ") + ") after rest argument");
			restname = arglist[i+1];
			break;
		}
	}
	
	return (function (env, args) {
		var body = args.slice(1);
		var documentation = null;
		
		if (body.length >= 1 && typeof(body[0]) === "string") {
			documentation = body[0];
		}
		
		var func = function () {
			var largs = argsToArray(arguments);
			var tempEnv = lisp.env;
			var ret = null;
			
			try {
				lisp.env = new Env(env);
				lisp.env.let("this", this);
				lisp.env.let("arguments", arguments);
			
				var i, j;
				var str;
				var index;
				var type;
				var typestr;
				var argname;
				var value;
				var optional = false;
				
				for (i = 0, j = 0; i < arglist.length; i++, j++) {
					argname = String(arglist[i]);
					
					if (argname == "&") {
						// The rest of the arguments should be collected into a list
						lisp.env.let(restname, largs.slice(j));
						i = arglist.length;
						j = largs.length;
						break;
					} else if (argname == "&opt") {
						// The rest of the arguments are optional
						optional = true;
						j--;
						continue;
					} else {
						index = argname.indexOf(":");
						type = undefined;
						typestr = undefined;
						
						if (index >= 0) {
							str = argname.slice(0, index);
							typestr = argname.slice(index+1);
							argname = str;
							if (typestr.length > 0) {
								type = lisp.eval(typestr);
							}
						}
						
						if (j < largs.length) {
							value = largs[j];
							if (value !== undefined || !optional) {
								if (type instanceof Keyword) {
									if (typeof(value) != String(type)) {
										throw new ArgumentError("Got invalid argument for " +
											toLisp(argname) + ". Expected a value of type " +
											toLisp(String(type)) + " (got " + toLisp(value) + ").");
									}
								} else if (typeof(type) === "function") {
									if (!(value instanceof type)) {
										throw new ArgumentError("Got invalid argument for " +
											toLisp(argname) + ". Expected an instance of " +
											typestr + " (got " + toLisp(value) + ").");
									}
								} else if (type !== undefined) {
									throw new ArgumentError("Invalid type specifier: " + toLisp(type));
								}
							}
							lisp.env.let(argname, value);
						} else if (optional) {
							lisp.env.let(argname, undefined);
						} else {
							throw new ArgumentError("Missing argument " + toLisp(argname) +
								" in function call.");
						}
						
					}
				}
				
				// Uncommenting this will throw an ArgumentError if the function
				// receives _too many_ arguments.
				// if (j < largs.length) {
				// 	throw new ArgumentError("Too many arguments passed to function " +
				// 		"with arglist " + toLisp(arglist));
				// }
				
				for (i = 0; i < body.length; i++) {
					ret = resolve(body[i]);
				}
			} finally {
				lisp.env = tempEnv;
			}
			
			return ret;
		};
		
		func.documentation = documentation;
		return func;
	})(env, args);
});

/**
 * <pre>
 * Defines a function. This is shorthand for (setq name (lambda ...)).
 * 
 * TODO: Test me
 * TODO: Add examples
 * 
 * FIXME: Define this is lisp.
 * </pre>
 * 
 * @name defun
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("defun", function (name, arglist /*, ... */) {
	// Input validation
	assert(arguments.length > 0, "(defun) requires at least 1 argument.");
	assert(name instanceof Symbol, "(defun) requires a symbol as its first argument " +
		"(got " + toLisp(name) + ")");
	assert(arguments.length === 1 || arglist instanceof Array, "(defun) requires " +
		"a list of symbols as its second expression (got " + toLisp(arglist) + ")");
	
	var body = argsToArray(arguments).slice(2);
	var lambda = resolve([_S("lambda"), arglist].concat(body));
	
	lisp.env.set(name, lambda);
	return lambda;
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
 *     >> (try)
 *     => nil
 * 
 * @example Empty catch block (silences the error)
 *     >> (try
 *            (throw (new Error))
 *          (catch))
 *     => nil
 * 
 * @example Multiple expressions with a full catch block
 *     >> (try
 *            (print "This will print")
 *            (throw (new Error "This cuts the expression short"))
 *            (print "This will not print")
 *          (catch (e)
 *            (format t "This will print when the error is thrown: %s" e)))
 *       This will print
 *       This will print when the error is thrown: Error: This cuts the expression short
 *       => nil
 * 
 * @example try/finally
 *     >> (try
 *            (throw (new Error))
 *          (finally
 *            (print "This always runs")))
 *     This always runs
 *     Error
 * 
 * @example try/catch/finally
 *     >> (try
 *            "hello" ; This will get returned because it's the last evaluated expression
 *            (throw (new Error))
 *          (catch (e)
 *            (print "This will get called"))
 *          (finally
 *            (print "This always runs")))
 *     This will get called
 *     This always runs
 *     => "hello" ; The error is caught, so there is a return value
 * 
 * @example catch/finally symbols as keywords
 *     >> (try
 *            (throw (new Error))
 *          (:catch) ; This will work
 *          (:finally)) ; And so will this
 *     => nil
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
	} catch (error) {
		// Evaluate the catch expression if there is one.
		if (catchExpression) {
			if (catchExpression.length > 0) {
				var tempEnv = lisp.env;
				
				try {
					lisp.env = new Env(lisp.env);
					
					if (catchExpression.length >= 2) { // If there is an argument list
						var catchArglist = catchExpression[1];
					
						assert(catchArglist instanceof Array, "(catch) expression's first " +
							"argument must be an Array (got " + toLisp(catchArglist) + ")");
						assert(catchArglist.length <= 1, "(catch) requires an arglist with " +
							"either 1 argument or no arguments (got " + catchArglist.length + ")");
					
						if (catchArglist.length === 1) {
							assert(catchArglist[0] instanceof Symbol, "(catch) arglist " +
								"requires a symbol as its argument, or nothing (got " +
								toLisp(catchArglist[0]) + ")");
							lisp.env.let(catchArglist[0], error);
						}
					}
					
					var expressions = catchExpression.slice(2);
					for (i = 0; i < expressions.length; i++) {
						resolve(expressions[i]);
					}
				} finally {
					lisp.env = tempEnv;
				}
			}
		} else {
			// If there is no catch expression, throw the error for something
			// else to catch it (or not).
			throw e;
		}
	} finally {
		// Evaluate all expressions in the finally expression if there
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
	// Input validation
	assert(arguments.length >= 2, "(funcall) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	// Grab the object.
	object = resolve(object);
	
	// Make sure we can get a string dotpath from the supplied argument.
	if (dotpath instanceof Symbol || dotpath instanceof Keyword) {
		dotpath = dotpath.value;
	}
	
	assert(typeof(dotpath) === "string", "Unknown key in " +
		"(funcall): " + String(dotpath));
	
	// Resolve the object down to the second-to-last part of the dot path.
	var parts = String(dotpath).split(".");
	for (var i = 0; i < parts.length-1; i++) {
		object = object[parts[i]];
	}
	
	// Make sure what's being "called" is actually a function.
	var funckey = parts[parts.length-1];
	
	assert(typeof(object[funckey]) === "function", String(dotpath) +
		" on " + object + " is not a function");
	
	var args = argsToArray(arguments).slice(2).map(resolve);
	return object[funckey].apply(object, args);
});

/**
 * <pre>
 * TODO: Document me
 * TODO: Add examples
 * 
 * FIXME: This method sucks (actually, env.let sucks)
 * </pre>
 * 
 * @tested
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
	
	var tempEnv = lisp.env;
	var ret = null;
	
	try {
		lisp.env = new Env(lisp.env);
		args = args.slice(1);
	
		for (i = 0; i < letset.length; i++) {
			var symbol = letset[i][0];
			var value = resolve(letset[i][1]);
			lisp.env.let(symbol, value);
		}
	
		for (i = 0; i < args.length; i++) {
			ret = resolve(args[i]);
		}
	} finally {
		lisp.env = tempEnv;
	}
	
	return ret;
});

/**
 * <pre>
 * Sets the value of the variable represented by the given symbol to
 * the given value.
 * </pre>
 * 
 * @tested
 * 
 * @name setq
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @param {symbol} symbol
 *     The variable whose value is to be set.
 * @param {mixed} value
 *     The value to set to the variable represented by the given symbol.
 * 
 * @example Basic usage
 *     >> x
 *     => undefined
 *     >> (setq x 1)
 *     => 1
 *     >> x
 *     => 1
 * 
 * @example Using the return value
 *     >> (let ((last nil))
 *     ..   (try
 *     ..     (dolist (item '(1 2 3))
 *     ..       (when (> (setq last item) 1)
 *     ..         (throw)))
 *     ..   (:catch))
 *     ..   last)
 *     => 2
 * 
 * @example Setting a property on an object
 *     >> (let ((obj (object :one (object :two 3))))
 *     ..   (print obj.one.two)
 *     ..   (setq obj.one.two 10)
 *     ..   (print obj.one.two))
 *     3
 *     10
 *     => nil
 */
defmacro("setq", function (symbol, expression) {
	assert(arguments.length === 2, "(setq) requires 2 arguments (got " +
		arguments.length + ")");
	assert(symbol instanceof Symbol, "(setq) requires a symbol as its first " +
		"argument (got " + toLisp(symbol) + ")");
	var value = resolve(expression);
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
 * 
 * @param {[mixed]} expressions
 *     The expressions to be evaluated.
 * @rest expressions
 */
defmacro("progn", function (/* &expressions */) {
	var ret = null;
	for (var i = 0; i < arguments.length; i++) {
		ret = resolve(arguments[i]);
	}
	return ret;
});

/**
 * <pre>
 * Takes a list of lists of expressions. Evaluates the first expression
 * of each top-level list until one evaluates to true, then it evaluates
 * the rest of the expressions in that list only, returning the return
 * value of its last expression. If no list contains a first expression
 * that evaluates to true, cond returns nil.
 * </pre>
 * 
 * @tested
 * 
 * @name cond
 * @lisp
 * @function
 * @macro
 * @member lisp.macros
 * 
 * @returns The value of the last evaluated expression, or nil.
 * 
 * @param {[Array]} rest
 *     A list of lists of expressions to be evaluated (until one
 *     contains a first expression that evaluates to true).
 * @rest rest
 * 
 * @example Empty cond
 *     >> (cond)
 *     => nil
 * 
 * @example Default expression
 *     >> (cond ((== 1 2) "this wont get evaluated, at least not in this universe")
 *              (nil "neither will this")
 *              (t "but this will")) ;; Acts like a "default" expression
 *     => "but this will"
 * 
 * @example Short circuiting
 *     >> (cond (t "this always gets avaluated")
 *              (t "this will never get evaluated"))
 *     => "this always gets avaluated"
 */
defmacro("cond", function (/* &rest */) {
	for (var i = 0; i < arguments.length; i++) {
		var clause = arguments[i];
		
		assert(clause instanceof Array, "(cond) clause must be a list " +
			"(got " + toLisp(clause) + ")");
		assert(clause.length > 0, "(cond) clauses must contain an " +
			"expression to evaluate");
		
		var condition = clause[0];
		if (!!resolve(condition)) {
			var ret = null;
			for (var j = 1; j < clause.length; j++) {
				ret = resolve(clause[j]);
			}
			return ret;
		}
	}
	return null;
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add Examples
 * TODO: Add support for 'otherwise
 * </pre>
 * 
 * @name switch
 * @lisp
 * @function
 * @macro
 * @member lisp.macros
 */
defmacro("switch", function (valueExpression /*, &rest */) {
	assert(arguments.length >= 1, "(switch) requires at least one argument");
	var value = resolve(valueExpression);
	var rest = argsToArray(arguments).slice(1);
	var ret = null;
	var clause;
	var key;
	
	for (var i = 0; i < rest.length; i++) {
		clause = rest[i];
		assert(clause instanceof Array, "(switch) requires an array for each " +
			"clause (got " + toLisp(clause) + ")");
		assert(clause.length >= 1, "(switch) requires each clause list have at " +
			"least one value");
		key = clause[0];
		if (equal(value, key)) {
			var expressions = clause.slice(1);
			for (var j = 0; j < expressions.length; j++) {
				ret = resolve(expressions[j]);
			}
			break;
		}
	}
	
	return ret;
});

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
	// Input validation
	assert(arguments.length >= 2, "(if) requires at least 2 arguments (got " +
		arguments.length + ")");
	
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
 * TODO: Document me
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
defmacro("when", function (ifExpression /*, &rest */) {
	// Input validation
	assert(arguments.length >= 1, "(when) requires at least 1 argument");
	
	var ret = null;
	
	if (!!resolve(ifExpression)) {
		var args = argsToArray(arguments);
		for (var i = 1; i < args.length; i++) {
			ret = resolve(args[i]);
		}
	}
	
	return ret;
});

/**
 * <pre>
 * Executes the rest of the arguments if the first argument
 * is false.
 * 
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name unless
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @returns The return value of the last expression.
 */
defmacro("unless", function (ifNotExpression /*, &rest */) {
	// Input validation
	assert(arguments.length >= 1, "(unless) requires at least 1 argument");
	
	var ret = null;
	
	if (!resolve(ifNotExpression)) {
		var args = argsToArray(arguments);
		for (var i = 1; i < args.length; i++) {
			ret = resolve(args[i]);
		}
	}
	
	return ret;
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
	// Input validation
	assert(arguments.length > 0, "(not) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return !value;
	});
});

/**
 * <pre>
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @tested
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
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @tested
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
 * Returns the first expression that evaluates to true in a boolean
 * context, otherwise returns null.
 * 
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name ||
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("||", function () {
	for (var i = 0; i < arguments.length; i++) {
		var ret = resolve(arguments[i]);
		if (ret) {
			return ret;
		}
	}
	
	return null;
});

/**
 * <pre>
 * If the given symbol resolves to true, simply returns that value,
 * otherwise evaluates the given expression, sets its value to the
 * symbol on the env, and returns it.
 * 
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name ||=
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("||=", function (symbol, expression) {
	assert(arguments.length === 2, "(||=) requires 2 arguments (got " +
		arguments.length + ")");
	assert(symbol instanceof Symbol, "(||=) requires a symbol as its first " +
		"argument (got " + toLisp(symbol) + ")");
	
	// If symbol already evaluates to true, don't set it, and just
	// return its current value.
	var ret = resolve(symbol);
	if (ret) {
		return ret;
	}
	
	// If symbol evaluates to fales, set it to the value of the given
	// expression, and return that value.
	ret = resolve(expression);
	lisp.env.set(symbol, ret);
	return ret;
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
	// Input validation
	assert(arguments.length >= 2, "(equal) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
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
	// Input validation
	assert(arguments.length >= 2, "(not-equal) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	return comparator(arguments, function (a, b) {
		return !equal(a, b);
	});
});

/**
 * <pre>
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name ==
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("==", function () {
	// Input validation
	assert(arguments.length >= 2, "(==) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	return comparator(arguments, function (a, b) {
		return a == b;
	});
});

/**
 * <pre>
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name ===
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("===", function () {
	// Input validation
	assert(arguments.length >= 2, "(===) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	return comparator(arguments, function (a, b) {
		return a === b;
	});
});

/**
 * <pre>
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name !=
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("!=", function () {
	// Input validation
	assert(arguments.length >= 2, "(!=) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	return comparator(arguments, function (a, b) {
		return a != b;
	});
});

/**
 * <pre>
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name !==
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("!==", function () {
	// Input validation
	assert(arguments.length >= 2, "(!==) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	return comparator(arguments, function (a, b) {
		return a !== b;
	});
});

/**
 * <pre>
 * TODO: Document me
 * </pre>
 * 
 * @tested
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
 *     => f
 */
defmacro("<", function () {
	// Input validation
	assert(arguments.length >= 2, "(<) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	return comparator(arguments, function (a, b) {
		return a < b;
	});
});

/**
 * <pre>
 * TODO: Document me
 * </pre>
 * 
 * @tested
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
 *     => f
 */
defmacro(">", function () {
	// Input validation
	assert(arguments.length >= 2, "(>) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	return comparator(arguments, function (a, b) {
		return a > b;
	});
});

/**
 * <pre>
 * TODO: Document me
 * </pre>
 * 
 * @tested
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
 *     => f
 */
defmacro("<=", function () {
	// Input validation
	assert(arguments.length >= 2, "(<=) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	return comparator(arguments, function (a, b) {
		return a <= b;
	});
});

/**
 * <pre>
 * TODO: Document me
 * </pre>
 * 
 * @tested
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
 *     => f
 */
defmacro(">=", function () {
	// Input validation
	assert(arguments.length >= 2, "(>=) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	return comparator(arguments, function (a, b) {
		return a >= b;
	});
});

/**
 * <pre>
 * Returns true if the given values === true.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name is-true
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-true", function () {
	// Input validation
	assert(arguments.length > 0, "(is-true) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return value === true;
	});
});

/**
 * <pre>
 * Returns true if the given values === false.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name is-false
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-false", function () {
	// Input validation
	assert(arguments.length > 0, "(is-false) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return value === false;
	});
});

/**
 * <pre>
 * Returns true if the given values === null.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name is-null
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-null", function () {
	// Input validation
	assert(arguments.length > 0, "(is-null) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return value === null;
	});
});

/**
 * <pre>
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name is-undefined
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-undefined", function () {
	// Input validation
	assert(arguments.length > 0, "(is-undefined) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return value === undefined;
	});
});

/**
 * <pre>
 * Returns true if the given values are strings.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name is-string
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-string", function () {
	// Input validation
	assert(arguments.length > 0, "(is-string) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return typeof(value) == "string";
	});
});

/**
 * <pre>
 * Returns true if the given values are numbers.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name is-number
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-number", function () {
	// Input validation
	assert(arguments.length > 0, "(is-number) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return typeof(value) == "number";
	});
});

/**
 * <pre>
 * Returns true if the given values are booleans.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name is-boolean
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-boolean", function () {
	// Input validation
	assert(arguments.length > 0, "(is-boolean) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return typeof(value) == "boolean";
	});
});

/**
 * <pre>
 * Returns true if the given values are functions.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name is-function
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-function", function () {
	// Input validation
	assert(arguments.length > 0, "(is-function) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return typeof(value) == "function";
	});
});

/**
 * <pre>
 * Returns true if the given values are objects.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name is-object
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-object", function () {
	// Input validation
	assert(arguments.length > 0, "(is-object) requires at least 1 argument");
	
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
	// Input validation
	assert(arguments.length > 0, "(is-array) requires at least 1 argument");
	
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
	// Input validation
	assert(arguments.length > 0, "(is-list) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return value instanceof Array;
	});
});

/**
 * <pre>
 * Returns true if the given values are Symbols.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-symbol
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-symbol", function () {
	// Input validation
	assert(arguments.length > 0, "(is-symbol) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return value instanceof Symbol;
	});
});

/**
 * <pre>
 * Returns true if the given values are Keywords.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-keyword
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-keyword", function () {
	// Input validation
	assert(arguments.length > 0, "(is-keyword) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return value instanceof Keyword;
	});
});

/**
 * <pre>
 * Returns true if the given values are Macros.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-macro
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-macro", function () {
	// Input validation
	assert(arguments.length > 0, "(is-macro) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return value instanceof Macro;
	});
});

/**
 * 
 */
defmacro("do", function (vardefs, end /*, &rest */) {
	// (do (variable-definition*)
	//     (end-test-form result-form*)
	//   statement*)
	// variable-definition = (var init-form step-form)
	var body = argsToArray(arguments).slice(2);
	var i;
	var def;
	
	assert(vardefs instanceof Array, "(do) requires a list as " +
		"its first argument (got " + toLisp(vardefs) + ")");
	
	for (i = 0; i < vardefs.length; i++) {
		def = vardefs[i];
		assert(def instanceof Array, "(do) requires lists as " +
			"variable definitions (got " + toLisp(def) + ")");
		assert(def.length >= 2 && def.length <= 3, "(do) variable " +
			"definitions must have 2 or 3 elements (got " +
			toLisp(def) + ")");
	}
	
	assert(end instanceof Array, "(do) requires a list as its " +
		"second argument (got " + toLisp(end) + ")");
	assert(end.length >= 1, "(do) requires its 'end' expression " +
		"to have at least 1 element (the 'test' expression)");
	
	return withNewEnv(function () {
		for (i = 0; i < vardefs.length; i++) {
			def = vardefs[i];
			lisp.env.let(def[0], resolve(def[1])); // 0 = name, 1 = init form
		}
		
		while (!resolve(end[0])) { // 0 = end test
			// Evaluate the body expressions.
			for (i = 0; i < body.length; i++) {
				resolve(body[i]);
			}
			
			// Evaluate the step forms.
			for (i = 0; i < vardefs.length; i++) {
				def = vardefs[i];
				var step = def.length === 2 ? def[1] : def[2]; // 2 = step form
				lisp.env.set(def[0], resolve(step));
			}
		}
		
		var resultForms = end.slice(1);
		var ret = null;
		for (i = 0; i < resultForms.length; i++) {
			ret = resolve(resultForms[i]);
		}
		return ret;
	});
});

/**
 * <pre>
 * An expression for basic iteration over a list.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * 
 * FIXME: Define this using (defmacro) in macros.lisp
 * </pre>
 * 
 * @name dolist
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("dolist", function (arglist /*, ... */) {
	// Input validation
	assert(arguments.length > 0, "(dolist) requires at least 1 argument " +
		"(got " + arguments.length + ")");
	assert(arglist instanceof Array, "(dolist) got invalid argument " +
		"list: " + toLisp(arglist));
	assert(arglist.length >= 2 && arglist.length <= 3, "(dolist) got " +
		"invalid argument list. Requires at least 2 arguments and no " +
		"more than 3 (got " + arglist.length + ")");
	
	var itemName = arglist[0];
	var list     = resolve(arglist[1]);
	
	assert(itemName instanceof Symbol, "(dolist) got invalid argument " +
		"list. First argument must be a symbol (got " +
		toLisp(itemName) + ")");
	assert(list instanceof Array, "(dolist) got invalid argument list. " +
		"Second argument must be a list (got " + toLisp(list) + ")");
	
	var tempEnv = lisp.env;	
	var ret = null;
	
	try {
		lisp.env = new Env(lisp.env);
		var body = argsToArray(arguments).slice(1);
		for (var i = 0; i < list.length; i++) {
			lisp.env.let(itemName, list[i]);
			for (var j = 0; j < body.length; j++) {
				ret = resolve(body[j]);
			}
		}
	} finally {
		lisp.env = tempEnv;
	}
	
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
 * </pre>
 * 
 * @name foreach
 * @lisp
 * @function
 * @macro
 * @member lisp.macros
 * 
 * @example Basic usage
 *     >> (let ((obj (object :one 1 :two 2 :three 3)))
 *          (foreach (item obj)
 *            (let ((key (first item)))
 *              (print (first item)))))
 *     one
 *     two
 *     three
 *     => nil
 * 
 * @example Explode the item
 *     >> (let ((obj (object :one 1 :two 2 :three 3)))
 *          (foreach ((key value) obj)
 *            (format t "%l: %l" key value)))
 *     "one": 1
 *     "two": 2
 *     "three": 3
 *     => nil
 */
defmacro("foreach", function (arglist /*, &rest */) {
	// Input validation
	assert(arguments.length > 0, "(foreach) requires at least 1 argument");
	assert(arglist instanceof Array, "(foreach) requires a list as its " +
		"first argument (got " + toLisp(arglist) + ")");
	assert(arglist.length === 2, "(foreach) got invalid argument list. " +
		"Requires 2 arguments (got " + arglist.length + ")");
	
	var item   = arglist[0];
	var object = resolve(arglist[1]);
	
	assert(item instanceof Symbol ||
		   (item instanceof Array && item.length === 2 &&
	        item[0] instanceof Symbol && item[1] instanceof Symbol),
		"(foreach) got invalid argument list. First argument must " +
		"be a symbol or list with 2 symbols (got " + toLisp(item) + ")");
	assert(object instanceof Object, "(foreach) got invalid argument list. " +
		"Second argument must be an object (got " + toLisp(object) + ")");
	
	var body = argsToArray(arguments).slice(1);
	
	var tempEnv = lisp.env;
	var ret = null;
	
	var itemName;
	var keyName;
	var valName;
	
	if (item instanceof Array) {
		keyName = item[0];
		valName = item[1];
	} else {
		itemName = item;
	}
	
	try {
		lisp.env = new Env(lisp.env);
		var value;
		for (var key in object) {
			value = object[key];
			if (itemName) {
				lisp.env.let(itemName, [key, value]);
			} else {
				lisp.env.let(keyName, key);
				lisp.env.let(valName, value);
			}
			for (var i = 0; i < body.length; i++) {
				ret = resolve(body[i]);
			}
		}
	} finally {
		lisp.env = tempEnv;
	}
	
	return ret;
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add more examples
 * </pre>
 * 
 * @name char
 * @lisp
 * @function
 * @macro
 * @member lisp.macros
 */
defmacro("char", function (symbol) {
	// Input validation
	assert(arguments.length === 1, "(char) requires 1 argument (got " +
		arguments.length + ")");
	assert(symbol instanceof Symbol, "(char) requires a symbol as its " +
		"argument (got " + toLisp(symbol) + ")");
	
	return _char_table[String(symbol)];
});

/**
 * <pre>
 * TODO: Test me more
 * TODO: Document me
 * TODO: Add more examples
 * </pre>
 * 
 * @tested
 * 
 * @name collect
 * @lisp
 * @function
 * @macro
 * @member lisp.macros
 * 
 * @example Basic usage
 *     >> (let ((obj (object :name "js-lisp" :age 0)))
 *          (collect (item obj)
 *            (is-number (second item))))
 *     => (("age" 0)) ; Returns every (key value) pair where the last 
 *                    ; expression of the body evaluates to true.
 */
var _macro_collect; // Defined in /src/lisp/macros.lisp

/**
* <pre>
* TODO: Test me more
* TODO: Document me
* TODO: Add more examples
* </pre>
* 
* @tested
* 
* @name inc
* @lisp
* @function
* @macro
* @member lisp.macros
 */
defmacro("inc", function (varName, amountName) {
	assert(arguments.length >= 1, "(inc) requires at least 1 argument");
	
	var amount = (amountName === undefined) ? 1 : resolve(amountName);
	var oldValue = lisp.env.get(varName);
	var newValue = oldValue + amount;
	lisp.env.set(varName, newValue);
	return newValue;
});

/**
* <pre>
* TODO: Test me more
* TODO: Document me
* TODO: Add more examples
* </pre>
* 
* @tested
* 
* @name dec
* @lisp
* @function
* @macro
* @member lisp.macros
 */
defmacro("dec", function (varName, amountName) {
	assert(arguments.length >= 1, "(dec) requires at least 1 argument");
	
	var amount = (amountName === undefined) ? 1 : resolve(amountName);
	var oldValue = lisp.env.get(varName);
	var newValue = oldValue - amount;
	lisp.env.set(varName, newValue);
	return newValue;
});

// FIXME: Put this somewhere else.
var Generator = Class.extend({
	init: function (callable) {
		this.callable = callable;
	},
	call: function () {
		return this.callable.call(null);
	}
});

// FIXME: Put this somewhere else.
var StopIteration = Class.extend(); // ({}) ?

/**
 * <pre>
 * Iteration for arrays, strings and generators (and other
 * sequences as well).
 * 
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name for
 * @lisp
 * @function
 * @macro
 * @member lisp.macros
 */
defmacro("for", function (arglist /*, &rest */) {
	assert(arguments.length >= 1, "(for) requires at least 1 argument");
	
	var itemName = arglist[0];
	var iterable = resolve(arglist[1]);
	var rest = argsToArray(arguments).slice(1);
	var i;
	var ret;
	
	function doIteration (index, item) {
		withNewEnv(function () {
			if (rest.length === 0) {
				return null;
			}

			lisp.env.let(itemName, item);
			ret = null;
			for (var i = 0; i < rest.length; i++) {
				ret = resolve(rest[i]);
			}
			return ret;
		});
	}
	
	ret = null;
	
	if (iterable instanceof Generator) {
		// The iterable object is a generator
		try {
			for (i = 0; true; i++) {
				ret = doIteration(i, iterable.call());
			}
		} catch (e) {
			if (!(e instanceof StopIteration)) {
				throw e;
			}
		}
	} else if (iterable.hasOwnProperty("length")) {
		// The iterable object is some kind of sequence
		for (i = 0; i < iterable.length; i++) {
			ret = doIteration(i, iterable[i]);
		}
	} else { // Don't know how to iterate over this
		throw new Error("(for) received an object it does not know how to iterate " +
			"over: " + toLisp(iterable));
	}
	
	return ret;
});

/**
 * <pre>
 * Emulates the goto functionality of languages like c. Takes
 * a list of lists, each one starting with a label and containing
 * an arbitrary number of expressions. Evalualtes the expressions
 * in order, except when (goto :label) is called.
 * 
 * FIXME: This macro works but is large in gross.
 * 
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name labels
 * @lisp
 * @function
 * @macro
 * @member lisp.macros
 * 
 * @example Basic (slightly contrived) usage
 *     >> (let ((i))
 *     ..   (labels
 *     ..     (:one ;; Setup
 *     ..       (setq i -1))
 *     ..     (:two ;; Increment
 *     ..       (inc i))
 *     ..     (:three ;; Test
 *     ..       (print i)
 *     ..       (when (< i 3)
 *     ..         (goto :two)))
 *     ..     (:four ;; Fin
 *     ..       (format t "Done at %l" i))))
 *     0
 *     1
 *     2
 *     3
 *     Done at 3
 *     => nil
 */
defmacro("labels", function (/* &rest */) {
	if (arguments.length === 0) {
		return null;
	}
	
	var args = argsToArray(arguments);
	
	var labels = args.map(function (arg) {
		assert(arg.length >= 1, "(labels) label must have at least a label");
		return {
			label: arg[0],
			expressions: arg.slice(1)
		};
	});
	
	var exprIndex = -1;
	var labelIndex = 0;
	var expressions = labels[labelIndex].expressions;
	
	var gotoFunc = function (label) {
		exprIndex = -1;
		if (label === null) {
			// (goto nil) is how you return from a (labels) expression.
			labelIndex = labels.length;
			return;
		}
		for (labelIndex = 0; labelIndex < labels.length; labelIndex++) {
			if (equal(labels[labelIndex].label, label)) {
				expressions = labels[labelIndex].expressions;
				return;
			}
		}
		throw new Error(toLisp(label) + " is not a valid label");
	};
	
	withNewEnv(function () {
		lisp.env.let("goto", gotoFunc);
		
		while (exprIndex < expressions.length &&
		       labelIndex < labels.length) {
			exprIndex++;
			resolve(expressions[exprIndex]);
			if (exprIndex >= expressions.length) {
				exprIndex = -1;
				labelIndex++;
				if (!labels[labelIndex]) {
					break;
				}
				expressions = labels[labelIndex].expressions;
			}
		}
	});
	
	return null;
});
