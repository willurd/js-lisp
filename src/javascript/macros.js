/**
 * Returns an anonymous function.
 */
defmacro("lambda", function () {
	var env  = new Env(lisp.env);
	var args = argsToArray(arguments);
	return (function (env, args) {
		var arglist = args[0];
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
 */
defmacro("defun", function () {
	var args = argsToArray(arguments);
	var name = args[0];
	var arglist = args[1];
	var body = args.slice(2);
	
	lisp.env.set(name, function () {
		var i;
		lisp.env = new Env(lisp.env);
		for (i = 0; i < arglist.length; i++) {
			lisp.env.set(arglist[i], arguments[i]);
		}
		var ret = null;
		for (i = 0; i < body.length; i++) {
			ret = resolve(body[i]);
		}
		lisp.env = lisp.env.parent;
		return ret;
	});
});

/**
 * Returns the function that the given symbol points to.
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
 * 
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
 * 
 */
defmacro("setq", function () {
	var args = argsToArray(arguments);
	var symbol = args[0];
	var value  = resolve(args[1]);
	lisp.env.set(symbol, value);
	return value;
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
	for (var i = 0; i < arguments.length; i++) {
		if (!(!resolve(arguments[i]))) {
			return false;
		}
	}
	return true;
});

/**
 * 
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
 * 
 */
defmacro("and", function () {
	for (var i = 0; i < arguments.length; i++) {
		if (!resolve(arguments[i])) {
			return false;
		}
	}
	return true;
});

/**
 * 
 */
defmacro("==", function () {
	if (arguments.length < 2) {
		throw new Error("Macro '==' requires at least to arguments");
	}
	var last = resolve(arguments[0]);
	for (var i = 1; i < arguments.length; i++) {
		var arg = resolve(arguments[i]);
		if (!(arg == last)) {
			return false;
		}
		last = arg;
	}
	return true;
});

/**
 * 
 */
defmacro("===", function () {
	if (arguments.length < 2) {
		throw new Error("Macro '===' requires at least to arguments");
	}
	var last = resolve(arguments[0]);
	for (var i = 1; i < arguments.length; i++) {
		var arg = resolve(arguments[i]);
		if (!(arg === last)) {
			return false;
		}
		last = arg;
	}
	return true;
});

/**
 * 
 */
defmacro("!=", function () {
	if (arguments.length < 2) {
		throw new Error("Macro '!=' requires at least to arguments");
	}
	var last = resolve(arguments[0]);
	for (var i = 1; i < arguments.length; i++) {
		var arg = resolve(arguments[i]);
		if (!(arg != last)) {
			return false;
		}
		last = arg;
	}
	return true;
});

/**
 * 
 */
defmacro("!==", function () {
	if (arguments.length < 2) {
		throw new Error("Macro '!==' requires at least to arguments");
	}
	var last = resolve(arguments[0]);
	for (var i = 1; i < arguments.length; i++) {
		var arg = resolve(arguments[i]);
		if (!(arg !== last)) {
			return false;
		}
		last = arg;
	}
	return true;
});

/**
 * Returns true if the given values === true.
 */
defmacro("is-true", function () {
	return predicate(arguments, function (value) {
		return value === true;
	});
});

/**
 * Returns true if the given values === false.
 */
defmacro("is-false", function () {
	return predicate(arguments, function (value) {
		return value === false;
	});
});

/**
 * Returns true if the given values === null.
 */
defmacro("is-null", function () {
	return predicate(arguments, function (value) {
		return value === null;
	});
});

/**
 * 
 */
defmacro("is-undefined", function () {
	return predicate(arguments, function (value) {
		return value === undefined;
	});
});

/**
 * Returns true if the given values are strings.
 */
defmacro("is-string", function () {
	return predicate(arguments, function (value) {
		return typeof(value) == "string";
	});
});

/**
 * Returns true if the given values are numbers.
 */
defmacro("is-number", function () {
	return predicate(arguments, function (value) {
		return typeof(value) == "number";
	});
});

/**
 * Returns true if the given values are booleans.
 */
defmacro("is-boolean", function () {
	return predicate(arguments, function (value) {
		return typeof(value) == "boolean";
	});
});

/**
 * Returns true if the given values are functions.
 */
defmacro("is-function", function () {
	return predicate(arguments, function (value) {
		return typeof(value) == "function";
	});
});

/**
 * Returns true if the given values are objects.
 */
defmacro("is-object", function () {
	return predicate(arguments, function (value) {
		return typeof(value) == "object";
	});
});
