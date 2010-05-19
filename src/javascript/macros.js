var MACROS = {
	"lambda": new Macro(function () {
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
	}),
	
	"defun": new Macro(function () {
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
	}),
	
	"let": new Macro(function () {
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
	}),
	
	"setq": new Macro(function () {
		var args = argsToArray(arguments);
		var symbol = args[0];
		var value  = resolve(args[1]);
		lisp.env.set(symbol, value);
	}),
	
	"or": new Macro(function () {
		if (arguments.length === 0) {
			return false;
		}
		for (var i = 0; i < arguments.length; i++) {
			if (resolve(arguments[i])) {
				return true;
			}
		}
		return false;
	}),
	
	"and": new Macro(function () {
		if (arguments.length === 0) {
			return false;
		}
		for (var i = 0; i < arguments.length; i++) {
			if (!resolve(arguments[i])) {
				return false;
			}
		}
		return true;
	}),
	
	"==": new Macro(function () {
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
	}),
	
	"===": new Macro(function () {
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
	}),
	
	"!=": new Macro(function () {
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
	}),
	
	"!==": new Macro(function () {
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
	}),
	
	/**
	 * Returns true if the given values === true.
	 */
	"is-true": new Macro(function () {
		return predicate(arguments, function (value) {
			return value === true;
		});
	}),
	
	/**
	 * Returns true if the given values === false.
	 */
	"is-false": new Macro(function () {
		return predicate(arguments, function (value) {
			return value === false;
		});
	}),
	
	/**
	 * Returns true if the given values === null.
	 */
	"is-null": new Macro(function () {
		return predicate(arguments, function (value) {
			return value === null;
		});
	}),
	
	/**
	 * Returns true if the given values === undefined.
	 */
	"is-undefined": new Macro(function () {
		return predicate(arguments, function (value) {
			return value === undefined;
		});
	}),
	
	/**
	 * Returns true if the given values are strings.
	 */
	"is-string": new Macro(function () {
		return predicate(arguments, function (value) {
			return typeof(value) == "string";
		});
	}),
	
	/**
	 * Returns true if the given values are numbers.
	 */
	"is-number": new Macro(function () {
		return predicate(arguments, function (value) {
			return typeof(value) == "number";
		});
	}),
	
	/**
	 * Returns true if the given values are booleans.
	 */
	"is-boolean": new Macro(function () {
		return predicate(arguments, function (value) {
			return typeof(value) == "boolean";
		});
	}),
	
	/**
	 * Returns true if the given values are functions.
	 */
	"is-function": new Macro(function () {
		return predicate(arguments, function (value) {
			return typeof(value) == "function";
		});
	}),
	
	/**
	 * Returns true if the given values are objects.
	 */
	"is-object": new Macro(function () {
		return predicate(arguments, function (value) {
			return typeof(value) == "object";
		});
	})
};
