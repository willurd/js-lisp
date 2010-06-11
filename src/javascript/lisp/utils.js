function _S (name) {
	return new Symbol(name);
}

function _K (name) {
	return new Keyword(name);
}

function defun (name, func) {
	var env = (lisp && lisp.env) || ROOT_ENV;
	env.set(name, func);
}

function defmacro (name, func) {
	var env = (lisp && lisp.env) || ROOT_ENV;
	env.set(name, new Macro(func));
}

function isCallable (object) {
	return typeof(object) === "function" ||
		   (object instanceof Macro);
}

function resolve (value) {
	if (value instanceof Symbol) {
		return lisp.env.get(value);
	} else if (value instanceof Array) {
		return doSExp(value);
	} else {
		return value;
	}
}

/**
 * Recursively resolves all (resolve) expressions an
 * unevaluated expression.
 */
function checkResolve (expression) {
	if (expression instanceof Array) {
		if ((expression.length > 0) &&
			(equal(expression[0], _S("resolve")))) {
			return resolve(expression);
		}
		for (var i = 0; i < expression.length; i++) {
			expression[i] = checkResolve(expression[i]);
		}
	}
	return expression;
}

function checkExplode (expression, parent, index) {
	if (expression instanceof Array) {
		if ((expression.length > 0) &&
			(equal(expression[0], _S("explode"))) &&
			parent) {
			var list = resolve(expression[1]);
			if (!(list instanceof Array)) {
				list = [list]; // Be lenient if someone is trying to "explode" a non-list
			}
			if (list.length === 0) {
				return index;
			}
			// Insert the expressions elements into the parent
			var end = parent.slice(index+1);
			parent.splice(index, 1); // Remove the (explode) expression
			parent.splice.apply(parent, [index, list.length].concat(list));
			parent.splice.apply(parent, [parent.length, end.length].concat(end));
			return index + list.length;
		} else {
			for (var i = 0; i < expression.length; i++) {
				i = checkExplode(expression[i], expression, i);
			}
		}
	}
	return parent ? index : expression;
}

function doSExp (sexp) {
	assert(!!sexp, "doSExp got empty expression");
	
	if (sexp.length === 0) {
		// An expression with no arguments, in js-lisp, is an empty list.
		return [];
	}
	
	var first = sexp[0];
	var object = resolve(first);
	
	assert(isCallable(object), "'" + first.value + "' is not callable")
	
	var thisObject = null;
	if (first instanceof Symbol) {
		var thisObjectPath = first.value.split(".").slice(0,-1).join(".");
		thisObject = lisp.env.get(thisObjectPath);
	}
	var args = sexp.slice(1);
	
	if (object instanceof lisp.Macro) {
		return object.callable.apply(thisObject, args);
	} else {
		return object.apply(thisObject, args.map(resolve));
	}
}

function predicate (args, testFunc) {
	if (args.length === 0) {
		return false;
	}
	for (var i = 0; i < args.length; i++) {
		if (!testFunc(resolve(args[i]))) {
			return false;
		}
	}
	return true;
}

function comparator (args, testFunc) {
	if (args.length < 2) {
		return false;
	}
	var a = resolve(args[0]);
	for (var i = 1; i < args.length; i++) {
		var b = resolve(args[i]);
		if (!testFunc(a, b)) {
			return false;
		}
		a = b;
	}
	return true;
}
