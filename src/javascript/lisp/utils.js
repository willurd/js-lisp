function defun (name, func) {
	var env = (lisp && lisp.env) || ROOT_ENV;
	env.set(name, func);
}

function defmacro (name, func) {
	var env = (lisp && lisp.env) || ROOT_ENV;
	env.set(name, new Macro(func));
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

function doSExp (sexp) {
	var first = sexp[0];
	var object = resolve(first);
	
	if (typeof(object) != "function" && !(object instanceof Macro)) {
		throw new Error(first.value + " is not callable");
	}
	
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
