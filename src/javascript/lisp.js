return {
	VERSION: "0.0.1",
	
	Class: Class,
	Env: Env,
	Macro: Macro,
	Symbol: Symbol,
	Keyword: Keyword,
	Generator: Generator,
	
	env: ROOT_ENV,
	
	exception: {
		StreamException: StreamException,
		StreamEOFException: StreamEOFException,
		ArgumentError: ArgumentError,
		StopIteration: StopIteration
	},
	
	parse: parse,
	defun: defun,
	defmacro: defmacro,
	
	/**
	 * Evaluates a string as lisp code.
	 */
	eval: function (string, env) {
		var tempEnv = lisp.env;
		lisp.env = env || lisp.env;
		try {
			var expressions = parse.script(string);
			var ret = null;
			for (var i = 0; i < expressions.length; i++) {
				ret = resolve(expressions[i]);
			}
		} finally {
			lisp.env = tempEnv;
		}
		return ret;
	},
	
	/**
	 * This method is here so lisp.log can be called in code and it
	 * will work regardless of if we're being called in a browser (with
	 * or without "console") or in node.js (if we're in node.js, this
	 * will be replaced with sys.puts).
	*/
	log: function () {
		if (typeof(console) == "object" && console.log) {
			console.log.apply(console, arguments);
		}
	},
	
	/**
	 * Loads an arbitrary file and evaluates it as lisp code.
	 */
	load: function (source, callback) {
		makeRequest(source, function (script) {
			lisp.eval(script);
			if (callback) {
				callback(source);
			}
		});
	},
	
	/**
	 * Handles a script tag by loading and evaluating the script pointed
	 * to by its src attribute (if there is one), and then by evaluating
	 * its inner content (if there is any).
	 */
	dotag: function (tag) {
		if (tag.src) {
			lisp.load(tag.src, function (script) {
				lisp.eval(tag.textContent);
				tag.parentElement.removeChild(tag);
			});
		} else {
			lisp.eval(tag.textContent);
			tag.parentElement.removeChild(tag);
		}
	},
	
	/**
	 * Grabs all of the unevaluated text/lisp script tags so far in the
	 * html document and evaluates their contents.
	 */
	run: function () {
		var tags = document.getElementsByTagName("script");
		for (var i = 0; i < tags.length; i++) {
			var tag = tags[i];
			if (tag.type == "text/lisp") {
				lisp.dotag(tag);
			}
		}
	}
};
