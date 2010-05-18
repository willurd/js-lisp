// ================================================================================
// js-lisp: A lisp interpreter for browser scripting
// 
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// 
// Author: William Bowers <william.bowers@gmail.com>
// ================================================================================

var lisp = (function (global) {
	// ----------------------------------------------------------------------------
	// Constants
	// ----------------------------------------------------------------------------
	
	const WHITESPACE = " \t\n\r";
	
	// ----------------------------------------------------------------------------
	// Vendor
	// ----------------------------------------------------------------------------
	
	// From: http://ejohn.org/blog/simple-javascript-inheritance/
	// Inspired by base2 and Prototype
	(function(){
	  var initializing = false, fnTest = /xyz/.test(function(){xyz;}) ? /\b_super\b/ : /.*/;
	  // The base Class implementation (does nothing)
	  this.Class = function(){};

	  // Create a new Class that inherits from this class
	  Class.extend = function (classNameOrProp, prop) {
	    var _super = this.prototype;

		var className = prop ? classNameOrProp : "Class";
		prop = prop || classNameOrProp;

	    // Instantiate a base class (but only create the instance,
	    // don't run the init constructor)
	    initializing = true;
	    var prototype = new this();
	    initializing = false;

	    // Copy the properties over onto the new prototype
	    for (var name in prop) {
	      // Check if we're overwriting an existing function
	      prototype[name] = typeof prop[name] == "function" && 
	        typeof _super[name] == "function" && fnTest.test(prop[name]) ?
	        (function(name, fn){
	          return function() {
	            var tmp = this._super;

	            // Add a new ._super() method that is the same method
	            // but on the super-class
	            this._super = _super[name];

	            // The method only need to be bound temporarily, so we
	            // remove it when we're done executing
	            var ret = fn.apply(this, arguments);        
	            this._super = tmp;

	            return ret;
	          };
	        })(name, prop[name]) :
	        prop[name];
	    }

	    // The dummy class constructor
	    function Class() {
	      // All construction is actually done in the init method
	      if ( !initializing && this.init )
	        this.init.apply(this, arguments);
	    }

		Class.className = className;

	    // Populate our constructed prototype object
	    Class.prototype = prototype;

	    // Enforce the constructor to be what we expect
	    Class.constructor = Class;

	    // And make this class extendable
	    Class.extend = arguments.callee;

	    return Class;
	  };
	})();
	
	// ----------------------------------------------------------------------------
	// Implementation
	// ----------------------------------------------------------------------------
	
	var StringStream = Class.extend({
		init: function (data) {
			if (typeof(data) != "string")
				throw new Error("Invalid object as StringStream input: " + data);

			this.data = data;
			this.length = data.length;
			this.position = 0;
		},
		
		slice: function () {
			return this.data.slice.apply(this.data, arguments);
		},
		
		rest: function (from) {
			from = from || this.position;
			return this.slice(from, this.data.length);
		},
		
		bof: function () {
			return this.position < 0;
		},
		
		eof: function () {
			return this.position >= this.length;
		},
		
		peek: function (distance) {
			distance = distance || 0;
			return this.charAt(this.position + distance);
		},
		
		charAt: function (index) {
			return this.data.charAt(index);
		},
		
		next: function (count) {
			if (this.eof())
				throw new Error("EOF reached in StringStream");

			count = count || 1;
			var c = this.charAt(this.position);
			this.position += 1;
			return c;
		},
		
		prev: function (count) {
			count = count || 1;
			this.position -= count;

			if (this.bof())
				throw new Error("Cannot access character at position " + this.position +
					" of StringStream");
			
			return this.charAt(this.position);
		},
		
		swallowWhitespace: function () {
			while (WHITESPACE.indexOf(this.peek()) != -1 && !this.eof())
				this.position++;
		}
	});
	
	var Env = Class.extend({
		init: function (parent, symbols) {
			this.parent = parent || null;
			this.symbols = symbols || {};
		},
		
		has: function (symbol) {
			if (symbol instanceof Symbol)
				symbol = symbol.value;
			
			if (this.symbols.hasOwnProperty(symbol)) {
				return true;
			} else if (!this.parent) {
				return false;
			} else {
				if (this.parent instanceof Env) {
					return this.parent.has(symbol);
				} else {
					return this.parent[symbol] != undefined;
				}
			}
		},
		
		get: function (symbol) {
			if (symbol instanceof Symbol)
				symbol = symbol.value;
			
			var parts = symbol.split(".");
			var value;
			symbol = parts[0];
			parts = parts.slice(1);
			
			if (this.symbols.hasOwnProperty(symbol)) {
				value = this.symbols[symbol];
			} else if (!this.parent) {
				// This will be undefined if MACROS doesn't have the value
				value = lisp.MACROS[symbol];
			} else {
				if (this.parent instanceof Env) {
					value = this.parent.get(symbol);
				} else {
					value = this.parent[symbol];
				}
			}
			
			if (value && parts.length > 0) {
				for (var i = 0; i < parts.length; i++) {
					value = value[parts[i]];
				}
			}
			
			return value;
		},
		
		set: function (symbol, value) {
			if (symbol instanceof Symbol)
				symbol = symbol.value;
			
			var parts = symbol.split(".");
			
			if (parts.length > 1) {
				var name = parts.slice(0,parts.length-1).join(".");
				object = this.get(name);
				
				if (!(object instanceof Object))
					throw new Error(name + " is unsubscriptable");
				
				object[parts[parts.length-1]] = value;
			} else {
				if (this.has(symbol)) {
					if (this.symbols.hasOwnProperty(symbol)) {
						this.symbols[symbol] = value;
					} else if (this.parent instanceof Env) {
						this.parent.set(symbol, value);
					} else {
						this.parent[symbol] = value;
					}
				} else {
					this.symbols[symbol] = value;
				}
			}
		},
		
		let: function (symbol, value) {
			if (symbol instanceof Symbol)
				symbol = symbol.value;
			this.symbols[symbol] = value;
		}
	});
	
	var Symbol = Class.extend({
		init: function (value) {
			this.value = value;
		},
		
		toString: function () {
			return this.value;
		}
	});
	
	var Keyword = Class.extend({
		init: function (value) {
			this.value = value;
		},
		
		toString: function () {
			return this.value;
		}
	});
	
	function argsToArray (args) {
		var a = [];
		for (var i = 0; i < args.length; i++) {
			a.push(args[i]);
		}
		return a;
	}
	
	function makeRequest (url, successCallback) {
		var request;
		
		if (window.XMLHttpRequest) {
			request = new XMLHttpRequest();
		} else if (window.ActiveXObject) {
			request = new ActiveXObject("Msxml2.XMLHTTP");
		} else {
			throw new Error("Ajax request not supported in this browser");
		}
		
		request.onreadystatechange = function () {
			if (request.readyState == 4) {
				if (request.status == 200) {
					successCallback(request.responseText);
				} else if (request.status == 404) {
					throw new Error("Trying to load lisp script that does not exist: " +
					 	url);
				}
			}
		};
		
		request.open("GET", url, false); // Load the script synchronously
		request.send(null);
	}
	
	var Macro = Class.extend({
		init: function (callable) {
			this.callable = callable;
		}
	});
	
	var MACROS = {
		"lambda": new Macro(function () {
			var env  = new Env(lisp.env);
			var args = argsToArray(arguments);
			return (function (env, args) {
				var arglist = args[0];
				var body = args.slice(1);
				return function () {
					var tempEnv = lisp.env;
					lisp.env = env;
					lisp.env.let("this", this);
					for (var i = 0; i < arglist.length; i++) {
						lisp.env.let(arglist[i], arguments[i]);
					}
					var ret = null;
					for (var i = 0; i < body.length; i++) {
						ret = resolve(body[i]);
					}
					lisp.env = tempEnv;
					return ret;
				}
			})(env, args);
		}),
		
		"defun": new Macro(function () {
			var args = argsToArray(arguments);
			var name = args[0];
			var arglist = args[1];
			var body = args.slice(2);
			
			lisp.env.set(name, function () {
				lisp.env = new Env(lisp.env);
				for (var i = 0; i < arglist.length; i++) {
					lisp.env.set(arglist[i], arguments[i]);
				}
				var ret = null;
				for (var i = 0; i < body.length; i++) {
					ret = resolve(body[i]);
				}
				lisp.env = lisp.env.parent;
				return ret;
			})
		}),
		
		"let": new Macro(function () {
			lisp.env = new Env(lisp.env);
			var args = argsToArray(arguments);
			var letset = args[0];
			args = args.slice(1);
			
			for (var i = 0; i < letset.length; i++) {
				var symbol = letset[i][0];
				var value = resolve(letset[i][1]);
				lisp.env.let(symbol, value);
			}
			
			var ret = null;
			for (var i = 0; i < args.length; i++) {
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
			if (arguments.length == 0)
				return false;
			for (var i = 0; i < arguments.length; i++) {
				if (resolve(arguments[i])) {
					return true;
				}
			}
			return false;
		}),
		
		"and": new Macro(function () {
			if (arguments.length == 0)
				return false;
			for (var i = 0; i < arguments.length; i++) {
				if (!resolve(arguments[i])) {
					return false;
				}
			}
			return true;
		}),
	};
	
	function predicate (args, testFunc) {
		if (args.length == 0)
			return false;
		for (var i = 0; i < args.length; i++) {
			if (!testFunc(args[i])) {
				return false;
			}
		}
		return true;
	};
	
	var ENV = new Env(new Env(null, global), {
		"t": true,
		"true": true,
		"false": false,
		"nil": null,
		"null": null,
		"undefined": undefined,
		
		"not": function (value) {
			if (arguments.length != 1) {
				throw new Error("(not) requires 1 argument");
			}
			return !value;
		},
		
		"list": function () {
			return argsToArray(arguments);
		},
		
		"object": function () {
			var args = argsToArray(arguments);
			var object = {};
			
			if (args.length % 2 != 0)
				throw new Error("Invalid number of arguments to (object): " + args.length);
			
			for (var i = 0; i < args.length; i += 2) {
				object[args[i]] = args[i+1];
			}
			
			return object;
		},
		
		"new": function (Class) {
			if (arguments.length < 1) {
				throw new Error("(new) requires at least 1 argument");
			}
			var argnames = [];
			for (var i = 1; i < arguments.length; i++) {
				var argname = "arg" + i;
				eval("var " + argname + " = " + arguments[i]);
				argnames.push(argname);
			}
			return eval("new Class(" + argnames.join(",") + ")");
		},
		
		"getkey": function (key, object) {
			if (arguments.length < 2) {
				throw new Error("(getkey) requires 2 arguments (got " +
					arguments.length + ")");
			} else if (arguments.length > 2) {
				throw new Error("(getkey) expects only 2 arguments (got " +
					arguments.length + ")");
			}
			return object[key];
		},
		
		"setkey": function (key, object, value) {
			if (arguments.length < 3) {
				throw new Error("(setkey) requires 3 arguments (got " +
					arguments.length + ")");
			} else if (arguments.length > 3) {
				throw new Error("(setkey) expects only 3 arguments (got " +
					arguments.length + ")");
			}
			return object[key] = value;
		},
		
		"puts": function () {
			// Do not remove this. This is not a debug statement.
			console.info.apply(console, arguments);
		},
		
		"concat": function () {
			return argsToArray(arguments).join("");
		},
		
		"join": function () {
			var args = argsToArray(arguments);
			var sep  = args[0];
			var list = args.slice(1).reduce(function (a, b) { return a.concat(b) });
			return list.join(sep);
		},
		
		"typeof": function (value) {
			if (arguments.length == 0)
				return undefined;
			if (arguments.length > 1)
				throw new Error("(typeof) only accepts 1 argument");
			return typeof(value);
		},
		
		"to-string": function (value) {
			if (arguments.length == 0)
				return "";
			if (arguments.length > 1)
				throw new Error("(to-string) only accepts 1 argument");
			return String(value);
		},
		
		"to-number": function (value) {
			if (arguments.length == 0)
				return 0;
			if (arguments.length > 1)
				throw new Error("(to-number) only accepts 1 argument");
			return Number(value);
		},
		
		"is-true": function (value) {
			return predicate(arguments, function (value) {
				return value === true;
			});
		},
		
		"is-false": function (value) {
			return predicate(arguments, function (value) {
				return value === false;
			});
		},
		
		"is-null": function (value) {
			return predicate(arguments, function (value) {
				return value === null;
			});
		},
		
		"is-undefined": function (value) {
			return predicate(arguments, function (value) {
				return value === undefined;
			});
		},
		
		"is-string": function (value) {
			return predicate(arguments, function (value) {
				return typeof(value) == "string";
			});
		},
		
		"is-number": function (value) {
			return predicate(arguments, function (value) {
				return typeof(value) == "number";
			});
		},
		
		"/": function () {
			return argsToArray(arguments).reduce(function (a, b) {
				return a / b;
			});
		},
		
		"*": function () {
			return argsToArray(arguments).reduce(function (a, b) {
				return a * b;
			});
		},
		
		"+": function () {
			return argsToArray(arguments).reduce(function (a, b) {
				return a + b;
			});
		},
		
		"-": function () {
			return argsToArray(arguments).reduce(function (a, b) {
				return a - b;
			});
		},
		
		"1+": function (value) {
			if (arguments.length == 0)
				return 1;
			if (arguments.length > 1)
				throw new Error("(1+) only accepts 1 argument");
			return Number(value) + 1;
		}
	});
	
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
		var symbol = sexp[0];
		
		var object = lisp.env.get(symbol);
		if (typeof(object) != "function" && !(object instanceof Macro))
			throw new Error(symbol.value + " is not a function");
		
		var thisObjectPath = symbol.value.split(".").slice(0,-1).join(".");
		var thisObject = lisp.env.get(thisObjectPath);
		var args = sexp.slice(1);
		
		if (object instanceof lisp.Macro) {
			return object.callable.apply(thisObject, args);
		} else {
			return object.apply(thisObject, args.map(resolve));
		}
	}
	
	function lispEval (string, env) {
		var tempEnv = lisp.env;
		lisp.env = env || lisp.env;
		var expressions = parse.script(string);
		var ret = null;
		for (var i = 0; i < expressions.length; i++) {
			ret = resolve(expressions[i]);
		}
		lisp.env = tempEnv;
		return ret;
	}
	
	function validateInput (input) {
		if (typeof(input) != "string" &&
			!(input instanceof StringStream)) {
			throw new parse.ParserException("Invalid input: " + input);
		}
		if (input instanceof StringStream)
			return input;
		return new StringStream(input);
	}
	
	var parse = {
		NUMBER_FORMATS: [
			/^([0-9]+(?:\.(?:[0-9]+))?(?:e([0-9]+))?)(?:\s+|\)|$)/,
			/^(0x(?:[0-9a-fA-F]+))(?:\s+|\)|$)/,
		],
		
		ParserException: function (message) {
			this.toString = function () {
				return "ParserException: " + message;
			};
		},
		
		script: function (stream) {
			stream = validateInput(stream);
			var expressions = [];
			
			try {
				while (!stream.eof()) {
					stream.swallowWhitespace();
					var sexp = parse.sexp(stream);
					expressions.push(sexp);
				}
			} catch (e) {
				// There aren't any sexps left, or the rest is invalid
			}
			
			return expressions;
		},
		
		any: function (stream) {
			stream = validateInput(stream);
			stream.swallowWhitespace();
			switch (stream.peek())
			{
			case '(':
				return parse.sexp(stream);
			case '"':
				return parse.string(stream);
			case ':':
				return parse.keyword(stream);
			// case '{':
			// 	return parse.object(stream);
			default:
				var rest = stream.rest();
				for (var i = 0; i < lisp.parse.NUMBER_FORMATS.length; i++) {
					var format = lisp.parse.NUMBER_FORMATS[i];
					var match = rest.match(format);
					if (match) {
						return parse.number(stream, match[1]);
					}
				}
				return parse.symbol(stream);
			}
		},
		
		sexp: function (stream) {
			stream = validateInput(stream);
			stream.swallowWhitespace();
			if (stream.peek() != '(') {
				throw new parse.ParserException("Invalid sexp at position " +
					stream.position + " (starting with: '" + stream.peek() + "')");
			}
			stream.next()
			stream.swallowWhitespace();
			var parts = [];
			while (stream.peek() != ')' && !stream.eof()) {
				parts.push(parse.any(stream));
				stream.swallowWhitespace();
			}
			stream.next();
			return parts;
		},
		
		// Do we want object literals?
		// object: function (stream) {
		// 	stream = validateInput(stream);
		// 	stream.swallowWhitespace();
		// 	if (stream.peek() != '{') {
		// 		throw new parse.ParserException("Invalid object at position " +
		// 			stream.position + " (starting with: '" + stream.peek() + "')");
		// 	}
		// 	stream.next()
		// 	stream.swallowWhitespace();
		// 	while (stream.peek() != '}') {
		// 		stream.swallowWhitespace();
		// 		var key = 
		// 	}
		// },
		
		symbol: function (stream) {
			stream = validateInput(stream);
			stream.swallowWhitespace();
			var badChars = WHITESPACE + '()';
			if (badChars.indexOf(stream.peek()) != -1) {
				throw new parse.ParserException("Invalid symbol at position " +
					stream.position + " (starting with: '" + stream.peek() + "')");
			}
			var symbol = "";
			while (badChars.indexOf(stream.peek()) == -1 && !stream.eof()) {
				symbol += stream.next();
			}
			return new Symbol(symbol);
		},
		
		keyword: function (stream) {
			stream = validateInput(stream);
			stream.swallowWhitespace();
			if (stream.peek() != ':') {
				throw new parse.ParserException("Invalid keyword at position " +
					stream.position + " (starting with: '" + stream.peek() + "')");
			}
			stream.next();
			return new Keyword(parse.symbol(stream).value);
		},
		
		string: function (stream) {
			stream = validateInput(stream);
			stream.swallowWhitespace();
			if (stream.peek() != '"') {
				throw new parse.ParserException("Invalid string at position " +
					stream.position + " (starting with: '" + stream.peek() + "')");
			}
			var string = "";
			stream.next();
			while (stream.peek() != '"' && !stream.eof()) {
				var c = stream.next();
				switch (c)
				{
				case "\\":
					string += parse.stringEscape(stream);
					break;
				default:
					string += c;
					break;
				}
			}
			stream.next();
			return string;
		},
		
		stringEscape: function (stream) {
			stream = validateInput(stream);
			var c = stream.next();
			return eval('"' + '\\' + c + '"');
		},
		
		number: function (stream, match) {
			if (!match) {
				stream = validateInput(stream);
				stream.swallowWhitespace();
				var rest = stream.rest();
				for (var i = 0; i < lisp.parse.NUMBER_FORMATS.length; i++) {
					var format = lisp.parse.NUMBER_FORMATS[i];
					match = rest.match(format);
					if (match) {
						match = match[1];
						break;
					}
				}
			}
			
			if (!match) {
				throw new ParseException("Invalid number at position " + stream.position +
					" (starting with: '" + stream.peek() + "')");
			}
			
			stream.position += match.length;
			return eval(match);
		}
	};
	
	return {
		MACROS: MACROS,
		
		Env: Env,
		Macro: Macro,
		Symbol: Symbol,
		
		parse: parse,
		env: ENV,
		eval: lispEval,
		
		dotag: function (tag) {
			if (tag.src) {
				makeRequest(tag.src, function (script) {
					lisp.eval(script);
					lisp.eval(tag.innerText);
					tag.parentElement.removeChild(tag);
				});
			} else {
				lisp.eval(tag.innerText);
				tag.parentElement.removeChild(tag);
			}
		},
		
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
})(this);
