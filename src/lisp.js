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
	
	const ESCAPES = {
		"n": "\n"
	};
	
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
				value = undefined;
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
				this.symbols[symbol] = value;
			}
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
			if (request.readyState == 4 && request.status == 200) {
				successCallback(request.responseText);
			}
		};

		request.open("GET", url, false); // Load the script synchronously
		request.send(null);
	}
	
	var ENV = new Env(new Env(null, global), {
		"lambda": function (env, args) {
			env = new Env(env);
			var arglist = args[0];
			var expressions = args.slice(1);
			return function () {
				for (var i = 0; i < arglist.length; i++) {
					env.set(arglist[i], arguments[i]);
				}
				for (var i = 0; i < expressions.length; i++) {
					doSExp(expressions[i], env);
				}
			};
		},
		
		"let": function (env, args) {
			var env = new Env(env);
			var letset = args[0];
			var args = args.slice(1);
			
			for (var i = 0; i < letset.length; i++) {
				var symbol = letset[i][0];
				var value = letset[i][1];
				if (value instanceof Array) {
					value = doSExp(value, env);
				}
				env.set(symbol, value);
			}
			
			var ret = null;
			for (var i = 0; i < args.length; i++) {
				ret = doSExp(args[i], env);
			}
			
			return ret;
		},
		
		"setq": function (env, args) {
			var symbol = args[0];
			var value  = args[1];
			
			if (value instanceof Symbol) {
				throw new Error("Not Implemented - Symbol values in setq");
			} else if (value instanceof Array) {
				value = doSExp(value, env);
			} else if (["string", "number"].indexOf(typeof(value)) >= 0) {
				console.info(3, value);
				value = value;
			} else {
				throw new Error("Unknown value type");
			}
			
			env.set(symbol, value);
		},
		
		"puts": function (env, args) {
			var arg;
			for (var i = 0; i < args.length; i++) {
				arg = args[i];
				if (arg instanceof Symbol) {
					args[i] = env.get(arg);
				} else if (arg instanceof Array) {
					args[i] = doSExp(arg, env);
				} else {
					// No need to modify the arg.
				}
			}
			// Do not remove this. This is not a debug statement.
			console.log.apply(console, args);
		}
	});
	
	function doSExp (sexp, env) {
		env = env || ENV;
		var symbol = sexp[0];
		var parent = null;
		var func;
		
		if (env.has(symbol)) {
			func = env.get(symbol);
			return func.apply(parent, [env, sexp.slice(1)]);
		} else {
			var parts = symbol.value.split('.');
			var object;
			var i = 0;
			
			if (env.has(parts[0])) {
				object = env.get(parts[0]);
				i = 1;
			} else {
				object = global;
			}
			
			for (; i < parts.length; i++) {
				parent = object;
				object = object[parts[i]];
			}
			
			func = object;
			if (func === null || func === undefined)
				throw new Error("Access of undefined symbol: " + symbol.value);
			return func.apply(parent, sexp.slice(1));
		}
	}
	
	function runScript (script, env) {
		var expressions = parse.script(script);
		
		for (var i = 0; i < expressions.length; i++) {
			doSExp(expressions[i], env);
		}
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
			default:
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
			return ESCAPES[c];
		}
	};

	return {
		Env: Env,
		parse: parse,
		
		run: function () {
			var scripts = document.getElementsByTagName("script");
			var script;
			for (var i = 0; i < scripts.length; i++) {
				script = scripts[i];
				if (script.type == "text/lisp") {
					if (script.src) {
						makeRequest(script.src, function (script) {
							runScript(script);
							runScript(scripts[i].innerText);
						});
					} else {
						runScript(scripts[i].innerText);
					}
				}
			}
		}
	};
})(this);
