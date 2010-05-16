// ================================================================================
// js-lisp: A lisp interpreter for browser scripting.
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

var lisp = (function () {
	// ----------------------------------------------------------------------------
	// Constants
	// ----------------------------------------------------------------------------
	
	const ESCAPES = {
		"n": "\n"
	};
	
	const WHITESPACE = " \t\n\r";
	
	// ----------------------------------------------------------------------------
	// Implementation
	// ----------------------------------------------------------------------------
	
	function StringStream (data) {
		if (typeof(data) != "string")
			throw new Error("Invalid object as StringStream input: " + data);
		
		this.data = data;
		this.length = data.length;
		this.position = 0;
		
		this.slice = function () {
			return this.data.slice.apply(this.data, arguments);
		};
		
		this.rest = function (from) {
			from = from || this.position;
			return this.slice(from, this.data.length);
		};
		
		this.bof = function () {
			return this.position < 0;
		};
		
		this.eof = function () {
			return this.position >= this.length;
		};
		
		this.peek = function (distance) {
			distance = distance || 0;
			return this.charAt(this.position + distance);
		};
		
		this.charAt = function (index) {
			return this.data.charAt(index);
		};
		
		this.next = function (count) {
			if (this.eof())
				throw new Error("EOF reached in StringStream");

			count = count || 1;
			var c = this.charAt(this.position);
			this.position += 1;
			return c;
		};
		
		this.prev = function (count) {
			count = count || 1;
			this.position -= count;

			if (this.bof())
				throw new Error("Cannot access character at position " + this.position +
					" of StringStream");
			
			return this.charAt(this.position);
		};
		
		this.swallowWhitespace = function () {
			while (WHITESPACE.indexOf(this.peek()) != -1 && !this.eof())
				this.position++;
		};
	}
	
	function Env (parent, symbols) {
		this.parent = parent || null;
		this.symbols = symbols || {};
		this.get = function (symbol) {
			if (this.symbols.hasOwnProperty(symbol)) {
				return this.symbols[symbol];
			} else if (!this.parent) {
				return null;
			} else {
				return this.parent.get(symbol);
			}
		};
		this.set = function (symbol, value) {
			this.symbols[symbol] = value;
		};
	}

	function argsToArray (args) {
		var a = [];
		for (var i = 0; i < args.length; i++) {
			a.push(args[i]);
		}
		return a;
	}

	var ENV = new Env(new Env(null, window), {
		"let": function (env, args) {
			var env = new Env(env, {});
			var letset = args[0];
			var args = args.slice(1);

			for (var i = 0; i < letset.length; i++) {
				var symbol = letset[i][0];
				var value = letset[i][1];
				if (value instanceof Array) {
					value = doSExp(value);
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
			var parts = args[0].split('.');
			var value = args[1];
			var obj = env.get(parts[0]);
			var i;
			for (i = 1; i < parts.length-1; i++) {
				obj = obj[parts[i]];
			}
			obj[parts[i]] = value;
		},

		"puts": function (env, args) {
			console.log(env.get(args[0]));
		}
	});
	
	function doSExp (sexp, env) {
		env = env || ENV;
		var symbol = sexp[0];
		var func = env.get(symbol);
		var parent = null;
		if (!func) {
			var parts = symbol.split('.');
			var object = window;
			for (var i = 0; i < parts.length; i++) {
				parent = object;
				object = object[parts[i]];
			}
			func = object;
			return func.apply(parent, sexp.slice(1));
		} else {
			return func.apply(parent, [env, sexp.slice(1)]);
		}
	}
	
	function runScript (script) {
		var expressions = parse.script(script);
		
		for (var i = 0; i < expressions.length; i++) {
			doSExp(expressions[i]);
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
			return symbol;
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
		parse: parse,
		
		run: function () {
			var scripts = document.getElementsByTagName("script");
			for (var i = 0; i < scripts.length; i++) {
				if (scripts[i].type == "text/lisp") {
					runScript(scripts[i].innerText);
				}
			}
		}
	};
})();
