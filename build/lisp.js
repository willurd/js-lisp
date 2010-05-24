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
/*jsl:ignore*/ // Suppress jsl warnings
// From: http://ejohn.org/blog/simple-javascript-inheritance/
// Inspired by base2 and Prototype
(function(){
  var initializing = false, fnTest = (/xyz/).test(function(){xyz;}) ? (/\b_super\b/) : /.*/;
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
/*jsl:end*/
/*jsl:ignore*/ // Suppress jsl warnings
// From: http://phpjs.org/functions/sprintf:522
// More info: http://php.net/manual/en/function.sprintf.php
function sprintf () {
    // http://kevin.vanzonneveld.net
    // +   original by: Ash Searle (http://hexmen.com/blog/)
    // + namespaced by: Michael White (http://getsprink.com)
    // +    tweaked by: Jack
    // +   improved by: Kevin van Zonneveld (http://kevin.vanzonneveld.net)
    // +      input by: Paulo Ricardo F. Santos
    // +   improved by: Kevin van Zonneveld (http://kevin.vanzonneveld.net)
    // +      input by: Brett Zamir (http://brett-zamir.me)
    // +   improved by: Kevin van Zonneveld (http://kevin.vanzonneveld.net)
    // *     example 1: sprintf("%01.2f", 123.1);
    // *     returns 1: 123.10
    // *     example 2: sprintf("[%10s]", 'monkey');
    // *     returns 2: '[    monkey]'
    // *     example 3: sprintf("[%'#10s]", 'monkey');
    // *     returns 3: '[####monkey]'

    var regex = /%%|%(\d+\$)?([-+\'#0 ]*)(\*\d+\$|\*|\d+)?(\.(\*\d+\$|\*|\d+))?([scboxXuidfegEG])/g;
    var a = arguments, i = 0, format = a[i++];

    // pad()
    var pad = function (str, len, chr, leftJustify) {
        if (!chr) {chr = ' ';}
        var padding = (str.length >= len) ? '' : Array(1 + len - str.length >>> 0).join(chr);
        return leftJustify ? str + padding : padding + str;
    };

    // justify()
    var justify = function (value, prefix, leftJustify, minWidth, zeroPad, customPadChar) {
        var diff = minWidth - value.length;
        if (diff > 0) {
            if (leftJustify || !zeroPad) {
                value = pad(value, minWidth, customPadChar, leftJustify);
            } else {
                value = value.slice(0, prefix.length) + pad('', diff, '0', true) + value.slice(prefix.length);
            }
        }
        return value;
    };

    // formatBaseX()
    var formatBaseX = function (value, base, prefix, leftJustify, minWidth, precision, zeroPad) {
        // Note: casts negative numbers to positive ones
        var number = value >>> 0;
        prefix = prefix && number && {'2': '0b', '8': '0', '16': '0x'}[base] || '';
        value = prefix + pad(number.toString(base), precision || 0, '0', false);
        return justify(value, prefix, leftJustify, minWidth, zeroPad);
    };

    // formatString()
    var formatString = function (value, leftJustify, minWidth, precision, zeroPad, customPadChar) {
        if (precision != null) {
            value = value.slice(0, precision);
        }
        return justify(value, '', leftJustify, minWidth, zeroPad, customPadChar);
    };

    // doFormat()
    var doFormat = function (substring, valueIndex, flags, minWidth, _, precision, type) {
        var number;
        var prefix;
        var method;
        var textTransform;
        var value;

        if (substring == '%%') {return '%';}

        // parse flags
        var leftJustify = false, positivePrefix = '', zeroPad = false, prefixBaseX = false, customPadChar = ' ';
        var flagsl = flags.length;
        for (var j = 0; flags && j < flagsl; j++) {
            switch (flags.charAt(j)) {
                case ' ': positivePrefix = ' '; break;
                case '+': positivePrefix = '+'; break;
                case '-': leftJustify = true; break;
                case "'": customPadChar = flags.charAt(j+1); break;
                case '0': zeroPad = true; break;
                case '#': prefixBaseX = true; break;
            }
        }

        // parameters may be null, undefined, empty-string or real valued
        // we want to ignore null, undefined and empty-string values
        if (!minWidth) {
            minWidth = 0;
        } else if (minWidth == '*') {
            minWidth = +a[i++];
        } else if (minWidth.charAt(0) == '*') {
            minWidth = +a[minWidth.slice(1, -1)];
        } else {
            minWidth = +minWidth;
        }

        // Note: undocumented perl feature:
        if (minWidth < 0) {
            minWidth = -minWidth;
            leftJustify = true;
        }

        if (!isFinite(minWidth)) {
            throw new Error('sprintf: (minimum-)width must be finite');
        }

        if (!precision) {
            precision = 'fFeE'.indexOf(type) > -1 ? 6 : (type == 'd') ? 0 : undefined;
        } else if (precision == '*') {
            precision = +a[i++];
        } else if (precision.charAt(0) == '*') {
            precision = +a[precision.slice(1, -1)];
        } else {
            precision = +precision;
        }
		
        // grab value using valueIndex if required?
        value = valueIndex ? a[valueIndex.slice(0, -1)] : a[i++];

        switch (type) {
            case 's': return formatString(String(value), leftJustify, minWidth, precision, zeroPad, customPadChar);
            case 'c': return formatString(String.fromCharCode(+value), leftJustify, minWidth, precision, zeroPad);
            case 'b': return formatBaseX(value, 2, prefixBaseX, leftJustify, minWidth, precision, zeroPad);
            case 'o': return formatBaseX(value, 8, prefixBaseX, leftJustify, minWidth, precision, zeroPad);
            case 'x': return formatBaseX(value, 16, prefixBaseX, leftJustify, minWidth, precision, zeroPad);
            case 'X': return formatBaseX(value, 16, prefixBaseX, leftJustify, minWidth, precision, zeroPad).toUpperCase();
            case 'u': return formatBaseX(value, 10, prefixBaseX, leftJustify, minWidth, precision, zeroPad);
            case 'i':
            case 'd':
                number = parseInt(+value, 10);
                prefix = number < 0 ? '-' : positivePrefix;
                value = prefix + pad(String(Math.abs(number)), precision, '0', false);
                return justify(value, prefix, leftJustify, minWidth, zeroPad);
            case 'e':
            case 'E':
            case 'f':
            case 'F':
            case 'g':
            case 'G':
                number = +value;
                prefix = number < 0 ? '-' : positivePrefix;
                method = ['toExponential', 'toFixed', 'toPrecision']['efg'.indexOf(type.toLowerCase())];
                textTransform = ['toString', 'toUpperCase']['eEfFgG'.indexOf(type) % 2];
                value = prefix + Math.abs(number)[method](precision);
                return justify(value, prefix, leftJustify, minWidth, zeroPad)[textTransform]();
            default: return substring;
        }
    };

    return format.replace(regex, doFormat);
}
/*jsl:end*/
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
var StringStream = Class.extend({
	init: function (data) {
		if (typeof(data) != "string") {
			throw new Error("Invalid object as StringStream input: " + data);
		}

		this.data = data;
		this.length = data.length;
		this.position = 0;
		this.line = 1;
		this.column = 0;
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
	
	next: function () {
		if (this.eof()) {
			throw new Error("EOF reached in StringStream");
		}
		
		var c = this.charAt(this.position);
		this.position += 1;
		this.column++;
		if (c == "\n") {
			this.line++;
			this.column = 0;
		}
		return c;
	},
	
	prev: function (count) {
		count = count || 1;
		this.position -= count;

		if (this.bof()) {
			throw new Error("Cannot access character at position " + this.position +
				" of StringStream");
		}
		
		return this.charAt(this.position);
	},
	
	swallowWhitespace: function () {
		while (WHITESPACE.indexOf(this.peek()) != -1 && !this.eof()) {
			this.position++;
		}
	}
});
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
var Env = Class.extend({
	init: function (parent, symbols) {
		this.parent = parent || null;
		this.symbols = symbols || {};
	},
	
	has: function (symbol) {
		if (symbol instanceof Symbol) {
			symbol = symbol.value;
		}
		
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
		symbol = String(symbol);
		
		var parts = symbol.split(".");
		var value;
		symbol = parts[0];
		parts = parts.slice(1);
		
		if (this.symbols.hasOwnProperty(symbol) || this.symbols[symbol]) {
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
		symbol = String(symbol);
		
		var parts = symbol.split(".");
		
		if (parts.length > 1) {
			var name = parts.slice(0,parts.length-1).join(".");
			object = this.get(name);
			
			if (!(object instanceof Object)) {
				throw new Error(name + " is unsubscriptable");
			}
			
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
	
	// FIXME: This method sucks.
	let: function (symbol, value) {
		symbol = String(symbol);
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
var Macro = Class.extend({
	init: function (callable) {
		this.callable = callable;
	}
});
function validateInput (input) {
	if (typeof(input) != "string" &&
		!(input instanceof StringStream)) {
		throw new parse.ParserException("Invalid input: " + input);
	}
	if (input instanceof StringStream) {
		return input;
	}
	return new StringStream(input);
}
var parse = {
	NUMBER_FORMATS: [
		(/^([0-9]+(?:\.(?:[0-9]+))?(?:e([0-9]+))?)(?:\s+|\)|$)/),
		(/^(0x(?:[0-9a-fA-F]+))(?:\s+|\)|$)/)
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
				var exp = parse.any(stream);
				if (exp !== undefined)
					expressions.push(exp);
				stream.swallowWhitespace();
			}
		} catch (e) {
			// There aren't any sexps left, or the rest is invalid
			// Should something else be done besides throwing an error?
			throw e;
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
		case ';':
			return parse.comment(stream);
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
		stream.next();
		stream.swallowWhitespace();
		var parts = [];
		while (stream.peek() != ')' && !stream.eof()) {
			var exp = parse.any(stream);
			if (exp !== undefined)
				parts.push(exp);
			stream.swallowWhitespace();
		}
		stream.next();
		return parts;
	},
	
	// Do we want object literals?
	// object: function (stream) {
	// 	throw new Error("Not impelemented");
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
	// 		var key /* grab the key */;
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
	},
	
	comment: function (stream) {
		stream = validateInput(stream);
		stream.swallowWhitespace();
		if (stream.peek() != ';') {
			throw new parse.ParserException("Invalid comment at position " +
				stream.position + " (starting with: '" + stream.peek() + "')");
		}
		var c = '';
		while ('\n\r'.indexOf(stream.peek()) == -1 &&
			   !stream.eof() &&
		 	   stream.slice(stream.position, stream.position+2) != '\n\r') {
			c += stream.next();
		}
		stream.next();
	}
};
const WHITESPACE = " \t\n\r";

var ROOT_ENV = new Env(new Env(null, global), {
	"t": true,
	"true": true,
	"false": false,
	"nil": null,
	"null": null,
	"undefined": undefined,
	"NaN": NaN,
	
	"*features*": [new Keyword("notmuch")]
});
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
 * Takes an object and a dotpath and calls the dotpath as a function
 * on the given object (with the given arguments).
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
 * Executes the rest of the arguments if the first argument
 * is true.
 * 
 * @return The return value of the last expression.
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
 * 
 */
// defmacro("<", function () {
// 	
// });

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
/**
 * Returns an instance of the given class, initialized with
 * the rest of the given arguments.
 */
defun("new", function (Class) {
	if (arguments.length < 1) {
		throw new Error("(new) requires at least 1 argument");
	}
	var argnames = [];
	// This is the only way I could figure out how to make the
	// passing of arguments to new Class(...) dynamic.
	for (var i = 1; i < arguments.length; i++) {
		var argname = "arg" + i;
		eval("var " + argname + " = " + arguments[i]);
		argnames.push(argname);
	}
	return eval("new Class(" + argnames.join(",") + ")");
});

/**
 * Returns the given arguments as a list.
 */
defun("list", function () {
	return argsToArray(arguments);
});

/**
 * Creates a JavaScript object using the given arguments as an
 * association list.
 */
defun("object", function () {
	var args = argsToArray(arguments);
	var object = {};
	
	if (args.length % 2 !== 0) {
		throw new Error("Invalid number of arguments to (object): " + args.length);
	}
	
	for (var i = 0; i < args.length; i += 2) {
		object[args[i]] = args[i+1];
	}
	
	return object;
});

/**
 * Returns the given arguments as a list.
 */
defun("array", function () {
	return argsToArray(arguments);
});

/**
 * Returns a value from an object given a key (will work with
 * array indices as well).
 */
defun("getkey", function (key, object) {
	if (arguments.length !== 2) {
		throw new Error("(getkey) requires 2 arguments (got " +
			arguments.length + ")");
	}
	return object[key];
});

/**
 * Sets a value on the given object using the given key.
 */
defun("setkey", function (key, object, value) {
	if (arguments.length !== 3) {
		throw new Error("(setkey) requires 3 arguments (got " +
			arguments.length + ")");
	}
	return object[key] = value;
});

/**
 * Prints the given arguments to the console.
 */
defun("print", function () {
	// Do not remove this. This is not a debug statement.
	lisp.log.apply(null, arguments);
});

/**
 * Joins the given arguments together into one string.
 */
defun("concat", function () {
	return argsToArray(arguments).join("");
});

/**
 * Joins the given arguments together into one string, using
 * the first argument as the separator.
 */
defun("join", function () {
	var args = argsToArray(arguments);
	var sep  = args[0];
	var list = args.slice(1).reduce(function (a, b) { return a.concat(b); });
	return list.join(sep);
});

/**
 * Returns the type of the given value.
 */
defun("typeof", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(typeof) requires 1 argument (got " +
			arguments.length + ")");
	}
	return typeof(value);
});

/**
 * Converts the given value to a string.
 * 
 * @tested
 */
defun("to-string", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(to-string) requires 1 argument (got " +
			arguments.length + ")");
	}
	return String(value);
});

/**
 * Converts the given value to a number.
 * 
 * @tested
 */
defun("to-number", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(to-number) requires 1 argument (got " +
			arguments.length + ")");
	}
	return Number(value);
});

/**
 * Converts the given value to a number.
 * 
 * @tested
 */
defun("to-boolean", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(to-boolean) requires 1 argument (got " +
			arguments.length + ")");
	}
	return Boolean(value);
});

/**
 * Converts the given string to uppercase.
 * 
 * @tested
 */
defun("to-upper", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(to-upper) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (typeof(value) != "string") {
		throw new Error("(to-upper) requires a string argument");
	}
	return value.toUpperCase();
});

/**
 * Converts the given string to uppercase.
 * 
 * @tested
 */
defun("to-lower", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(to-lower) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (typeof(value) != "string") {
		throw new Error("(to-lower) requires a string argument");
	}
	return value.toLowerCase();
});

/**
 * Reduces the given arguments on the / operator.
 */
defun("/", function () {
	return argsToArray(arguments).reduce(function (a, b) {
		return a / b;
	});
});

/**
 * Reduces the given arguments on the * operator.
 */
defun("*", function () {
	return argsToArray(arguments).reduce(function (a, b) {
		return a * b;
	});
});

/**
 * Reduces the given arguments on the + operator.
 */
defun("+", function () {
	return argsToArray(arguments).reduce(function (a, b) {
		return a + b;
	});
});

/**
 * Reduces the given arguments on the - operator.
 */
defun("-", function () {
	return argsToArray(arguments).reduce(function (a, b) {
		return a - b;
	});
});

/**
 * Reduces the given arguments on the % operator.
 */
defun("%", function () {
	return argsToArray(arguments).reduce(function (a, b) {
		return a % b;
	});
});

/**
 * Adds 1 to the given value.
 */
defun("1+", function (value) {
	if (arguments.length > 1) {
		throw new Error("(1+) requires 1 argument (got " +
			arguments.length + ")");
	}
	return Number(value) + 1;
});

/**
 * Calls sprintf (found in the vendor section) with the
 * supplied arguments.
 * 
 * @tested
 */
defun("format", function (print, format) {
	if (arguments.length < 2) {
		throw new Error("(format) expects at least 2 arguments (got " +
			arguments.length + ")");
	}
	if (typeof(format) != "string") {
		throw new Error("(format) expects a string format");
	}
	var output = sprintf.apply(null, argsToArray(arguments).slice(1));
	if (print) {
		lisp.log(output);
		return null;
	} else {
		return output;
	}
});
return {
	VERSION: "0.0.1",
	
	Env: Env,
	Macro: Macro,
	Symbol: Symbol,
	Keyword: Keyword,
	
	env: ROOT_ENV,
	
	parse: parse,
	defun: defun,
	defmacro: defmacro,
	
	eval: function (string, env) {
		var tempEnv = lisp.env;
		lisp.env = env || lisp.env;
		var expressions = parse.script(string);
		var ret = null;
		for (var i = 0; i < expressions.length; i++) {
			ret = resolve(expressions[i]);
		}
		lisp.env = tempEnv;
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

// Set this library up to work with node.js
if (typeof(window) == "undefined" &&
	typeof(global) == "object" &&
	typeof(require) == "function" &&
	typeof(exports) == "object") {
	// FIXME: Find a better way to tell we're running in node.js
	for (var key in lisp) {
		if (lisp.hasOwnProperty(key)) {
			exports[key] = lisp[key];
		}
	}
	
	lisp.log = require("sys").puts;
}
