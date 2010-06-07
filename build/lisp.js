// ================================================================================
// js-lisp: A, 100% JavaScript, lisp interpreter for browser scripting
// 
// Disclaimer: DO NOT attempt to use this project for any production code
// whatsoever. This project is still VERY young; if there were such a thing
// as negative version numbers, it would have one. For now, this is a toy
// project. It is my hope that js-lisp will one day grow up to become a solid
// library devs around the world can use to sefely get their Lisp fix while
// they get their JavaScript fix, but until that time, DO NOT use this in
// production code (and if you do, I'm not responsible for the craziness that
// will most certainly ensue thereafter).
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

/**
 * @namespace
 */
var lisp = (function (global) {
	var jseval = window.eval;

/*jsl:ignore*/ // Suppress jsl warnings

(function () {
	var initializing = false;
	var fnTest = (/xyz/).test(function(){xyz;}) ? (/\b_super\b/) : /.*/;
	
	/**
	 * <p>Defines a base class from which to create new classes that can be
	 * extended into new classes as well.</p>
	 * 
	 * <p>This constructor does nothing.</p>
	 * 
	 * <p>Modified from: http://ejohn.org/blog/simple-javascript-inheritance/</p>
	 * 
	 * @class
	 * @name Class
	 */
	this.Class = function () {};
	
	/**
	 * <p>Creates a new class that inherits from the calling class.</p>
	 * 
	 * @function
	 * 
	 * @param {string, object} classNameOrProps
	 *     Either the name of the class, or an object containing the class' properties.
	 * @param {object} props
	 *     An object containing the class' properties (only if classNameOrProps
	 *     specifies the class name).
	 */
	Class.extend = function (classNameOrProps, props) {
		var _super = this.prototype;
		var className = props ? classNameOrProps : "Class";
		props = props || classNameOrProps;
		
		// Instantiate a base class (but only create the instance,
		// don't run the init constructor)
		initializing = true;
		var prototype = new this();
		initializing = false;
		
		// Copy the properties over onto the new prototype
		for (var name in props) {
			// Check if we're overwriting an existing function
			if (typeof props[name] == "function" &&
				typeof _super[name] == "function" &&
				fnTest.test(props[name])) {
				prototype[name] = (function(name, fn){
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
				})(name, props[name]);
			} else {
				prototype[name] = props[name];
			}
		}
		
		// The new class
		var NewClass = function () {
			// All construction is actually done in the init method
			if (!initializing && this.init)
				this.init.apply(this, arguments);
		}
		
		NewClass.className = className;
		NewClass.prototype = prototype;
		NewClass.constructor = Class;
		NewClass.extend = arguments.callee;
		
		return NewClass;
	};
})();

/*jsl:end*/

/*jsl:ignore*/ // Suppress jsl warnings

/**
 * <p>Formats the string 'format' with the given arguments. Uses the
 * php formatting style, defined at
 * http://php.net/manual/en/function.sprintf.php.</p>
 * 
 * <p>From: http://phpjs.org/functions/sprintf:522.</p>
 * 
 * @param {string} format
 *     The string to be formatted with the given arguments.
 * @param {[mixed]} rest
 *     The rest of the arguments with which to format the given string.
 * 
 * @example
 *     >> sprintf("%01.2f", 123.1)
 *     => 123.10
 * @example
 *     >> sprintf("[%10s]", 'monkey')
 *     => "[    monkey]"
 * @example
 *     >> sprintf("[%'#10s]", 'monkey')
 *     => "[####monkey]"
 */
function sprintf (format /*, ... */) {
    // http://kevin.vanzonneveld.net
    // +   original by: Ash Searle (http://hexmen.com/blog/)
    // + namespaced by: Michael White (http://getsprink.com)
    // +    tweaked by: Jack
    // +   improved by: Kevin van Zonneveld (http://kevin.vanzonneveld.net)
    // +      input by: Paulo Ricardo F. Santos
    // +   improved by: Kevin van Zonneveld (http://kevin.vanzonneveld.net)
    // +      input by: Brett Zamir (http://brett-zamir.me)
    // +   improved by: Kevin van Zonneveld (http://kevin.vanzonneveld.net)

    var regex = /%%|%(\d+\$)?([-+\'#0 ]*)(\*\d+\$|\*|\d+)?(\.(\*\d+\$|\*|\d+))?([scboxXuidfegEG])/g;
    var a = arguments, i = 0, format = a[i++];
	
    var pad = function (str, len, chr, leftJustify) {
        if (!chr) {chr = ' ';}
        var padding = (str.length >= len) ? '' : Array(1 + len - str.length >>> 0).join(chr);
        return leftJustify ? str + padding : padding + str;
    };
	
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

function toJSON (object, pretty, levels, level) {
	levels = levels || 2; // Default levels
	level = level || 0;
	
	var done = level >= levels;
	var newline = pretty ? '\n' : '';
	var singleprefix = pretty ? times(' ', 2) : '';
	var prefix  = pretty ? times(singleprefix, level) : '';
	
	var json;
	var value;
	
	switch (typeof(object))
	{
	case 'undefined':
		return 'undefined';
	case 'object':
		if (!object) {
			// 'object' is null.
			return 'null';
		} else if (object instanceof Array) {
			// 'object' is an Array.
			json = '[';
			
			if (!done) {
				json += newline;
				value = null;
				for (var i = 0; i < object.length; i++) {
					value = toJSON(object[i], pretty, levels, level+1);
					json += prefix + singleprefix + value + ', ' + newline;
				}
				json = json.replace(/[\s\n]*$/g, '');
				json = json.replace(/,$/, '');
				if (object.length > 0) {
					json += newline + prefix;
				}
			} else {
				json += ' ... ';
			}
			
			return json + ']';
		} else if (object instanceof Date) {
			// 'object' is a Date.
			// Taken from http://www.json.org/json2.js
			function f (n) {
				// Format integers to have at least two digits.
				return n < 10 ? '0' + n : n;
			}
			
			return '"' +
				   object.getUTCFullYear()	   + '-' +
				   f(object.getUTCMonth() + 1) + '-' +
				   f(object.getUTCDate())	   + ' ' +
				   f(object.getUTCHours())	   + ':' +
				   f(object.getUTCMinutes())   + ':' +
				   f(object.getUTCSeconds())   +
				   '"';
		} else {
			json = '{';
			
			if (!done) {
				json = json + newline;
				count = 0;
				value = null;
				for (var key in object) {
					count++;
					if (object.hasOwnProperty(key)) {
						if (object[key] == window) {
							value = "[window]";
						} else {
							value = toJSON(object[key], pretty, levels, level+1);
						}
						json += prefix + singleprefix + '"' + key + '": ' + value;
						json += ', ' + newline;
					}
				}
				json = json.replace(/[\s\n]*$/g, '');
				json = json.replace(/,$/, '');
				if (count > 0) {
					json += newline + prefix;
				}
			} else {
				json += ' ... ';
			}
			
			return json + '}';
		}
	case 'function':
		var match = object.toString().match(/[^\(]+\([^\)]*\)/);
		return (match ? match[0] : 'function ()') + ' { ... }';
	case 'string':
		return '"' + object.replace(/"/g, '\\"') + '"';
	case 'number':
		return object;
	case 'boolean':	
	default:
		return object.toString();
	}
}

/**
 * The method used for (equal) equality in js-lisp.
 * 
 * @returns Whether a and b are equal from js-lisp's perspective.
 */
function equal (a, b) {
	// Test Symbol equality
	if (a instanceof Symbol) {
		if (!(b instanceof Symbol)) {
			return false;
		}
		return a.value == b.value;
	}
	
	// Test Keyword equality
	if (a instanceof Keyword) {
		if (!(b instanceof Keyword)) {
			return false;
		}
		return a.value == b.value;
	}
	
	// Test Array (list) equality
	if (a instanceof Array) {
		if (!(b instanceof Array)) {
			return false;
		}
		if (a.length !== b.length) {
			return false; // Return early in this easy and fast test
		}
		for (var i = 0; i < a.length; i++) {
			if (!equal(a[i], b[i])) {
				return false;
			}
		}
		return true;
	}
	
	return a == b;
}

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
	
	request.open("GET", url, false); // Load the script synchronously
	request.send(null);
	
	if (request.status == 200) {
		successCallback(request.responseText);
	} else if (request.status == 404) {
		throw new Error("Trying to load lisp script that does not exist: " +
			url);
	}
}

function times (string, num) {
	var ret = '';
	for (var i = 0; i < num; i++) {
		ret += string;
	}
	return ret;
}

var StreamException = Class.extend({
	init: function (message) {
		this.message = message;
	},
	toString: function () {
		return "StreamException: " + this.message;
	}
});

var StreamEOFException = StreamException.extend({
	init: function (message) {
		this.message = message;
	},
	toString: function () {
		return "StreamEOFException: " + this.message;
	}
});

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
			throw new StreamEOFException("EOF reached in StringStream");
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
	if (!sexp) {
		throw new Error("doSExp got empty expression");
	}
	
	if (sexp.length === 0) {
		// An expression with no arguments, in js-lisp, is an empty list.
		return [];
	}
	
	var first = sexp[0];
	var object = resolve(first);
	
	if (typeof(object) != "function" && !(object instanceof Macro)) {
		throw new Error("'" + first.value + "' is not callable");
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
			
			try {
				object[parts[parts.length-1]] = value;
			} catch (e) {
				throw new Error(name + " is unsubscriptable: " + e);
			}
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
				var object = this;
				while (object.parent) {
					object = object.parent;
				}
				object.symbols[symbol] = value;
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

/**
 * <p>An object containing all of the js-lisp parsers (string, numbers,
 * quoted items, lists, symbols, keywords, comments, etc);</p>
 * 
 * @namespace
 * @name parse
 */
var parse = {};

/**
 * <p>A list of possible number formats.</p>
 * 
 * @constant
 */
parse.NUMBER_FORMATS = [
	(/^(([+-]{1})?[0-9]+(?:\.(?:[0-9]+))?(?:e([0-9]+))?)(?:\s+|\)|$)/),
	(/^(0x(?:[0-9a-fA-F]+))(?:\s+|\)|$)/)
];

parse.ParserException = Class.extend("ParserException", 
	/** @lends parse.ParserException */
{
	/**
	 * <pre>
	 * The exception type thrown when any parse error occurs.
	 * </pre>
	 * 
	 * @constructs
	 * @extends Class
	 */
	init: function (message) {
		this.message = message;
	},
	
	/**
	 * <pre>
	 * Returns a string representation of the exception.
	 * </pre>
	 * 
	 * @function
	 */
	toString: function () {
		return "ParserException: " + this.message;
	}
});

/**
 * <p>Parses a lisp script, which can be any number of root-level expression,
 * into an array of ASTs representing those expressions.</p>
 * 
 * @param {string, StringStream} stream
 *     A string or StringStream instance that holds the script contents.
 * 
 * @returns An array of the parsed expressions.
 */
parse.script = function (stream) {
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
};

/**
 * @returns The parsed object.
 */
parse.any = function (stream) {
	stream = validateInput(stream);
	stream.swallowWhitespace();
	
	switch (stream.peek())
	{
	case '(':
		return parse.sexp(stream);
	case "'":
		stream.next();
		return [_S("quote"), parse.any(stream)];
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
};

/**
 * @returns The parsed sexp.
 */
parse.sexp = function (stream) {
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
};

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

/**
 * @returns The parsed symbol.
 */
parse.symbol = function (stream) {
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
	return _S(symbol);
};

/**
 * @returns The parsed keyword.
 */
parse.keyword = function (stream) {
	stream = validateInput(stream);
	stream.swallowWhitespace();
	if (stream.peek() != ':') {
		throw new parse.ParserException("Invalid keyword at position " +
			stream.position + " (starting with: '" + stream.peek() + "')");
	}
	stream.next();
	return new Keyword(parse.symbol(stream).value);
};

/**
 * @returns The parsed string.
 */
parse.string = function (stream) {
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
};

/**
 * @returns The parsed escaped character.
 */
parse.stringEscape = function (stream) {
	stream = validateInput(stream);
	var c = stream.next();
	return eval('"' + '\\' + c + '"');
};

/**
 * @returns The parsed number.
 */
parse.number = function (stream, match) {
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
		throw new parse.ParserException("Invalid number at position " + stream.position +
			" (starting with: '" + stream.peek() + "')");
	}
	
	stream.position += match.length;
	return eval(match);
};

/**
 * @returns Nothing
 */
parse.comment = function (stream) {
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
	if (!stream.eof()) {
		stream.next();
	}
};

var WHITESPACE = " \t\n\r";

var ROOT_ENV = new Env(new Env(null, global), {
	"t": true,
	"true": true,
	"f": false,
	"false": false,
	"nil": null,
	"null": null,
	"undefined": undefined,
	"NaN": NaN,
	
	/**
	 * This is here because values defined in the lisp env clobber the
	 * global namespace (this is on purpose, so those values can be used
	 * by other JavaScript code). Howver, one thing we definitely don't
	 * want to do is clobber standard JavaScript functions, like eval.
	 * 
	 * TODO: Test me
	 */
	"eval": function (expression) {
		if (arguments.length !== 1) {
			throw new Error("(eval) requires 1 argument (got " +
				arguments.length + ")");
		}
		return resolve(expression);
	},
	
	"*features*": [_K("notmuch")]
});

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
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name quote
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @returns The given argument, unevaluated.
 */
defmacro("quote", function (expression) {
	if (arguments.length !== 1) {
		throw new Error("(quote) requires 1 argument (got " +
			arguments.length + ")");
	}
	return expression;
});

/**
 * <pre>
 * Creates an anonymous function with the first (required)
 * expression as its arglist and which executes the rest of the
 * expressions when called.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @name lambda
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @returns The created function.
 */
defmacro("lambda", function (arglist /*, ... */) {
	var env  = new Env(lisp.env);
	var args = argsToArray(arguments);
	
	if (arguments.length > 0 && !(arglist instanceof Array)) {
		throw new Error("(lambda) requires a list as its first expression " +
			"(got " + String(arglist) + ")");
	}
	
	return (function (env, args) {
		var body = args.slice(1);
		return function () {
			if (args.length < 2) {
				return null; // This function does nothing
			}
			var largs = argsToArray(arguments);
			var tempEnv = lisp.env;
			var i;
			lisp.env = env;
			lisp.env.let("this", this);
			for (i = 0; i < arglist.length; i++) {
				var argname = arglist[i];
				if (argname == "&rest") {
					if (i == arglist.length - 1) {
						throw new Error("No argument name after &rest identifier");
					}
					if (arglist.length > i + 2) {
						throw new Error("Unexpected arguments (" +
							arglist.slice(i+1).join(" ") + ") after &rest argument");
					}
					lisp.env.let(arglist[i+1], largs.slice(i));
					break;
				} else {
					lisp.env.let(argname, largs[i]);
				}
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
 * <pre>
 * Defines a function. This is shorthand for (setq name (lambda ...)).
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name defun
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("defun", function (name, arglist /*, ... */) {
	if (arguments.length === 0) {
		throw new Error("(defun) requires at least 1 argument.");
	}
	if (!(name instanceof Symbol)) {
		throw new Error("(defun) requires a symbol as its first argument (got " +
			String(name) + ")");
	}
	if (arguments.length > 1 && !(arglist instanceof Array)) {
		throw new Error("(defun) requires a list of symbols as its second " +
			"expression (got " + String(arglist) + ")");
	}
	var body = argsToArray(arguments).slice(2);
	var lambda = [_S("lambda"), arglist].concat(body);
	resolve([_S("setq"), name, lambda]);
	return null;
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
 *   >> (try)
 *   => nil
 * 
 * @example Empty catch block (silences the error)
 *   >> (try
 *          (throw (new Error))
 *        (catch))
 *   => nil
 * 
 * @example Multiple expressions with a full catch block
 *   >> (try
 *          (print "This will print")
 *          (throw (new Error "This cuts the expression short"))
 *          (print "This will not print")
 *        (catch (e)
 *          (format t "This will print when the error is thrown: %s" e)))
 *   => (no return value)
 * 
 * @example try/finally
 *   >> (try
 *          (throw (new Error))
 *        (finally
 *          (print "This always runs")))
 *   => (no return value) ; Due to the error that is thrown
 * 
 * @example try/catch/finally
 *   >> (try
 *          "hello" ; This will get returned because it's the last evaluated expression
 *          (throw (new Error))
 *        (catch (e)
 *          (print "This will get called"))
 *        (finally
 *          (print "This always runs")))
 *   => "hello" ; The error is caught, so there is a return value
 * 
 * @example catch/finally symbols as keywords
 *   >> (try
 *          (throw (new Error))
 *        (:catch) ; This will work
 *        (:finally)) ; And so will this
 *   => nil
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
	} catch (e) {
		// Evaluate the `catch` expression if there is one.
		if (catchExpression) {
			var expression = [_S("lambda")].concat(catchExpression.slice(1));
			if (expression.length === 1) { // Add an arglist if there isn't one
				expression.push([]);
			}
			var callback = resolve(expression);
			callback(e);
		} else {
			// If there is no catch expression, throw the error for something
			// else to catch it (or not).
			throw e;
		}
	} finally {
		// Evaluate all expressions in the `finally` expression if there
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
 * Returns the function that the given symbol points to.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name getfunc
 * @lisp
 * @function
 * @member lisp.macros
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
 * <pre>
 * FIXME: This method sucks (actually, env.let sucks)
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
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
 * <pre>
 * TODO: Test me more
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name setq
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("setq", function () {
	var args = argsToArray(arguments);
	var symbol = args[0];
	var value  = resolve(args[1]);
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
 */
defmacro("progn", function (/* .. */) {
	var ret = null;
	for (var i = 0; i < arguments.length; i++) {
		ret = resolve(arguments[i]);
	}
	return ret;
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name cond
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @returns The value of the last evaluated expression, or nil.
 */
defmacro("cond", function () {
	for (var i = 0; i < arguments.length; i++) {
		var clause = arguments[i];
		if (clause.length === 0) {
			throw new Error("(cond) clauses must contain an expression to evaluate");
		}
		var condition = clause[0];
		if (!!resolve(condition)) {
			var ret;
			for (var j = 1; j < clause.length; j++) {
				ret = resolve(clause[j]);
			}
			return ret;
		}
	}
	return null;
});

// TODO: Create (case) or (switch) macro

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
	if (arguments.length < 2) {
		throw new Error("(if) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
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
	if (arguments.length === 0) {
		throw new Error("(not) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return !value;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
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
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
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
	if (arguments.length < 2) {
		throw new Error("(equal) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
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
	if (arguments.length < 2) {
		throw new Error("(not-equal) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return !equal(a, b);
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name ==
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("==", function () {
	if (arguments.length < 2) {
		throw new Error("(==) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a == b;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name ===
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("===", function () {
	if (arguments.length < 2) {
		throw new Error("(===) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a === b;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name !=
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("!=", function () {
	if (arguments.length < 2) {
		throw new Error("(!=) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a != b;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name !==
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("!==", function () {
	if (arguments.length < 2) {
		throw new Error("(!==) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a !== b;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * </pre>
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
 *     => false
 */
defmacro("<", function () {
	if (arguments.length < 2) {
		throw new Error("(<) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a < b;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * </pre>
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
 *     => false
 */
defmacro(">", function () {
	if (arguments.length < 2) {
		throw new Error("(>) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a > b;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * </pre>
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
 *     => false
 */
defmacro("<=", function () {
	if (arguments.length < 2) {
		throw new Error("(<=) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a <= b;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * </pre>
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
 *     => false
 */
defmacro(">=", function () {
	if (arguments.length < 2) {
		throw new Error("(>=) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return comparator(arguments, function (a, b) {
		return a >= b;
	});
});

/**
 * <pre>
 * Returns true if the given values === true.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-true
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-true", function () {
	if (arguments.length === 0) {
		throw new Error("(is-true) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return value === true;
	});
});

/**
 * <pre>
 * Returns true if the given values === false.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-false
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-false", function () {
	if (arguments.length === 0) {
		throw new Error("(is-false) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return value === false;
	});
});

/**
 * <pre>
 * Returns true if the given values === null.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-null
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-null", function () {
	if (arguments.length === 0) {
		throw new Error("(is-null) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return value === null;
	});
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-undefined
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-undefined", function () {
	if (arguments.length === 0) {
		throw new Error("(is-undefined) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return value === undefined;
	});
});

/**
 * <pre>
 * Returns true if the given values are strings.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-string
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-string", function () {
	if (arguments.length === 0) {
		throw new Error("(is-string) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return typeof(value) == "string";
	});
});

/**
 * <pre>
 * Returns true if the given values are numbers.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-number
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-number", function () {
	if (arguments.length === 0) {
		throw new Error("(is-number) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return typeof(value) == "number";
	});
});

/**
 * <pre>
 * Returns true if the given values are booleans.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-boolean
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-boolean", function () {
	if (arguments.length === 0) {
		throw new Error("(is-boolean) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return typeof(value) == "boolean";
	});
});

/**
 * <pre>
 * Returns true if the given values are functions.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-function
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-function", function () {
	if (arguments.length === 0) {
		throw new Error("(is-function) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return typeof(value) == "function";
	});
});

/**
 * <pre>
 * Returns true if the given values are objects.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-object
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-object", function () {
	if (arguments.length === 0) {
		throw new Error("(is-object) requires at least 1 argument");
	}
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
	if (arguments.length === 0) {
		throw new Error("(is-array) requires at least 1 argument");
	}
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
	if (arguments.length === 0) {
		throw new Error("(is-list) requires at least 1 argument");
	}
	return predicate(arguments, function (value) {
		return value instanceof Array;
	});
});

/**
 * <pre>
 * An expression for basic iteration over a list.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @name dolist
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("dolist", function (arglist /*, ... */) {
	if (arguments.length === 0) {
		throw new Error("(dolist) requires at least 1 argument (got " +
			arguments.length + ")");
	}
	if (!(arglist instanceof Array)) {
		throw new Error("(dolist) got invalid argument list: " + String(arglist));
	}
	if (arglist.length < 2 || arglist.length > 3) {
		throw new Error("(dolist) got invalid argument list. Requires at least " +
			"2 arguments and no more than 3 (got " + arglist.length + ")");
	}
	if (!(arglist[0] instanceof Symbol)) {
		throw new Error("(dolist) got invalid argument list. First argument " +
			"must be a symbol (got " + String(arglist[0]) + ")");
	}
	var symbol = arglist[0];
	var list = resolve(arglist[1]);
	var expressions = argsToArray(arguments).slice(1);
	var ret;
	if (!(list instanceof Array)) {
		throw new Error("(dolist) got invalid list argument: " + String(list));
	}
	
	lisp.env = new Env(lisp.env);
	for (var i = 0; i < list.length; i++) {
		lisp.env.let(symbol, list[i]);
		for (var j = 0; j < expressions.length; j++) {
			ret = resolve(expressions[j]);
		}
	}
	lisp.env = lisp.env.parent;
	
	if (arglist.length === 3) {
		return resolve(arglist[2]);
	} else {
		return ret;
	}
});

// TODO: Write macro (dotimes)

/**
 * <p>Functions that are defined for the lisp environment.
 * 
 * @name lisp.functions
 * @namespace
 */
var functions = {}; // This is just for documentation. It doesn't get used.

/**
 * <pre>
 * Applies the given arguments to the JavaScript eval function.
 * </pre>
 * 
 * @tested
 * 
 * @name jseval
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {[mixed]} rest
 *     The arguments to be applied to the JavaScript eval function.
 * @rest rest
 * 
 * @example eval some JSON
 *     >> (setq some-json "{'key1': 1, 'key2': 2}")
 *     => "{'key1': 1, 'key2': 2}"
 *     >> (jseval (concat "(" some-json ")"))
 *     => {
 *       "key1": 1,
 *       "key2": 2
 *     }
 * 
 * @example eval anything
 *     >> (setq my-1+ (jseval "new Function('x', 'return x + 1')"))
 *     => function anonymous(x) { ... }
 *     >> (my-1+ 2)
 *     => 3
 */
defun("jseval", function (/* &rest */) {
	return eval.apply(null, arguments);
});

/**
 * <pre>
 * Returns an instance of the given class, initialized with
 * the rest of the given arguments.
 * </pre>
 * 
 * @tested
 * 
 * @name new
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {function} cls
 *     The class to create a new instance of.
 * @param {[mixed]} rest
 *     The arguments to be passed to the class constructor.
 * @rest rest
 * 
 * @returns The new class instance.
 * 
 * @example Instantiate a class
 *     >> (new MyClass)
 *     => ; the MyClass instance
 * 
 * @example Instantiate a class with constructor arguments
 *     >> (to-string (new Error "My error message"))
 *     => "Error: My error message"
 */
defun("new", function (cls /*, &rest */) {
	if (arguments.length === 0) {
		throw new Error("(new) requires at least 1 argument");
	}
	if (typeof(cls) != "function") {
		throw new Error("(new) requires a function as its first argument " +
			"(got " + String(cls) + ")");
	}
	var args = argsToArray(arguments).slice(1);
	var argnames = args.map(function (item, i, thisObject) { return "args[" + i + "]"; });
	return eval("new cls(" + argnames.join(",") + ")");
});

/**
 * <pre>
 * Returns the value of "value instanceof cls".
 * </pre>
 * 
 * @tested
 * 
 * @name instanceof
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The value of "value instanceof cls".
 * 
 * @param {mixed} value
 *     The left side of the instanceof operator.
 * @param {function} cls
 *     The right side of the instanceof operator.
 * 
 * @example Basic example
 *     >> (instanceof (new Error) Error)
 *     => t
 *     >> (instanceof "not an error" Error)
 *     => f
 */
defun("instanceof", function (value, cls) {
	if (arguments.length !== 2) {
		throw new Error("(instanceof) requires 2 arguments (got " +
			arguments.length + ")");
	}
	if (typeof(cls) != "function") {
		throw new Error("(instanceof) requires a function as its second " +
			"argument (got " + String(cls) + ")");
	}
	return (value instanceof cls);
});

/**
 * <pre>
 * Throws the given object, or "new Error()" if no object is provided.
 * </pre>
 * 
 * @tested
 * 
 * @name throw
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} [value=new Error()]
 *     The value to throw.
 * @optional value
 * 
 * @returns Nothing. After throw'ing the stack is unwound to the
 *          nearest 'catch' block (or to the top).
 * 
 * @example Default Error
 *     >> (throw) ; Throws "new Error()"
 * 
 * @example Custom Error
 *     >> (throw (new Error "My Custom Error"))
 * 
 * @example Anything else you can normally throw
 *     >> (throw "a string")
 *     >> (throw 12)
 *     >> (throw (object :type "MyError"))
 *     >> (throw (array 1 2 3))
 */
defun("throw", function (value) {
	if (arguments.length > 1) {
		throw new Error("(throw) accepts 1 optional argument (got " +
			arguments.length + ")");
	}
	value = value || new Error(); // Throw "new Error()" if nothing is provided
	throw value;
});

/**
 * <pre>
 * Creates an array from the given arguments.
 * </pre>
 * 
 * @tested
 * 
 * @name array
 * @alias list
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The new array.
 * 
 * @param {[mixed]} rest
 *     The objects to join into an array.
 * @rest rest
 * 
 * @example An empty array
 *     >> (array)
 *     => []
 * 
 * @example An array with some elements
 *     >> (array 1 2 "three")
 *     => [1, 2, "three"]
 * 
 * @example A compound array
 *     >> (array (array 1 2) (array 3 4))
 *     => [[1, 2], [3, 4]]
 */
defun("array", function (/* &rest */) {
	return argsToArray(arguments);
});

/**
 * <pre>
 * Returns the given arguments as an array (this is an alias
 * for (array)).
 * </pre>
 * 
 * @tested
 * 
 * @name list
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The new array.
 * 
 * @param {[mixed]} rest
 *     The objects to join into an array.
 * @rest rest
 */
defun("list", function (/* &rest */) {
	return argsToArray(arguments);
});

/**
 * <pre>
 * Creates a JavaScript object, using the given arguments as a
 * property list to initialize the object. There must be an even
 * number of arguments -- one value for every key.
 * </pre>
 * 
 * @tested
 * 
 * @name object
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The new object.
 * 
 * @param {[mixed]} rest
 *     The property list from which to make the object.
 * @rest rest
 * 
 * @example An empty object
 *     >> (object)
 *     => {}
 * 
 * @example An object using keywords as keys
 *     >> (object :start 0 :end 10)
 *     => {start: 0, end: 10}
 * 
 * @example An object using strings as keys
 *     >> (object "start" 0 "end" 10)
 *     => {start: 0, end: 10}
 * 
 * @example A compound object
 *     >> (object :name "Joe"
 *                :parents (array (object :name "John")
 *                                (object :name "Jane")))
 *     => {name: "Joe",
 *         parents: [{name: "John"},
 *                   {name: "Jane"}]}
 */
defun("object", function (/* &rest */) {
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
 * <pre>
 * Returns a value from an object given a key (will work with
 * array indices as well).
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name getkey
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The value of "object[key]".
 * 
 * @param {mixed} key
 *     The key to access on the given object.
 * @param {object} object
 *     The object on which to access the given key.
 */
defun("getkey", function (key, object) {
	if (arguments.length !== 2) {
		throw new Error("(getkey) requires 2 arguments (got " +
			arguments.length + ")");
	}
	return object[key];
});

/**
 * <pre>
 * Sets a value on the given object using the given key.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name setkey
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The given value.
 * 
 * @param {mixed} key
 *     The key to set on the given object.
 * @param {object} object
 *     The object on which to set the given key.
 * @param {mixed} value
 *     The value to set to the given key on the given object.
 */
defun("setkey", function (key, object, value) {
	if (arguments.length !== 3) {
		throw new Error("(setkey) requires 3 arguments (got " +
			arguments.length + ")");
	}
	object[key] = value;
	return value;
});

/**
 * <pre>
 * Prints the given arguments to the console.
 * </pre>
 * 
 * @tested
 * 
 * @name print
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns nil.
 * 
 * @param {[mixed]} rest
 *     The objects to print.
 * @rest rest
 * 
 * @example Basic (print) expression
 *     >> (print "Hello" "Lisp")
 *     Hello Lisp
 *     => nil
 */
defun("print", function (/* &rest */) {
	// Do not remove this. This is not a debug statement.
	lisp.log.apply(null, arguments);
	return null;
});

/**
 * <pre>
 * Joins the given arguments together into one string.
 * </pre>
 * 
 * @tested
 * 
 * @name concat
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The string result of the joined arguments.
 * 
 * @param {[mixed]} rest
 *     The objects to join together into the returning string.
 * @rest rest
 * 
 * @example Concatenate a single object (if you really want to)
 *     >> (concat "Hello")
 *     => "Hello"
 * 
 * @example Concatenate many objects
 *     >> (let ((name "Lisp"))
 *          (concat "Hello: " name))
 *     => "Hello: Lisp"
 */
defun("concat", function (/* &rest */) {
	return argsToArray(arguments).join("");
});

/**
 * <pre>
 * Joins the given arguments together into one string, using
 * the first argument as the separator.
 * </pre>
 * 
 * @tested
 * 
 * @name join
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The string result of the joined arguments.
 * 
 * @param {mixed} sep
 *     The value that separates the objects in the returning string.
 * @param {[mixed]} rest
 *     The objects to join together into the returning string,
 *     separated by 'sep'.
 * @rest rest
 * 
 * @example Join items from a single list
 *     >> (join ", " (list "one" "two"))
 *     => "one, two"
 * 
 * @example Join items from multiple lists
 *     >> (let ((l1 (list "one" "two"))
 *              (l2 (list "three")))
 *          (join ", " l1 l2))
 *     => "one, two, three"
 */
defun("join", function (sep /*, &rest */) {
	if (arguments.length === 0) {
		throw new Error("(join) requires at least 1 argument");
	}
	var args = argsToArray(arguments);
	var rest = args.slice(1);
	for (var i = 0; i < rest.length; i++) {
		var arg = rest[i];
		if (!(arg instanceof Array)) {
			throw new Error("(join) got an invalid argument: '" +
			    String(arg) + "' is not a list");
		}
	}
	var list = rest.reduce(function (a, b) { return a.concat(b); });
	return list.join(sep);
});

/**
 * <pre>
 * Returns the type of the given value (the result of "typeof(value)").
 * </pre>
 * 
 * <li>TODO: Add examples
 * 
 * @tested
 * 
 * @name typeof
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} value
 *     The value whose type is returned.
 */
defun("typeof", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(typeof) requires 1 argument (got " +
			arguments.length + ")");
	}
	return typeof(value);
});

/**
 * <pre>
 * Converts the given value to a string.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name to-string
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} value
 *     The value to turn into a string.
 */
defun("to-string", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(to-string) requires 1 argument (got " +
			arguments.length + ")");
	}
	return String(value);
});

/**
 * <pre>
 * Converts the given value to a number.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name to-number
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} value
 *     The value to turn into a number.
 */
defun("to-number", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(to-number) requires 1 argument (got " +
			arguments.length + ")");
	}
	return Number(value);
});

/**
 * <pre>
 * Converts the given value to a number.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name to-boolean
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} value
 *     The value to turn into a boolean.
 */
defun("to-boolean", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(to-boolean) requires 1 argument (got " +
			arguments.length + ")");
	}
	return Boolean(value);
});

/**
 * <pre>
 * Converts the given value to a json representation of that value.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name to-json
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} value
 *     The value whose json representation is returned.
 * @param {boolean} pretty
 *     Whether to pretty-print the resulting json string.
 * @param {boolean} levels
 *     The amount of levels deep to go before stopping.
 * @optional pretty
 * @optional levels
 */
defun("to-json", function (value, pretty, levels) {
	return toJSON(value, pretty, levels);
});

/**
 * <pre>
 * Converts the given string to uppercase.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name to-upper
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of "string.toUpperCase()".
 * 
 * @param {string} string
 *     The string to convert to uppercase.
 */
defun("to-upper", function (string) {
	if (arguments.length !== 1) {
		throw new Error("(to-upper) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (typeof(string) != "string") {
		throw new Error("(to-upper) requires a string argument (got " +
			String(string) + ")");
	}
	return string.toUpperCase();
});

/**
 * <pre>
 * Converts the given string to uppercase.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name to-lower
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of "string.toLowerCase()".
 * 
 * @param {string} string
 *     The string to convert to lowercase.
 */
defun("to-lower", function (string) {
	if (arguments.length !== 1) {
		throw new Error("(to-lower) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (typeof(string) != "string") {
		throw new Error("(to-lower) requires a string argument (got " +
			String(string) + ")");
	}
	return string.toLowerCase();
});

/**
 * <pre>
 * Reduces the given arguments on the / operator.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name /
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of reducing all of the given arguments
 *          on the division operator.
 * 
 * @param {[number]} rest
 *     The numbers to divide into each other.
 * @rest rest
 */
defun("/", function (/* &rest */) {
	if (arguments.length === 0) {
		throw new Error("(/) requires at least 1 argument");
	}
	var args = argsToArray(arguments);
	// This is to emulate common lisp, where dividing one number
	// is the same as dividing 1 by that number.
	if (args.length === 1) {
		args = [1].concat(args);
	}
	return args.reduce(function (a, b) {
		return a / b;
	});
});

/**
 * <pre>
 * Reduces the given arguments on the * operator.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name *
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of reducing all of the given arguments
 *          on the multiplication operator.
 * 
 * @param {[number]} rest
 *     The numbers to multiply by each other.
 * @rest rest
 */
defun("*", function (/* &rest */) {
	var args = argsToArray(arguments);
	if (args.length === 0) {
		args = [1];
	}
	return args.reduce(function (a, b) {
		return a * b;
	});
});

/**
 * <pre>
 * Reduces the given arguments on the + operator.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name +
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of reducing all of the given arguments
 *          on the addition operator.
 * 
 * @param {[number]} rest
 *     The numbers to add to each other.
 * @rest rest
 */
defun("+", function (/* &rest */) {
	var args = argsToArray(arguments);
	if (args.length === 0) {
		args = [0];
	}
	return args.reduce(function (a, b) {
		return a + b;
	});
});

/**
 * <pre>
 * Reduces the given arguments on the - operator.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name -
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of reducing all of the given arguments
 *          on the subtraction operator.
 * 
 * @param {[number]} rest
 *     The numbers to subtract by each other.
 * @rest rest
 */
defun("-", function (/* &rest */) {
	if (arguments.length === 0) {
		throw new Error("(-) requires at least 1 argument");
	}
	var args = argsToArray(arguments);
	if (args.length === 1) {
		args = [0].concat(args);
	}
	return args.reduce(function (a, b) {
		return a - b;
	});
});

/**
 * <pre>
 * Reduces the given arguments on the % operator.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name %
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of reducing all of the given arguments
 *          on the modulo operator.
 * 
 * @param {[number]} rest
 *     The numbers on which to use the modulo operator.
 * @rest rest
 */
defun("%", function () {
	if (arguments.length < 2) {
		throw new Error("(%) requires at least 2 arguments (got " +
			arguments.length + ")");
	}
	return argsToArray(arguments).reduce(function (a, b) {
		return a % b;
	});
});

/**
 * <pre>
 * Adds 1 to the given value.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name 1+
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of adding 1 to the given number.
 * 
 * @param {number} number
 *     The number to add 1 to.
 */
defun("1+", function (number) {
	if (arguments.length !== 1) {
		throw new Error("(1+) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (typeof(number) != "number") {
		throw new Error("(1+) requires a number argument (got " +
			number + ")");
	}
	return number + 1;
});

/**
 * <pre>
 * Subtracts 1 from the given value.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name 1-
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @return The result of subtracting 1 from the given number.
 * 
 * @param {number} number
 *     The number to subtract 1 from.
 */
defun("1-", function (number) {
	if (arguments.length !== 1) {
		throw new Error("(1-) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (typeof(number) != "number") {
		throw new Error("(1-) requires a number argument (got " +
			number + ")");
	}
	return number - 1;
});

/**
 * <pre>
 * Calls sprintf (found in the vendor section) with the supplied arguments.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name format
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @return The result of applying the given arguments to sprintf
 *         (in the vendor section) if print evaluates to true,
 *         otherwise nil.
 * 
 * @param {boolean} print
 *     Whether to print the result, or return it.
 * @param {string} format
 *     The string format to which to apply the given arguments.
 * @param {[mixed]} rest
 *     The arguments to apply to the given format
 * @rest rest
 */
defun("format", function (print, format /*, &rest */) {
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

/**
 * <pre>
 * Run each of the items in the given list through the given
 * function and returns a new list with the given return values.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name map
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @return The result of mapping the given list to the given function.
 * 
 * @param {function} func
 *     The function to apply to each object in the given list.
 * @param {Array} list
 *     The list of items to pass to the given function.
 */
defun("map", function (func, list) {
	if (arguments.length !== 2) {
		throw new Error("(map) requires 2 arguments (got " +
			arguments.length + ")");
	}
	if (typeof(func) != "function") {
		throw new Error("(map) requires a function as its first argument (got " +
			String(func) + ")");
	}
	if (!(list instanceof Array)) {
		throw new Error("(map) requires a list as its second argument (got " +
			String(list) + ")");
	}
	var newlist = [];
	for (var i = 0; i < list.length; i++) {
		newlist.push(func(list[i]));
	}
	return newlist;
});

/**
 * <pre>
 * Returns an object containing the values of each property
 * in the given list on the given object.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name props
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @return A new object containing only the values on the given
 *         object specified by the list of keys.
 * 
 * @param {object} object
 *     The object containing the values to return (specified by the
 *     given list of keys).
 * @param {Array} list
 *     The list of keys specifying which values to return in the
 *     resulting object.
 */
defun("props", function (object, list) {
	if (arguments.length !== 2) {
		throw new Error("(props) requires 2 arguments (got " +
			arguments.length + ")");
	}
	if (!(list instanceof Array)) {
		throw new Error("(props) requires a list as its second argument " +
			"(got " + String(list) + ")");
	}
	var newObject = {};
	for (var i = 0; i < list.length; i++) {
		var key = list[i];
		newObject[key] = object[key];
	}
	return newObject;
});

return {
	VERSION: "0.0.1",
	
	Env: Env,
	Macro: Macro,
	Symbol: Symbol,
	Keyword: Keyword,
	
	env: ROOT_ENV,
	
	exception: {
		StreamException: StreamException,
		StreamEOFException: StreamEOFException
	},
	
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
	
	load: function (source, callback) {
		makeRequest(source, function (script) {
			lisp.eval(script);
			if (callback) {
				callback(source);
			}
		});
	},
	
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
if ((typeof(window) == "undefined") &&
	(typeof(global) == "object") && global && // Make sure it isn't null
	(typeof(require) == "function") &&
	(typeof(exports) == "object") && exports) {
	// We are probably running in node.js now.
	// FIXME: Find a better way to tell we're running in node.js
	for (var key in lisp) {
		if (lisp.hasOwnProperty(key)) {
			exports[key] = lisp[key];
		}
	}
	
	lisp.log = require("sys").puts;
}

