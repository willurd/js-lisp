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
function toJSON (object) {
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
			var json = '[';

			for (var i = 0; i < object.length; i++)
				json += toJSON(object[i]) + ', ';

			return json.replace(/, $/, '') + ']';
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
			var json = '{';

			for (var key in object)
				if (object.hasOwnProperty(key))
					json += key + ': ' + toJSON(object[key]) + ', ';

			return json.replace(/, $/, '') + '}';
		}
	case 'function':
		return object.toString();
	case 'string':
		return '"' + object.replace(/"/g, '\\"') + '"';
	case 'number':
		return object;
	case 'boolean':
		return object.toString();
	}
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
	
	/**
	 * @ignore
	 */
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

/**
 * <p>The exception type thrown when any parse error occurs.</p>
 * 
 * @class
 * @extends Class
 */
parse.ParserException = Class.extend("ParserException", {
	/**
	 * <p>Returns a string representation of the exception.</p>
	 * 
	 * @function
	 * @name toString
	 * @member parse.ParserException
	 */
	toString: function () {
		return "ParserException: " + message;
	}
});

/**
 * Parses a lisp script, which can be any number of root-level expression,
 * into an array of ASTs representing those expressions.
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
		for (var i = 0; i < parse.NUMBER_FORMATS.length; i++) {
			var format = parse.NUMBER_FORMATS[i];
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
// parse.object = function (stream) {
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
// };

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
	return new Symbol(symbol);
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
		for (var i = 0; i < parse.NUMBER_FORMATS.length; i++) {
			var format = parse.NUMBER_FORMATS[i];
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
	stream.next();
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
 * Creates an anonymous function with the first (required) expression
 * as its arglist and which executes the rest of the expressions
 * when called.
 * 
 * @returns The created function.
 */
defmacro("lambda", function (arglist /*, ... */) {
	var env  = new Env(lisp.env);
	var args = argsToArray(arguments);
	return (function (env, args) {
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
 * Provides JavaScript's try/catch feature to the lisp environment.
 * 
 * Note: (catch) and (finally) must be the last expressions in the
 * (try) expression, and (catch) must come before (finally). The only
 * valid uses of (catch) and (finally) in a (try) expression are:
 * 
 *     - (try ...) ; Not using either of them
 *     - (try ... (catch ...))
 *     - (try ... (finally ...))
 *     - (try ... (catch ...) (finally ...))
 * 
 * @returns The return value of the last evaluated expression.
 * 
 * @tested
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
			catchExpression[0] = new Symbol("lambda"); // Just make it a lambda
			if (catchExpression.length === 1) { // Add an arglist if there isn't one
				catchExpression.push([]);
			}
			var callback = resolve(catchExpression);
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
 * Simply executes all of the given expressions in order. This
 * is mainly for being able to execute multiple expressions inside
 * of places in other macros/functions where only one expression
 * can go.
 * 
 * @returns The return value of the last expression, or nil if there
 *         are no expression.
 * 
 * @tested
 */
defmacro("progn", function (/* .. */) {
	var ret = null;
	for (var i = 0; i < arguments.length; i++) {
		ret = resolve(arguments[i]);
	}
	return ret;
});

/**
 * If the first expression evaluates to true in a boolean context,
 * this macro evaluates and returns the result of the second
 * expression, otherwise it evaluates all of the remaining expression
 * and returns the return value of the last one.
 * 
 * @returns The return value of either the second or last expression, or
 *         nil if testExpression evaluates to false and there are no
 *         remaining expressions to evaluate.
 * 
 * @tested
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
 * Executes the rest of the arguments if the first argument
 * is true.
 * 
 * @returns The return value of the last expression.
 * 
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
	return predicate(arguments, function (value) {
		return !value;
	});
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
	if (arguments.length === 0) {
		return true;
	}
	return predicate(arguments, function (value) {
		return !!value;
	});
});

/**
 * 
 */
defmacro("==", function () {
	if (arguments.length < 2) {
		throw new Error("Macro '==' requires at least 2 arguments");
	}
	return comparator(arguments, function (a, b) {
		return a == b;
	});
});

/**
 * 
 */
defmacro("===", function () {
	if (arguments.length < 2) {
		throw new Error("Macro '===' requires at least 2 arguments");
	}
	return comparator(arguments, function (a, b) {
		return a === b;
	});
});

/**
 * 
 */
defmacro("!=", function () {
	if (arguments.length < 2) {
		throw new Error("Macro '!=' requires at least 2 arguments");
	}
	return comparator(arguments, function (a, b) {
		return a != b;
	});
});

/**
 * 
 */
defmacro("!==", function () {
	if (arguments.length < 2) {
		throw new Error("Macro '!==' requires at least 2 arguments");
	}
	return comparator(arguments, function (a, b) {
		return a !== b;
	});
});

/**
* Examples:
*    * (< x y)
*    * (< -1 0 1 2 3)
 */
defmacro("<", function () {
	if (arguments.length < 2) {
		throw new Error("Macro '<' requires at least 2 arguments");
	}
	return comparator(arguments, function (a, b) {
		return a < b;
	});
});

/**
 * Examples:
 *    * (> x y)
 *    * (> 3 2 1 0 -1)
 */
defmacro(">", function () {
	if (arguments.length < 2) {
		throw new Error("Macro '>' requires at least 2 arguments");
	}
	return comparator(arguments, function (a, b) {
		return a > b;
	});
});

/**
 * Examples:
 *    * (<= x y)
 *    * (<= 1 1 2 3 4)
 */
defmacro("<=", function () {
	if (arguments.length < 2) {
		throw new Error("Macro '>' requires at least 2 arguments");
	}
	return comparator(arguments, function (a, b) {
		return a <= b;
	});
});

/**
 * Examples:
 *    * (>= x y)
 *    * (>= 4 3 2 2 1)
 */
defmacro(">=", function () {
	if (arguments.length < 2) {
		throw new Error("Macro '>' requires at least 2 arguments");
	}
	return comparator(arguments, function (a, b) {
		return a >= b;
	});
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
/**
 * Functions that are defined for the lisp environment.
 * 
 * @name lisp.functions
 * @namespace
 */
var functions = {}; // This is just for documentation. It doesn't get used.

/**
 * Returns an instance of the given class, initialized with
 * the rest of the given arguments.
 * 
 * @function
 * @name new
 * @member lisp.functions
 * 
 * @param {Class} Class
 *     The class to create a new instance of.
 * @param {[mixed]} rest
 *     The arguments to be passed to the class constructor.
 * 
 * @returns The new class instance.
 * 
 * @example Instantiate a class
 *     >> (new MyClass)
 *     => ; the MyClass instance
 *
 * @example Instantiate a class with constructor arguments
 *     >> (new Error "My error message")
 *     => ; the Error instance
 */
defun("new", function (Class /*, ... */) {
	if (arguments.length === 0) {
		throw new Error("(new) requires at least 1 argument");
	}
	var args = argsToArray(arguments).slice(1);
	var argnames = args.map(function (item, i, thisObject) { return "args[" + i + "]"; });
	return eval("new Class(" + argnames.join(",") + ")");
});

/**
 * Throws the given object, or new Error() if no object is
 * provided.
 * 
 * @function
 * @name throw
 * @member lisp.functions
 * 
 * @param {mixed} object The object to throw. Defaults to new Error().
 * 
 * @returns Nothing. After throw'ing the stack is unwided to the
 *          nearest 'catch' block.
 * 
 * @tested
 * 
 * @example Basic Error
 *   >> (throw) ; Throws "new Error()"
 * 
 * @example Custom Error
 *   >> (throw (new Error "My Custom Error"))
 * 
 * @example Anything else you can normally throw
 *   >> (throw "a string")
 *   >> (throw 12)
 *   >> (throw (object :type "MyError"))
 *   >> (throw (array 1 2 3))
 */
defun("throw", function (object) {
	if (arguments.length > 1) {
		throw new Error("(throw) accepts 1 optional argument (got " +
			arguments.length + ")");
	}
	object = object || new Error(); // Throw "new Error()" if nothing is provided
	throw object;
});

/**
 * Returns the given arguments as a list.
 * 
 * @function
 * @name list
 * @member lisp.functions
 */
defun("list", function (/* ... */) {
	return argsToArray(arguments);
});

/**
 * Creates a JavaScript object using the given arguments as a
 * property list to initialize the object. There must be an even
 * number of arguments -- one value for every key.
 * 
 * @function
 * @name object
 * @member lisp.functions
 * 
 * @returns The new object.
 * 
 * @tested
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
 * Creates an array from the given arguments.
 * 
 * @function
 * @name array
 * @member lisp.functions
 * 
 * @returns The new array.
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
defun("array", function () {
	return argsToArray(arguments);
});

/**
 * Returns a value from an object given a key (will work with
 * array indices as well).
 * 
 * @function
 * @name getkey
 * @member lisp.functions
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
 * 
 * @function
 * @name setkey
 * @member lisp.functions
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
 * 
 * @function
 * @name print
 * @member lisp.functions
 * 
 * @returns nil.
 * 
 * @tested
 * 
 * @example Basic (print) expression
 *     >> (print "Hello" "Lisp")
 *     Hello Lisp
 *     => nil
 */
defun("print", function () {
	// Do not remove this. This is not a debug statement.
	lisp.log.apply(null, arguments);
	return null;
});

/**
 * Joins the given arguments together into one string.
 * 
 * @function
 * @name concat
 * @member lisp.functions
 * 
 * @returns The string result of the joined arguments.
 * 
 * @tested
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
defun("concat", function () {
	return argsToArray(arguments).join("");
});

/**
 * Joins the given arguments together into one string, using
 * the first argument as the separator.
 * 
 * @function
 * @name join
 * @member lisp.functions
 * 
 * @returns The string result of the joined arguments.
 * 
 * @tested
 * 
 * @example Join items from a single list
 *   >> (join ", " (list "one" "two"))
 *   => "one, two"
 * 
 * @example Join items from multiple lists
 *   >> (let ((l1 (list "one" "two"))
 *            (l2 (list "three")))
 *        (join ", " l1 l2))
 *   => "one, two, three"
 */
defun("join", function () {
	if (arguments.length === 0) {
		throw new Error("(join) requires at least 1 argument");
	}
	var args = argsToArray(arguments);
	var sep  = args[0];
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
 * Returns the type of the given value.
 * 
 * @function
 * @name typeof
 * @member lisp.functions
 * 
 * @tested
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
 * @function
 * @name to-string
 * @member lisp.functions
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
 * @function
 * @name to-number
 * @member lisp.functions
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
 * @function
 * @name to-boolean
 * @member lisp.functions
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
 * Converts the given value to a json representation of that value.
 * 
 * @function
 * @name to-json
 * @member lisp.functions
 */
defun("to-json", function (object) {
	if (arguments.length !== 1) {
		throw new Error("(to-json) requires 1 argument (got " +
			arguments.length + ")");
	}
	return toJSON(object);
});

/**
 * Converts the given string to uppercase.
 * 
 * @function
 * @name to-upper
 * @member lisp.functions
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
 * @function
 * @name to-lower
 * @member lisp.functions
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
 * 
 * @function
 * @name /
 * @member lisp.functions
 * 
 * @tested
 */
defun("/", function () {
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
 * Reduces the given arguments on the * operator.
 * 
 * @function
 * @name *
 * @member lisp.functions
 * 
 * @tested
 */
defun("*", function () {
	var args = argsToArray(arguments);
	if (args.length === 0) {
		args = [1];
	}
	return args.reduce(function (a, b) {
		return a * b;
	});
});

/**
 * Reduces the given arguments on the + operator.
 * 
 * @function
 * @name +
 * @member lisp.functions
 * 
 * @tested
 */
defun("+", function () {
	var args = argsToArray(arguments);
	if (args.length === 0) {
		args = [0];
	}
	return args.reduce(function (a, b) {
		return a + b;
	});
});

/**
 * Reduces the given arguments on the - operator.
 * 
 * @function
 * @name -
 * @member lisp.functions
 * 
 * @tested
 */
defun("-", function () {
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
 * Reduces the given arguments on the % operator.
 * 
 * @function
 * @name %
 * @member lisp.functions
 * 
 * @tested
 */
defun("%", function () {
	if (arguments.length !== 2) {
		throw new Error("(%) requires 2 arguments (got " +
			arguments.length + ")");
	}
	return argsToArray(arguments).reduce(function (a, b) {
		return a % b;
	});
});

/**
 * Adds 1 to the given value.
 * 
 * @function
 * @name 1+
 * @member lisp.functions
 * 
 * @tested
 */
defun("1+", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(1+) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (typeof(value) != "number") {
		throw new Error("(1+) requires a number argument (got " +
			value + ")");
	}
	return Number(value) + 1;
});

/**
 * Subtracts 1 from the given value.
 * 
 * @function
 * @name 1-
 * @member lisp.functions
 * 
 * @tested
 */
defun("1-", function (value) {
	if (arguments.length !== 1) {
		throw new Error("(1-) requires 1 argument (got " +
			arguments.length + ")");
	}
	if (typeof(value) != "number") {
		throw new Error("(1-) requires a number argument (got " +
			value + ")");
	}
	return Number(value) - 1;
});

/**
 * Calls {@link sprintf} (found in the vendor section) with the
 * supplied arguments.
 * 
 * @function
 * @name format
 * @member lisp.functions
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
