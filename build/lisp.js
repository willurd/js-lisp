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

    var regex = /%%|%(\d+\$)?([-+\'#0 ]*)(\*\d+\$|\*|\d+)?(\.(\*\d+\$|\*|\d+))?([lscboxXuidfegEG])/g;
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
			case 'l': return toLisp(value);
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
					if (object[key] == global) {
						value = "[global]";
					} else {
						value = toJSON(object[key], pretty, levels, level+1);
					}
					json += prefix + singleprefix + '"' + key + '": ' + value;
					json += ', ' + newline;
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
 * Returns a lispy string representation of the given value.
 */
function toLisp (object) {
	if (object === null) {
		return "nil";
	}
	if (object === true) {
		return "t";
	}
	if (object === false) {
		return "f";
	}
	if (object instanceof Symbol) {
		return String(object);
	}
	if (object instanceof Keyword) {
		return ":" + String(object);
	}
	if (object instanceof Array) {
		return "(" + object.map(toLisp).join(" ") + ")";
	}
	return toJSON(object, true);
}

var ArgumentError = Class.extend({
	init: function (message) {
		this.message = message;
	},
	toString: function () {
		return "ArgumentError: " + this.message;
	}
});

function assert (assertion, errorString) {
	if (!assertion) {
		throw new Error(errorString || "");
	}
}

function withNewEnv (callable) {
	var tempEnv = lisp.env;
	lisp.env = new Env(lisp.env);
	var ret = callable();
	lisp.env = tempEnv;
	return ret;
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

function deepCopyArray (array) {
	var newArray = [];
	var item;
	for (var i = 0; i < array.length; i++) {
		item = array[i];
		if (item instanceof Array) {
			item = deepCopyArray(item);
		}
		newArray.push(item);
	}
	return newArray;
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
	}
	
	assert(request.status != 404, "Trying to load lisp script that " +
		"does not exist: " + url);
}

function times (string, num) {
	var ret = '';
	for (var i = 0; i < num; i++) {
		ret += string;
	}
	return ret;
}

// From: http://note19.com/2007/05/27/javascript-guid-generator/
// This is for gensym().
// I know these aren't real guids, and i'm sure in a million years
// someone might actually be unlucky enough to witness a conflict
// with gensym(), but this seems safe enough for the time being.
function S4() {
   return (((1+Math.random())*0x10000)|0).toString(16).substring(1);
}
function guid() {
   return (S4()+S4()+"-"+S4()+"-"+S4()+"-"+S4()+"-"+S4()+S4()+S4());
}

// Used primarly for auto-generated code, so you don't end up pummeling
// any vars in the current scope.
function gensym () {
	return _S(guid().replace(/\-/g,'#')); // Be extra random :)
}

// Used for (char ...) (for the #\ special operator)
var _char_table = {
	"Nul": 0,
	"Soh": 1,
	"Stx": 2,
	"Etx": 3,
	"Eot": 4,
	"Enq": 5,
	"Ack": 6,
	"Bel": 7,
	"Backspace": 8,
	"Tab": 9,
	"Newline": 10,
	"Vt": 11,
	"Page": 12,
	"Return": 13,
	"So": 14,
	"Si": 15,
	"Dle": 16,
	"Dc1": 17,
	"Dc2": 18,
	"Dc3": 19,
	"Dc4": 20,
	"Nak": 21,
	"Syn": 22,
	"Etb": 23,
	"Can": 24,
	"Em": 25,
	"Sub": 26,
	"Esc": 27,
	"Fs": 28,
	"Gs": 29,
	"Rs": 30,
	"Us": 31,
	"Space": 32,
	"!": 33,
	"\"": 34,
	"#": 35,
	"$": 36,
	"%": 37,
	"&": 38,
	"'": 39,
	"(": 40,
	")": 41,
	"*": 42,
	"+": 43,
	",": 44,
	"-": 45,
	".": 46,
	"/": 47,
	"0": 48,
	"1": 49,
	"2": 50,
	"3": 51,
	"4": 52,
	"5": 53,
	"6": 54,
	"7": 55,
	"8": 56,
	"9": 57,
	":": 58,
	";": 59,
	"<": 60,
	"=": 61,
	">": 62,
	"?": 63,
	"@": 64,
	"A": 65,
	"B": 66,
	"C": 67,
	"D": 68,
	"E": 69,
	"F": 70,
	"G": 71,
	"H": 72,
	"I": 73,
	"J": 74,
	"K": 75,
	"L": 76,
	"M": 77,
	"N": 78,
	"O": 79,
	"P": 80,
	"Q": 81,
	"R": 82,
	"S": 83,
	"T": 84,
	"U": 85,
	"V": 86,
	"W": 87,
	"X": 88,
	"Y": 89,
	"Z": 90,
	"[": 91,
	"\\": 92,
	"]": 93,
	"^": 94,
	"_": 95,
	"`": 96,
	"a": 97,
	"b": 98,
	"c": 99,
	"d": 100,
	"e": 101,
	"f": 102,
	"g": 103,
	"h": 104,
	"i": 105,
	"j": 106,
	"k": 107,
	"l": 108,
	"m": 109,
	"n": 110,
	"o": 111,
	"p": 112,
	"q": 113,
	"r": 114,
	"s": 115,
	"t": 116,
	"u": 117,
	"v": 118,
	"w": 119,
	"x": 120,
	"y": 121,
	"z": 122,
	"{": 123,
	"|": 124,
	"}": 125,
	"~": 126,
	"Rubout": 127,
	"C80": 128,
	"C81": 129,
	"Break-Permitted": 130,
	"No-Break-Permitted": 131,
	"C84": 132,
	"Next-Line": 133,
	"Start-Selected-Area": 134,
	"End-Selected-Area": 135,
	"Character-Tabulation-Set": 136,
	"Character-Tabulation-With-Justification": 137,
	"Line-Tabulation-Set": 138,
	"Partial-Line-Forward": 139,
	"Partial-Line-Backward": 140,
	"Reverse-Linefeed": 141,
	"Single-Shift-Two": 142,
	"Single-Shift-Three": 143,
	"Device-Control-String": 144,
	"Private-Use-One": 145,
	"Private-Use-Two": 146,
	"Set-Transmit-State": 147,
	"Cancel-Character": 148,
	"Message-Waiting": 149,
	"Start-Guarded-Area": 150,
	"End-Guarded-Area": 151,
	"Start-String": 152,
	"C99": 153,
	"Single-Character-Introducer": 154,
	"Control-Sequence-Introducer": 155,
	"String-Terminator": 156,
	"Operating-System-Command": 157,
	"Privacy-Message": 158,
	"Application-Program-Command": 159,
	"NO-BREAK_SPACE": 160,
	"INVERTED_EXCLAMATION_MARK": 161,
	"CENT_SIGN": 162,
	"POUND_SIGN": 163,
	"CURRENCY_SIGN": 164,
	"YEN_SIGN": 165,
	"BROKEN_BAR": 166,
	"SECTION_SIGN": 167,
	"DIAERESIS": 168,
	"COPYRIGHT_SIGN": 169,
	"FEMININE_ORDINAL_INDICATOR": 170,
	"LEFT-POINTING_DOUBLE_ANGLE_QUOTATION_MARK": 171,
	"NOT_SIGN": 172,
	"SOFT_HYPHEN": 173,
	"REGISTERED_SIGN": 174,
	"MACRON": 175,
	"DEGREE_SIGN": 176,
	"PLUS-MINUS_SIGN": 177,
	"SUPERSCRIPT_TWO": 178,
	"SUPERSCRIPT_THREE": 179,
	"ACUTE_ACCENT": 180,
	"MICRO_SIGN": 181,
	"PILCROW_SIGN": 182,
	"MIDDLE_DOT": 183,
	"CEDILLA": 184,
	"SUPERSCRIPT_ONE": 185,
	"MASCULINE_ORDINAL_INDICATOR": 186,
	"RIGHT-POINTING_DOUBLE_ANGLE_QUOTATION_MARK": 187,
	"VULGAR_FRACTION_ONE_QUARTER": 188,
	"VULGAR_FRACTION_ONE_HALF": 189,
	"VULGAR_FRACTION_THREE_QUARTERS": 190,
	"INVERTED_QUESTION_MARK": 191,
	"LATIN_CAPITAL_LETTER_A_WITH_GRAVE": 192,
	"LATIN_CAPITAL_LETTER_A_WITH_ACUTE": 193,
	"LATIN_CAPITAL_LETTER_A_WITH_CIRCUMFLEX": 194,
	"LATIN_CAPITAL_LETTER_A_WITH_TILDE": 195,
	"LATIN_CAPITAL_LETTER_A_WITH_DIAERESIS": 196,
	"LATIN_CAPITAL_LETTER_A_WITH_RING_ABOVE": 197,
	"LATIN_CAPITAL_LETTER_AE": 198,
	"LATIN_CAPITAL_LETTER_C_WITH_CEDILLA": 199,
	"LATIN_CAPITAL_LETTER_E_WITH_GRAVE": 200,
	"LATIN_CAPITAL_LETTER_E_WITH_ACUTE": 201,
	"LATIN_CAPITAL_LETTER_E_WITH_CIRCUMFLEX": 202,
	"LATIN_CAPITAL_LETTER_E_WITH_DIAERESIS": 203,
	"LATIN_CAPITAL_LETTER_I_WITH_GRAVE": 204,
	"LATIN_CAPITAL_LETTER_I_WITH_ACUTE": 205,
	"LATIN_CAPITAL_LETTER_I_WITH_CIRCUMFLEX": 206,
	"LATIN_CAPITAL_LETTER_I_WITH_DIAERESIS": 207,
	"LATIN_CAPITAL_LETTER_ETH": 208,
	"LATIN_CAPITAL_LETTER_N_WITH_TILDE": 209,
	"LATIN_CAPITAL_LETTER_O_WITH_GRAVE": 210,
	"LATIN_CAPITAL_LETTER_O_WITH_ACUTE": 211,
	"LATIN_CAPITAL_LETTER_O_WITH_CIRCUMFLEX": 212,
	"LATIN_CAPITAL_LETTER_O_WITH_TILDE": 213,
	"LATIN_CAPITAL_LETTER_O_WITH_DIAERESIS": 214,
	"MULTIPLICATION_SIGN": 215,
	"LATIN_CAPITAL_LETTER_O_WITH_STROKE": 216,
	"LATIN_CAPITAL_LETTER_U_WITH_GRAVE": 217,
	"LATIN_CAPITAL_LETTER_U_WITH_ACUTE": 218,
	"LATIN_CAPITAL_LETTER_U_WITH_CIRCUMFLEX": 219,
	"LATIN_CAPITAL_LETTER_U_WITH_DIAERESIS": 220,
	"LATIN_CAPITAL_LETTER_Y_WITH_ACUTE": 221,
	"LATIN_CAPITAL_LETTER_THORN": 222,
	"LATIN_SMALL_LETTER_SHARP_S": 223,
	"LATIN_SMALL_LETTER_A_WITH_GRAVE": 224,
	"LATIN_SMALL_LETTER_A_WITH_ACUTE": 225,
	"LATIN_SMALL_LETTER_A_WITH_CIRCUMFLEX": 226,
	"LATIN_SMALL_LETTER_A_WITH_TILDE": 227,
	"LATIN_SMALL_LETTER_A_WITH_DIAERESIS": 228,
	"LATIN_SMALL_LETTER_A_WITH_RING_ABOVE": 229,
	"LATIN_SMALL_LETTER_AE": 230,
	"LATIN_SMALL_LETTER_C_WITH_CEDILLA": 231,
	"LATIN_SMALL_LETTER_E_WITH_GRAVE": 232,
	"LATIN_SMALL_LETTER_E_WITH_ACUTE": 233,
	"LATIN_SMALL_LETTER_E_WITH_CIRCUMFLEX": 234,
	"LATIN_SMALL_LETTER_E_WITH_DIAERESIS": 235,
	"LATIN_SMALL_LETTER_I_WITH_GRAVE": 236,
	"LATIN_SMALL_LETTER_I_WITH_ACUTE": 237,
	"LATIN_SMALL_LETTER_I_WITH_CIRCUMFLEX": 238,
	"LATIN_SMALL_LETTER_I_WITH_DIAERESIS": 239,
	"LATIN_SMALL_LETTER_ETH": 240,
	"LATIN_SMALL_LETTER_N_WITH_TILDE": 241,
	"LATIN_SMALL_LETTER_O_WITH_GRAVE": 242,
	"LATIN_SMALL_LETTER_O_WITH_ACUTE": 243,
	"LATIN_SMALL_LETTER_O_WITH_CIRCUMFLEX": 244,
	"LATIN_SMALL_LETTER_O_WITH_TILDE": 245,
	"LATIN_SMALL_LETTER_O_WITH_DIAERESIS": 246,
	"DIVISION_SIGN": 247,
	"LATIN_SMALL_LETTER_O_WITH_STROKE": 248,
	"LATIN_SMALL_LETTER_U_WITH_GRAVE": 249,
	"LATIN_SMALL_LETTER_U_WITH_ACUTE": 250,
	"LATIN_SMALL_LETTER_U_WITH_CIRCUMFLEX": 251,
	"LATIN_SMALL_LETTER_U_WITH_DIAERESIS": 252,
	"LATIN_SMALL_LETTER_Y_WITH_ACUTE": 253,
	"LATIN_SMALL_LETTER_THORN": 254,
	"LATIN_SMALL_LETTER_Y_WITH_DIAERESIS": 255
};

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
		assert(typeof(data) === "string", "Invalid object as " +
			"StringStream input: " + data);
		
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
		if (this.eof()) {
			throw new StreamEOFException("EOF reached in StringStream");
		}
		
		count = count || 1;
		
		var slice = this.slice(this.position, this.position+count);
		this.position += count;
		return slice;
	},
	
	prev: function (count) {
		count = count || 1;
		this.position -= count;
		
		assert(!this.bof(), "Cannot access character at position " +
			this.position + " of StringStream");
		
		return this.charAt(this.position);
	},
	
	swallowWhitespace: function () {
		while (WHITESPACE.indexOf(this.peek()) != -1 && !this.eof()) {
			this.position++;
		}
	},
	
	line: function () {
		var substr = this.data.slice(0, this.position);
		var matches = substr.match(/\n/g);
		return matches ? matches.length+1 : 1;
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
 * Recursively resolves all (resolve) expressions.
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
	
	assert(isCallable(object), "'" + first.value + "' is not callable");
	
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
					try {
						this.parent.set(symbol, value);
					} catch (e) {
						this.symbols[symbol] = value;
					}
				} else {
					this.parent[symbol] = value;
				}
			} else {
				var object = this;
				while (object.parent && object.parent.symbols != global) {
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

var _inMacro = false; // So we can have expressions that only work inside macros
var Macro = Class.extend({
	init: function (callable) {
		/** @ignore */
		this.callable = function () {
			_inMacro = true;
			var ret = callable.apply(null, arguments);
			_inMacro = false;
			return ret;
		};
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
 * <pre>
 * An object containing all of the js-lisp parsers (string, numbers,
 * quoted items, lists, symbols, keywords, comments, etc).
 * </pre>
 * 
 * @namespace
 * @name parse
 */
var parse = {};

/**
 * <pre>
 * A list of possible number formats.
 * </pre>
 * 
 * @constant
 */
parse.NUMBER_FORMATS = [
	(/^([+-]?[0-9]+(\.[0-9]+)?(e[+-]?[0-9]+)?)([\s\(\)\"]|$)/),
	(/^([+-]?\.[0-9]+(e[+-]?[0-9]+)?)([\s\(\)\"]|$)/),
	(/^(0x([0-9a-fA-F]+))(\s+|\)|$)/)
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
 * <pre>
 * Parses a lisp script, which can be any number of root-level expression,
 * into an array of ASTs representing those expressions.
 * </pre>
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
	case "`":
		stream.next();
		return [_S("backquote"), parse.any(stream)];
	case "#":
		stream.next();
		switch (stream.peek())
		{
		case "'": // This is the function special operator
			stream.next();
			return [_S("function"), parse.any(stream)];
		case "\\": // This is the char special operator
			stream.next();
			return [_S("char"), parse.symbol(stream)];
		//case "?":
		default:
			// This is an unknown operator.
			var c = stream.next();
			throw new parse.ParserException("Undefined special operator #" + c + " at " +
				"position " + stream.position + " (expression: #" + c +
				toLisp(parse.any(stream)) + ")");
		}
	case ",":
		stream.next();
		return [_S("resolve"), parse.any(stream)];
	case "@":
		stream.next();
		return [_S("explode"), parse.any(stream)];
	case '"':
		return parse.string(stream);
	case ':':
		return parse.keyword(stream);
	case ';':
		return parse.comment(stream);
	// case '{': // An object literal
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
		throw new parse.ParserException("Invalid sexp at line " + stream.line() +
			" (starting with: '" + stream.peek() + "')");
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
		throw new parse.ParserException("Invalid symbol at line " + stream.line() +
			" (starting with: '" + stream.peek() + "')");
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
		throw new parse.ParserException("Invalid keyword at line " + stream.line() +
			", position " + stream.position + " (starting with: '" +
			stream.peek() + "')");
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
		throw new parse.ParserException("Invalid string at line " + stream.line() +
			" (starting with: '" + stream.peek() + "')");
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
	switch (c)
	{
	case "x":
		var hex = stream.next(2);
		return eval('"' + '\\' + c + hex + '"');
	default:
		return eval('"' + '\\' + c + '"');
	}
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
		throw new parse.ParserException("Invalid number at line " + stream.line() +
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
		throw new parse.ParserException("Invalid comment at line " + stream.line() +
			" (starting with: '" + stream.peek() + "')");
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
		// Input validation
		assert(arguments.length === 1, "(eval) requires 1 argument (got " +
			arguments.length + ")");
		
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
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name quote
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @returns The given argument, unevaluated.
 */
defmacro("quote", function (expression) {
	// Input validation
	assert(arguments.length === 1, "(quote) requires 1 argument (got " +
		arguments.length + ")");
	
	return expression;
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * 
 * FIXME: At some point macros will be expanded by the parser, so
 *        this will be unnecessary.
 * </pre>
 * 
 * @name backquote
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("backquote", function (expression) {
	expression = checkResolve(expression);
	expression = checkExplode(expression);
	return expression;
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name resolve
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("resolve", function (expression) {
	// Input validation
	assert(arguments.length === 1, "(resolve) requires 1 argument (got " +
		arguments.length + ")");
	
	return resolve(expression);
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name explode
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("explode", function (expression) {
	// Input validation
	assert(arguments.length === 1, "(explode) requires 1 argument (got " +
		arguments.length + ")");
	
	return [_S("explode"), expression];
});

/**
 * <pre>
 * TODO: Test me more
 * TODO: Document me
 * TODO: Add examples
 * 
 * FIXME: While this "solution" works for the limited amount of tests
 *        i've throw at it, it is FAR from elegant...in fact I might
 *        go so far as to say it makes me nauseous. That being said,
 *        I'm happy there is at least a preliminary working version.
 * </pre>
 * 
 * @tested
 * 
 * @name defmacro
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("defmacro", function (name, arglist /*, &rest */) {
	// Input validation
	assert(arguments.length >= 1, "(defmacro) requires at least 1 argument");
	assert(name instanceof Symbol, "(defmacro) requires a symbol as its first " +
		"argument (got " + toLisp(name) + ")");
	assert(arguments.length <= 1 || arglist instanceof Array, "(defmacro) " +
		"requires an Array as its second argument (got " + toLisp(arglist) + ")");
	
	arglist = arglist || [];
	
	function setargs (arglist, args, top) {
		if (top === undefined) {
			top = true;
		}
		assert(args instanceof Array, "Error while parsing arguments to macro " + toLisp(name) + ".\n" +
			"\tInvalid arguments to:\n" +
			"\t  " + toLisp(arglist) + "\n" +
			"\tExpected a list but got '" + toLisp(args) + "'");
		
		var i;
		var j = 0;
		var arg;
		for (i = 0; i < arglist.length; i++) {
			arg = arglist[i];
			j++;
			if (arg instanceof Array) {
				setargs(arg, args[i], false);
			} else {
				if (String(arg) == "&") {
					assert(i != arglist.length - 1,
						"No argument name after rest identifier");
					assert(arglist.length <= i + 2, "Unexpected arguments (" +
						arglist.slice(i+1).join(" ") + ") after rest argument");
					
					lisp.env.let(arglist[i+1], args.slice(i));
					j = args.length;
					i = args.length;
					break;
				} else {
					lisp.env.let(arg, args[i]);
				}
			}
		}
		
		assert(i == j, "Error while parsing arguments to macro " + toLisp(name) + ".\n" +
			"\tNot enough arguments to:\n" +
			"\t  " + toLisp(arglist) + "\n" +
			"\tGot:\n" +
			"\t  " + toLisp(args));
	}
	
	var env  = new Env(lisp.env);
	var args = argsToArray(arguments);
	
	// FIXEME: This is a way better way of "unquoting" an expression:
	//         http://www.gigamonkeys.com/book/macros-defining-your-own.html#generating-the-expansion
	
	var documentation = null;
	if (args.length >= 3 && typeof(args[2]) === "string") {
		documentation = args[2];
	}
	
	return (function (env, args) {
		var macro = new Macro(function () {
			if (args.length < 3) {
				return null; // This macro does nothing
			}
			args = deepCopyArray(args);
			var tempEnv = lisp.env;
			var ret = null;
			
			try {
				lisp.env = new Env(lisp.env);
				//lisp.env.let("this", this); // Is this necessary or even wanted?
				lisp.env.let("arguments", arguments);
				setargs(arglist, argsToArray(arguments));
				var body = deepCopyArray(args.slice(2));
				for (var i = 0; i < body.length; i++) {
					ret = resolve(body[i]);
				}
				ret = resolve(ret); // FIXME: This extra resolve is a hack
			} finally {
				lisp.env = tempEnv;
			}
			
			return ret;
		});
		macro.documentation = documentation;
		lisp.env.set(name, macro);
		return macro;
	})(env, args);
});

/**
 * <pre>
 * Creates an anonymous function with the first (required)
 * expression as its arglist and which executes the rest of the
 * expressions when called.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name lambda
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @returns The created function.
 */
defmacro("lambda", function (arglist /*, &rest */) {
	// Input validation
	assert(arguments.length === 0 || arglist instanceof Array, "(lambda) requires " +
		"a list as its first expression (got " + toLisp(arglist) + ")");
	
	arglist = arglist || [];
	
	var env  = new Env(lisp.env);
	var args = argsToArray(arguments);
	var restname;
	
	for (i = 0; i < arglist.length; i++) {
		if (arglist[i] == "&") {
			assert(i != arglist.length - 1,
				"No argument name after rest identifier");
			assert(!(arglist.length > i + 2), "Unexpected arguments (" +
				arglist.slice(i+2).join(" ") + ") after rest argument");
			restname = arglist[i+1];
			break;
		}
	}
	
	return (function (env, args) {
		var body = args.slice(1);
		var documentation = null;
		
		if (body.length >= 1 && typeof(body[0]) === "string") {
			documentation = body[0];
		}
		
		var func = function () {
			var largs = argsToArray(arguments);
			var tempEnv = lisp.env;
			var ret = null;
			
			try {
				lisp.env = new Env(env);
				lisp.env.let("this", this);
				lisp.env.let("arguments", arguments);
			
				var i, j;
				var str;
				var index;
				var type;
				var typestr;
				var argname;
				var value;
				var optional = false;
				
				for (i = 0, j = 0; i < arglist.length; i++, j++) {
					argname = String(arglist[i]);
					
					if (argname == "&") {
						// The rest of the arguments should be collected into a list
						lisp.env.let(restname, largs.slice(j));
						i = arglist.length;
						j = largs.length;
						break;
					} else if (argname == "&opt") {
						// The rest of the arguments are optional
						optional = true;
						j--;
						continue;
					} else {
						index = argname.indexOf(":");
						type = undefined;
						typestr = undefined;
						
						if (index >= 0) {
							str = argname.slice(0, index);
							typestr = argname.slice(index+1);
							argname = str;
							if (typestr.length > 0) {
								type = lisp.eval(typestr);
							}
						}
						
						if (j < largs.length) {
							value = largs[j];
							if (value !== undefined || !optional) {
								if (type instanceof Keyword) {
									if (typeof(value) != String(type)) {
										throw new ArgumentError("Got invalid argument for " +
											toLisp(argname) + ". Expected a value of type " +
											toLisp(String(type)) + " (got " + toLisp(value) + ").");
									}
								} else if (typeof(type) === "function") {
									if (!(value instanceof type)) {
										throw new ArgumentError("Got invalid argument for " +
											toLisp(argname) + ". Expected an instance of " +
											typestr + " (got " + toLisp(value) + ").");
									}
								} else if (type !== undefined) {
									throw new ArgumentError("Invalid type specifier: " + toLisp(type));
								}
							}
							lisp.env.let(argname, value);
						} else if (optional) {
							lisp.env.let(argname, undefined);
						} else {
							throw new ArgumentError("Missing argument " + toLisp(argname) +
								" in function call.");
						}
						
					}
				}
				
				// Uncommenting this will throw an ArgumentError if the function
				// receives _too many_ arguments.
				// if (j < largs.length) {
				// 	throw new ArgumentError("Too many arguments passed to function " +
				// 		"with arglist " + toLisp(arglist));
				// }
				
				for (i = 0; i < body.length; i++) {
					ret = resolve(body[i]);
				}
			} finally {
				lisp.env = tempEnv;
			}
			
			return ret;
		};
		
		func.documentation = documentation;
		return func;
	})(env, args);
});

/**
 * <pre>
 * Defines a function. This is shorthand for (setq name (lambda ...)).
 * 
 * TODO: Test me
 * TODO: Add examples
 * 
 * FIXME: Define this is lisp.
 * </pre>
 * 
 * @name defun
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("defun", function (name, arglist /*, ... */) {
	// Input validation
	assert(arguments.length > 0, "(defun) requires at least 1 argument.");
	assert(name instanceof Symbol, "(defun) requires a symbol as its first argument " +
		"(got " + toLisp(name) + ")");
	assert(arguments.length === 1 || arglist instanceof Array, "(defun) requires " +
		"a list of symbols as its second expression (got " + toLisp(arglist) + ")");
	
	var body = argsToArray(arguments).slice(2);
	var lambda = resolve([_S("lambda"), arglist].concat(body));
	
	lisp.env.set(name, lambda);
	return lambda;
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
 *     >> (try)
 *     => nil
 * 
 * @example Empty catch block (silences the error)
 *     >> (try
 *            (throw (new Error))
 *          (catch))
 *     => nil
 * 
 * @example Multiple expressions with a full catch block
 *     >> (try
 *            (print "This will print")
 *            (throw (new Error "This cuts the expression short"))
 *            (print "This will not print")
 *          (catch (e)
 *            (format t "This will print when the error is thrown: %s" e)))
 *       This will print
 *       This will print when the error is thrown: Error: This cuts the expression short
 *       => nil
 * 
 * @example try/finally
 *     >> (try
 *            (throw (new Error))
 *          (finally
 *            (print "This always runs")))
 *     This always runs
 *     Error
 * 
 * @example try/catch/finally
 *     >> (try
 *            "hello" ; This will get returned because it's the last evaluated expression
 *            (throw (new Error))
 *          (catch (e)
 *            (print "This will get called"))
 *          (finally
 *            (print "This always runs")))
 *     This will get called
 *     This always runs
 *     => "hello" ; The error is caught, so there is a return value
 * 
 * @example catch/finally symbols as keywords
 *     >> (try
 *            (throw (new Error))
 *          (:catch) ; This will work
 *          (:finally)) ; And so will this
 *     => nil
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
	} catch (error) {
		// Evaluate the catch expression if there is one.
		if (catchExpression) {
			if (catchExpression.length > 0) {
				var tempEnv = lisp.env;
				
				try {
					lisp.env = new Env(lisp.env);
					
					if (catchExpression.length >= 2) { // If there is an argument list
						var catchArglist = catchExpression[1];
					
						assert(catchArglist instanceof Array, "(catch) expression's first " +
							"argument must be an Array (got " + toLisp(catchArglist) + ")");
						assert(catchArglist.length <= 1, "(catch) requires an arglist with " +
							"either 1 argument or no arguments (got " + catchArglist.length + ")");
					
						if (catchArglist.length === 1) {
							assert(catchArglist[0] instanceof Symbol, "(catch) arglist " +
								"requires a symbol as its argument, or nothing (got " +
								toLisp(catchArglist[0]) + ")");
							lisp.env.let(catchArglist[0], error);
						}
					}
					
					var expressions = catchExpression.slice(2);
					for (i = 0; i < expressions.length; i++) {
						resolve(expressions[i]);
					}
				} finally {
					lisp.env = tempEnv;
				}
			}
		} else {
			// If there is no catch expression, throw the error for something
			// else to catch it (or not).
			throw e;
		}
	} finally {
		// Evaluate all expressions in the finally expression if there
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
	// Input validation
	assert(arguments.length >= 2, "(funcall) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	// Grab the object.
	object = resolve(object);
	
	// Make sure we can get a string dotpath from the supplied argument.
	if (dotpath instanceof Symbol || dotpath instanceof Keyword) {
		dotpath = dotpath.value;
	}
	
	assert(typeof(dotpath) === "string", "Unknown key in " +
		"(funcall): " + String(dotpath));
	
	// Resolve the object down to the second-to-last part of the dot path.
	var parts = String(dotpath).split(".");
	for (var i = 0; i < parts.length-1; i++) {
		object = object[parts[i]];
	}
	
	// Make sure what's being "called" is actually a function.
	var funckey = parts[parts.length-1];
	
	assert(typeof(object[funckey]) === "function", String(dotpath) +
		" on " + object + " is not a function");
	
	var args = argsToArray(arguments).slice(2).map(resolve);
	return object[funckey].apply(object, args);
});

/**
 * <pre>
 * TODO: Document me
 * TODO: Add examples
 * 
 * FIXME: This method sucks (actually, env.let sucks)
 * </pre>
 * 
 * @tested
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
	
	var tempEnv = lisp.env;
	var ret = null;
	
	try {
		lisp.env = new Env(lisp.env);
		args = args.slice(1);
	
		for (i = 0; i < letset.length; i++) {
			var symbol = letset[i][0];
			var value = resolve(letset[i][1]);
			lisp.env.let(symbol, value);
		}
	
		for (i = 0; i < args.length; i++) {
			ret = resolve(args[i]);
		}
	} finally {
		lisp.env = tempEnv;
	}
	
	return ret;
});

/**
 * <pre>
 * Sets the value of the variable represented by the given symbol to
 * the given value.
 * </pre>
 * 
 * @tested
 * 
 * @name setq
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @param {symbol} symbol
 *     The variable whose value is to be set.
 * @param {mixed} value
 *     The value to set to the variable represented by the given symbol.
 * 
 * @example Basic usage
 *     >> x
 *     => undefined
 *     >> (setq x 1)
 *     => 1
 *     >> x
 *     => 1
 * 
 * @example Using the return value
 *     >> (let ((last nil))
 *     ..   (try
 *     ..     (dolist (item '(1 2 3))
 *     ..       (when (> (setq last item) 1)
 *     ..         (throw)))
 *     ..   (:catch))
 *     ..   last)
 *     => 2
 * 
 * @example Setting a property on an object
 *     >> (let ((obj (object :one (object :two 3))))
 *     ..   (print obj.one.two)
 *     ..   (setq obj.one.two 10)
 *     ..   (print obj.one.two))
 *     3
 *     10
 *     => nil
 */
defmacro("setq", function (symbol, expression) {
	assert(arguments.length === 2, "(setq) requires 2 arguments (got " +
		arguments.length + ")");
	assert(symbol instanceof Symbol, "(setq) requires a symbol as its first " +
		"argument (got " + toLisp(symbol) + ")");
	var value = resolve(expression);
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
 * 
 * @param {[mixed]} expressions
 *     The expressions to be evaluated.
 * @rest expressions
 */
defmacro("progn", function (/* &expressions */) {
	var ret = null;
	for (var i = 0; i < arguments.length; i++) {
		ret = resolve(arguments[i]);
	}
	return ret;
});

/**
 * <pre>
 * Takes a list of lists of expressions. Evaluates the first expression
 * of each top-level list until one evaluates to true, then it evaluates
 * the rest of the expressions in that list only, returning the return
 * value of its last expression. If no list contains a first expression
 * that evaluates to true, cond returns nil.
 * </pre>
 * 
 * @tested
 * 
 * @name cond
 * @lisp
 * @function
 * @macro
 * @member lisp.macros
 * 
 * @returns The value of the last evaluated expression, or nil.
 * 
 * @param {[Array]} rest
 *     A list of lists of expressions to be evaluated (until one
 *     contains a first expression that evaluates to true).
 * @rest rest
 * 
 * @example Empty cond
 *     >> (cond)
 *     => nil
 * 
 * @example Default expression
 *     >> (cond ((== 1 2) "this wont get evaluated, at least not in this universe")
 *              (nil "neither will this")
 *              (t "but this will")) ;; Acts like a "default" expression
 *     => "but this will"
 * 
 * @example Short circuiting
 *     >> (cond (t "this always gets avaluated")
 *              (t "this will never get evaluated"))
 *     => "this always gets avaluated"
 */
defmacro("cond", function (/* &rest */) {
	for (var i = 0; i < arguments.length; i++) {
		var clause = arguments[i];
		
		assert(clause instanceof Array, "(cond) clause must be a list " +
			"(got " + toLisp(clause) + ")");
		assert(clause.length > 0, "(cond) clauses must contain an " +
			"expression to evaluate");
		
		var condition = clause[0];
		if (!!resolve(condition)) {
			var ret = null;
			for (var j = 1; j < clause.length; j++) {
				ret = resolve(clause[j]);
			}
			return ret;
		}
	}
	return null;
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add Examples
 * TODO: Add support for 'otherwise
 * </pre>
 * 
 * @name switch
 * @lisp
 * @function
 * @macro
 * @member lisp.macros
 */
defmacro("switch", function (valueExpression /*, &rest */) {
	assert(arguments.length >= 1, "(switch) requires at least one argument");
	var value = resolve(valueExpression);
	var rest = argsToArray(arguments).slice(1);
	var ret = null;
	var clause;
	var key;
	
	for (var i = 0; i < rest.length; i++) {
		clause = rest[i];
		assert(clause instanceof Array, "(switch) requires an array for each " +
			"clause (got " + toLisp(clause) + ")");
		assert(clause.length >= 1, "(switch) requires each clause list have at " +
			"least one value");
		key = clause[0];
		if (equal(value, key)) {
			var expressions = clause.slice(1);
			for (var j = 0; j < expressions.length; j++) {
				ret = resolve(expressions[j]);
			}
			break;
		}
	}
	
	return ret;
});

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
	// Input validation
	assert(arguments.length >= 2, "(if) requires at least 2 arguments (got " +
		arguments.length + ")");
	
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
 * TODO: Document me
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
defmacro("when", function (ifExpression /*, &rest */) {
	// Input validation
	assert(arguments.length >= 1, "(when) requires at least 1 argument");
	
	var ret = null;
	
	if (!!resolve(ifExpression)) {
		var args = argsToArray(arguments);
		for (var i = 1; i < args.length; i++) {
			ret = resolve(args[i]);
		}
	}
	
	return ret;
});

/**
 * <pre>
 * Executes the rest of the arguments if the first argument
 * is false.
 * 
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name unless
 * @lisp
 * @function
 * @member lisp.macros
 * 
 * @returns The return value of the last expression.
 */
defmacro("unless", function (ifNotExpression /*, &rest */) {
	// Input validation
	assert(arguments.length >= 1, "(unless) requires at least 1 argument");
	
	var ret = null;
	
	if (!resolve(ifNotExpression)) {
		var args = argsToArray(arguments);
		for (var i = 1; i < args.length; i++) {
			ret = resolve(args[i]);
		}
	}
	
	return ret;
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
	// Input validation
	assert(arguments.length > 0, "(not) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return !value;
	});
});

/**
 * <pre>
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @tested
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
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @tested
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
 * Returns the first expression that evaluates to true in a boolean
 * context, otherwise returns null.
 * 
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name ||
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("||", function () {
	for (var i = 0; i < arguments.length; i++) {
		var ret = resolve(arguments[i]);
		if (ret) {
			return ret;
		}
	}
	
	return null;
});

/**
 * <pre>
 * If the given symbol resolves to true, simply returns that value,
 * otherwise evaluates the given expression, sets its value to the
 * symbol on the env, and returns it.
 * 
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name ||=
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("||=", function (symbol, expression) {
	assert(arguments.length === 2, "(||=) requires 2 arguments (got " +
		arguments.length + ")");
	assert(symbol instanceof Symbol, "(||=) requires a symbol as its first " +
		"argument (got " + toLisp(symbol) + ")");
	
	// If symbol already evaluates to true, don't set it, and just
	// return its current value.
	var ret = resolve(symbol);
	if (ret) {
		return ret;
	}
	
	// If symbol evaluates to fales, set it to the value of the given
	// expression, and return that value.
	ret = resolve(expression);
	lisp.env.set(symbol, ret);
	return ret;
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
	// Input validation
	assert(arguments.length >= 2, "(equal) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
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
	// Input validation
	assert(arguments.length >= 2, "(not-equal) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	return comparator(arguments, function (a, b) {
		return !equal(a, b);
	});
});

/**
 * <pre>
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name ==
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("==", function () {
	// Input validation
	assert(arguments.length >= 2, "(==) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	return comparator(arguments, function (a, b) {
		return a == b;
	});
});

/**
 * <pre>
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name ===
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("===", function () {
	// Input validation
	assert(arguments.length >= 2, "(===) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	return comparator(arguments, function (a, b) {
		return a === b;
	});
});

/**
 * <pre>
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name !=
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("!=", function () {
	// Input validation
	assert(arguments.length >= 2, "(!=) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	return comparator(arguments, function (a, b) {
		return a != b;
	});
});

/**
 * <pre>
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name !==
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("!==", function () {
	// Input validation
	assert(arguments.length >= 2, "(!==) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	return comparator(arguments, function (a, b) {
		return a !== b;
	});
});

/**
 * <pre>
 * TODO: Document me
 * </pre>
 * 
 * @tested
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
 *     => f
 */
defmacro("<", function () {
	// Input validation
	assert(arguments.length >= 2, "(<) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	return comparator(arguments, function (a, b) {
		return a < b;
	});
});

/**
 * <pre>
 * TODO: Document me
 * </pre>
 * 
 * @tested
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
 *     => f
 */
defmacro(">", function () {
	// Input validation
	assert(arguments.length >= 2, "(>) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	return comparator(arguments, function (a, b) {
		return a > b;
	});
});

/**
 * <pre>
 * TODO: Document me
 * </pre>
 * 
 * @tested
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
 *     => f
 */
defmacro("<=", function () {
	// Input validation
	assert(arguments.length >= 2, "(<=) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	return comparator(arguments, function (a, b) {
		return a <= b;
	});
});

/**
 * <pre>
 * TODO: Document me
 * </pre>
 * 
 * @tested
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
 *     => f
 */
defmacro(">=", function () {
	// Input validation
	assert(arguments.length >= 2, "(>=) requires at least 2 arguments " +
		"(got " + arguments.length + ")");
	
	return comparator(arguments, function (a, b) {
		return a >= b;
	});
});

/**
 * <pre>
 * Returns true if the given values === true.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name is-true
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-true", function () {
	// Input validation
	assert(arguments.length > 0, "(is-true) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return value === true;
	});
});

/**
 * <pre>
 * Returns true if the given values === false.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name is-false
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-false", function () {
	// Input validation
	assert(arguments.length > 0, "(is-false) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return value === false;
	});
});

/**
 * <pre>
 * Returns true if the given values === null.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name is-null
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-null", function () {
	// Input validation
	assert(arguments.length > 0, "(is-null) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return value === null;
	});
});

/**
 * <pre>
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name is-undefined
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-undefined", function () {
	// Input validation
	assert(arguments.length > 0, "(is-undefined) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return value === undefined;
	});
});

/**
 * <pre>
 * Returns true if the given values are strings.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name is-string
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-string", function () {
	// Input validation
	assert(arguments.length > 0, "(is-string) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return typeof(value) == "string";
	});
});

/**
 * <pre>
 * Returns true if the given values are numbers.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name is-number
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-number", function () {
	// Input validation
	assert(arguments.length > 0, "(is-number) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return typeof(value) == "number";
	});
});

/**
 * <pre>
 * Returns true if the given values are booleans.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name is-boolean
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-boolean", function () {
	// Input validation
	assert(arguments.length > 0, "(is-boolean) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return typeof(value) == "boolean";
	});
});

/**
 * <pre>
 * Returns true if the given values are functions.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name is-function
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-function", function () {
	// Input validation
	assert(arguments.length > 0, "(is-function) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return typeof(value) == "function";
	});
});

/**
 * <pre>
 * Returns true if the given values are objects.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name is-object
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-object", function () {
	// Input validation
	assert(arguments.length > 0, "(is-object) requires at least 1 argument");
	
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
	// Input validation
	assert(arguments.length > 0, "(is-array) requires at least 1 argument");
	
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
	// Input validation
	assert(arguments.length > 0, "(is-list) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return value instanceof Array;
	});
});

/**
 * <pre>
 * Returns true if the given values are Symbols.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-symbol
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-symbol", function () {
	// Input validation
	assert(arguments.length > 0, "(is-symbol) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return value instanceof Symbol;
	});
});

/**
 * <pre>
 * Returns true if the given values are Keywords.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-keyword
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-keyword", function () {
	// Input validation
	assert(arguments.length > 0, "(is-keyword) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return value instanceof Keyword;
	});
});

/**
 * <pre>
 * Returns true if the given values are Macros.
 * 
 * TODO: Test me
 * TODO: Add examples
 * </pre>
 * 
 * @name is-macro
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("is-macro", function () {
	// Input validation
	assert(arguments.length > 0, "(is-macro) requires at least 1 argument");
	
	return predicate(arguments, function (value) {
		return value instanceof Macro;
	});
});

/**
 * 
 */
defmacro("do", function (vardefs, end /*, &rest */) {
	// (do (variable-definition*)
	//     (end-test-form result-form*)
	//   statement*)
	// variable-definition = (var init-form step-form)
	var body = argsToArray(arguments).slice(2);
	var i;
	var def;
	
	assert(vardefs instanceof Array, "(do) requires a list as " +
		"its first argument (got " + toLisp(vardefs) + ")");
	
	for (i = 0; i < vardefs.length; i++) {
		def = vardefs[i];
		assert(def instanceof Array, "(do) requires lists as " +
			"variable definitions (got " + toLisp(def) + ")");
		assert(def.length >= 2 && def.length <= 3, "(do) variable " +
			"definitions must have 2 or 3 elements (got " +
			toLisp(def) + ")");
	}
	
	assert(end instanceof Array, "(do) requires a list as its " +
		"second argument (got " + toLisp(end) + ")");
	assert(end.length >= 1, "(do) requires its 'end' expression " +
		"to have at least 1 element (the 'test' expression)");
	
	return withNewEnv(function () {
		for (i = 0; i < vardefs.length; i++) {
			def = vardefs[i];
			lisp.env.let(def[0], resolve(def[1])); // 0 = name, 1 = init form
		}
		
		while (!resolve(end[0])) { // 0 = end test
			// Evaluate the body expressions.
			for (i = 0; i < body.length; i++) {
				resolve(body[i]);
			}
			
			// Evaluate the step forms.
			for (i = 0; i < vardefs.length; i++) {
				def = vardefs[i];
				var step = def.length === 2 ? def[1] : def[2]; // 2 = step form
				lisp.env.set(def[0], resolve(step));
			}
		}
		
		var resultForms = end.slice(1);
		var ret = null;
		for (i = 0; i < resultForms.length; i++) {
			ret = resolve(resultForms[i]);
		}
		return ret;
	});
});

/**
 * <pre>
 * An expression for basic iteration over a list.
 * 
 * TODO: Test me more
 * TODO: Add examples
 * 
 * FIXME: Define this using (defmacro) in macros.lisp
 * </pre>
 * 
 * @name dolist
 * @lisp
 * @function
 * @member lisp.macros
 */
defmacro("dolist", function (arglist /*, ... */) {
	// Input validation
	assert(arguments.length > 0, "(dolist) requires at least 1 argument " +
		"(got " + arguments.length + ")");
	assert(arglist instanceof Array, "(dolist) got invalid argument " +
		"list: " + toLisp(arglist));
	assert(arglist.length >= 2 && arglist.length <= 3, "(dolist) got " +
		"invalid argument list. Requires at least 2 arguments and no " +
		"more than 3 (got " + arglist.length + ")");
	
	var itemName = arglist[0];
	var list     = resolve(arglist[1]);
	
	assert(itemName instanceof Symbol, "(dolist) got invalid argument " +
		"list. First argument must be a symbol (got " +
		toLisp(itemName) + ")");
	assert(list instanceof Array, "(dolist) got invalid argument list. " +
		"Second argument must be a list (got " + toLisp(list) + ")");
	
	var tempEnv = lisp.env;	
	var ret = null;
	
	try {
		lisp.env = new Env(lisp.env);
		var body = argsToArray(arguments).slice(1);
		for (var i = 0; i < list.length; i++) {
			lisp.env.let(itemName, list[i]);
			for (var j = 0; j < body.length; j++) {
				ret = resolve(body[j]);
			}
		}
	} finally {
		lisp.env = tempEnv;
	}
	
	if (arglist.length === 3) {
		return resolve(arglist[2]);
	} else {
		return ret;
	}
});

// TODO: Write macro (dotimes)

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * </pre>
 * 
 * @name foreach
 * @lisp
 * @function
 * @macro
 * @member lisp.macros
 * 
 * @example Basic usage
 *     >> (let ((obj (object :one 1 :two 2 :three 3)))
 *          (foreach (item obj)
 *            (let ((key (first item)))
 *              (print (first item)))))
 *     one
 *     two
 *     three
 *     => nil
 * 
 * @example Explode the item
 *     >> (let ((obj (object :one 1 :two 2 :three 3)))
 *          (foreach ((key value) obj)
 *            (format t "%l: %l" key value)))
 *     "one": 1
 *     "two": 2
 *     "three": 3
 *     => nil
 */
defmacro("foreach", function (arglist /*, &rest */) {
	// Input validation
	assert(arguments.length > 0, "(foreach) requires at least 1 argument");
	assert(arglist instanceof Array, "(foreach) requires a list as its " +
		"first argument (got " + toLisp(arglist) + ")");
	assert(arglist.length === 2, "(foreach) got invalid argument list. " +
		"Requires 2 arguments (got " + arglist.length + ")");
	
	var item   = arglist[0];
	var object = resolve(arglist[1]);
	
	assert(item instanceof Symbol ||
		   (item instanceof Array && item.length === 2 &&
	        item[0] instanceof Symbol && item[1] instanceof Symbol),
		"(foreach) got invalid argument list. First argument must " +
		"be a symbol or list with 2 symbols (got " + toLisp(item) + ")");
	assert(object instanceof Object, "(foreach) got invalid argument list. " +
		"Second argument must be an object (got " + toLisp(object) + ")");
	
	var body = argsToArray(arguments).slice(1);
	
	var tempEnv = lisp.env;
	var ret = null;
	
	var itemName;
	var keyName;
	var valName;
	
	if (item instanceof Array) {
		keyName = item[0];
		valName = item[1];
	} else {
		itemName = item;
	}
	
	try {
		lisp.env = new Env(lisp.env);
		var value;
		for (var key in object) {
			value = object[key];
			if (itemName) {
				lisp.env.let(itemName, [key, value]);
			} else {
				lisp.env.let(keyName, key);
				lisp.env.let(valName, value);
			}
			for (var i = 0; i < body.length; i++) {
				ret = resolve(body[i]);
			}
		}
	} finally {
		lisp.env = tempEnv;
	}
	
	return ret;
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add more examples
 * </pre>
 * 
 * @name char
 * @lisp
 * @function
 * @macro
 * @member lisp.macros
 */
defmacro("char", function (symbol) {
	// Input validation
	assert(arguments.length === 1, "(char) requires 1 argument (got " +
		arguments.length + ")");
	assert(symbol instanceof Symbol, "(char) requires a symbol as its " +
		"argument (got " + toLisp(symbol) + ")");
	
	return _char_table[String(symbol)];
});

/**
 * <pre>
 * TODO: Test me more
 * TODO: Document me
 * TODO: Add more examples
 * </pre>
 * 
 * @tested
 * 
 * @name collect
 * @lisp
 * @function
 * @macro
 * @member lisp.macros
 * 
 * @example Basic usage
 *     >> (let ((obj (object :name "js-lisp" :age 0)))
 *          (collect (item obj)
 *            (is-number (second item))))
 *     => (("age" 0)) ; Returns every (key value) pair where the last 
 *                    ; expression of the body evaluates to true.
 */
var _macro_collect; // Defined in /src/lisp/macros.lisp

/**
* <pre>
* TODO: Test me more
* TODO: Document me
* TODO: Add more examples
* </pre>
* 
* @tested
* 
* @name inc
* @lisp
* @function
* @macro
* @member lisp.macros
 */
defmacro("inc", function (varName, amountName) {
	assert(arguments.length >= 1, "(inc) requires at least 1 argument");
	
	var amount = (amountName === undefined) ? 1 : resolve(amountName);
	var oldValue = lisp.env.get(varName);
	var newValue = oldValue + amount;
	lisp.env.set(varName, newValue);
	return newValue;
});

/**
* <pre>
* TODO: Test me more
* TODO: Document me
* TODO: Add more examples
* </pre>
* 
* @tested
* 
* @name dec
* @lisp
* @function
* @macro
* @member lisp.macros
 */
defmacro("dec", function (varName, amountName) {
	assert(arguments.length >= 1, "(dec) requires at least 1 argument");
	
	var amount = (amountName === undefined) ? 1 : resolve(amountName);
	var oldValue = lisp.env.get(varName);
	var newValue = oldValue - amount;
	lisp.env.set(varName, newValue);
	return newValue;
});

// FIXME: Put this somewhere else.
var Generator = Class.extend({
	init: function (callable) {
		this.callable = callable;
	},
	call: function () {
		return this.callable.call(null);
	}
});

// FIXME: Put this somewhere else.
var StopIteration = Class.extend(); // ({}) ?

/**
 * <pre>
 * Iteration for arrays, strings and generators (and other
 * sequences as well).
 * 
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name for
 * @lisp
 * @function
 * @macro
 * @member lisp.macros
 */
defmacro("for", function (arglist /*, &rest */) {
	assert(arguments.length >= 1, "(for) requires at least 1 argument");
	
	var itemName = arglist[0];
	var iterable = resolve(arglist[1]);
	var rest = argsToArray(arguments).slice(1);
	var i;
	var ret;
	
	function doIteration (index, item) {
		withNewEnv(function () {
			if (rest.length === 0) {
				return null;
			}

			lisp.env.let(itemName, item);
			ret = null;
			for (var i = 0; i < rest.length; i++) {
				ret = resolve(rest[i]);
			}
			return ret;
		});
	}
	
	ret = null;
	
	if (iterable instanceof Generator) {
		// The iterable object is a generator
		try {
			for (i = 0; true; i++) {
				ret = doIteration(i, iterable.call());
			}
		} catch (e) {
			if (!(e instanceof StopIteration)) {
				throw e;
			}
		}
	} else if (iterable.hasOwnProperty("length")) {
		// The iterable object is some kind of sequence
		for (i = 0; i < iterable.length; i++) {
			ret = doIteration(i, iterable[i]);
		}
	} else { // Don't know how to iterate over this
		throw new Error("(for) received an object it does not know how to iterate " +
			"over: " + toLisp(iterable));
	}
	
	return ret;
});

/**
 * <pre>
 * Emulates the goto functionality of languages like c. Takes
 * a list of lists, each one starting with a label and containing
 * an arbitrary number of expressions. Evalualtes the expressions
 * in order, except when (goto :label) is called.
 * 
 * FIXME: This macro works but is large in gross.
 * 
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name labels
 * @lisp
 * @function
 * @macro
 * @member lisp.macros
 * 
 * @example Basic (slightly contrived) usage
 *     >> (let ((i))
 *     ..   (labels
 *     ..     (:one ;; Setup
 *     ..       (setq i -1))
 *     ..     (:two ;; Increment
 *     ..       (inc i))
 *     ..     (:three ;; Test
 *     ..       (print i)
 *     ..       (when (< i 3)
 *     ..         (goto :two)))
 *     ..     (:four ;; Fin
 *     ..       (format t "Done at %l" i))))
 *     0
 *     1
 *     2
 *     3
 *     Done at 3
 *     => nil
 */
defmacro("labels", function (/* &rest */) {
	if (arguments.length === 0) {
		return null;
	}
	
	var args = argsToArray(arguments);
	
	var labels = args.map(function (arg) {
		assert(arg.length >= 1, "(labels) label must have at least a label");
		return {
			label: arg[0],
			expressions: arg.slice(1)
		};
	});
	
	var exprIndex = -1;
	var labelIndex = 0;
	var expressions = labels[labelIndex].expressions;
	
	var gotoFunc = function (label) {
		exprIndex = -1;
		if (label === null) {
			// (goto nil) is how you return from a (labels) expression.
			labelIndex = labels.length;
			return;
		}
		for (labelIndex = 0; labelIndex < labels.length; labelIndex++) {
			if (equal(labels[labelIndex].label, label)) {
				expressions = labels[labelIndex].expressions;
				return;
			}
		}
		throw new Error(toLisp(label) + " is not a valid label");
	};
	
	withNewEnv(function () {
		lisp.env.let("goto", gotoFunc);
		
		while (exprIndex < expressions.length &&
		       labelIndex < labels.length) {
			exprIndex++;
			resolve(expressions[exprIndex]);
			if (exprIndex >= expressions.length) {
				exprIndex = -1;
				labelIndex++;
				if (!labels[labelIndex]) {
					break;
				}
				expressions = labels[labelIndex].expressions;
			}
		}
	});
	
	return null;
});

/**
 * <p>Functions that are defined for the lisp environment.
 * 
 * @name lisp.functions
 * @namespace
 */
var functions = {}; // This is just for documentation. It doesn't get used.

/**
 * <pre>
 * Evaluates the given expression in JavaScript.
 * </pre>
 * 
 * @tested
 * 
 * @name jseval
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} expression
 *     The expression to be evaluated by JavaScript.
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
defun("jseval", function (expression) {
	return eval(expression);
});

/**
 * <pre>
 * Parses and evaluates a string as lisp.
 * 
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name eval-string
 * @lisp
 * @function
 * @member lisp.functions
 */
// defun("eval-string", function (str) {
// 	assert(arguments.length === 1, "(eval-string) requires 1 argument (got " +
// 		arguments.length + ")");
// 	assert(typeof(str) === "string", "(eval-string) requires a string as its " +
// 		"argument (got " + toLisp(str) + ")");
// 	
// 	return lisp.eval(str);
// });
var _function_eval_string; // Defined in /src/lisp/functions.lisp

/**
 * <pre>
 * Raises new Error(errorMessage) if assertion evaluates to false.
 * </pre>
 * 
 * @tested
 * 
 * @name assert
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} assertion
 *     The expression to evaluate in a boolean context.
 * @param {mixed} [errorMessage]
 *     The option message to be passed to new Error().
 * 
 * @example Asserting a true expression
 *     >> (assert t "Oh no! t is false!")
 *     => nil
 * 
 * @example Asserting a false expression
 *     >> (assert f "f is definitely false")
 *     Error: f is definitely false
 */
defun("assert", function (assertion, errorMessage) {
	// Input validation
	assert(arguments.length > 0, "(assert) requires at least 1 argument");
	assert(arguments.length <= 2, "Too many arguments given to (assert). " +
		"Expected no more than 2 (got " + arguments.length + ")");
	
	// This is the assert the user is making
	assert(assertion, errorMessage);
	
	return null;
});

/**
 * <pre>
 * Returns a symbol that is <em>likely</em> to be unique in your environment.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name gensym
 * @lisp
 * @function
 * @member lisp.functions
 */
defun("gensym", function () {
	// Input validation
	assert(arguments.length === 0, "(gensym) takes no arguments (got " +
		arguments.length + ")");
	
	return gensym();
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
	// Input validation
	assert(arguments.length > 0, "(new) requires at least 1 argument");
	assert(typeof(cls) === "function", "(new) requires a function as its first argument " +
		"(got " + toLisp(cls) + ")");
	
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
	// Input validation
	assert(arguments.length === 2, "(instanceof) requires 2 arguments (got " +
		arguments.length + ")");
	assert(typeof(cls) === "function", "(instanceof) requires a function as its second " +
		"argument (got " + toLisp(cls) + ")");
	
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
	// Input validation
	assert(arguments.length <= 1, "(throw) accepts 1 optional argument (got " +
		arguments.length + ")");
	
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
	// Input validation
	assert(arguments.length % 2 === 0, "(object) expects an even number of " +
		"arguments (got " + arguments.length + ")");
	
	var args = argsToArray(arguments);
	var object = {};
	
	for (var i = 0; i < args.length; i += 2) {
		object[args[i]] = args[i+1];
	}
	
	return object;
});

/**
 * <pre>
 * Returns the function that the given expression evaluates to.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name function
 * @lisp
 * @function
 * @member lisp.macros
 */
defun("function", function (value) {
	// Input validation
	assert(arguments.length === 1, "(function) requires 1 argument (got " +
		arguments.length + ")");
	
	var object = resolve(value);
	
	if (typeof(object) == "function") {
		return object;
	} else if (object instanceof Macro) {
		return object.callable;
	}
	
	throw new Error("'" + toLisp(value) + "' is not callable");
});

/**
 * <pre>
 * A shortcut for (not (not value)), or (and value).
 * 
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name !!
 * @lisp
 * @function
 * @member lisp.functions
 */
var _function_exc_exc; // Defined in /src/lisp/functions.lisp

/**
 * <pre>
 * A shortcut for (new Regex (concat rest) flags).
 * 
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name regex
 * @lisp
 * @function
 * @member lisp.functions
 */
var _function_regex; // Defined in /src/lisp/functions.lisp

/**
 * <pre>
 * Returns a value from an object given a key (will work with
 * array indices as well).
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name getkey
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The value of object[key].
 * 
 * @param {object} object
 *     The object on which to access the given key.
 * @param {mixed} key
 *     The key to access on the given object.
 */
defun("getkey", function (object, key) {
	// Input validation
	assert(arguments.length === 2, "(getkey) requires 2 arguments (got " +
		arguments.length + ")");
	
	return object[key];
});

/**
 * <pre>
 * Sets a value on the given object using the given key.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name setkey
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The given value.
 * 
 * @param {object} object
 *     The object on which to set the given value.
 * @param {mixed} key
 *     The key to set on the given object.
 * @param {mixed} value
 *     The value to set to the given key on the given object.
 */
defun("setkey", function (object, key, value) {
	// Input validation
	assert(arguments.length === 3, "(setkey) requires 3 arguments (got " +
		arguments.length + ")");
	
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
	// Input validation
	assert(arguments.length > 0, "(join) requires at least 1 argument");
	
	var args = argsToArray(arguments);
	var rest = args.slice(1);
	
	for (var i = 0; i < rest.length; i++) {
		var arg = rest[i];
		assert(arg instanceof Array, "(join) got an invalid argument: '" +
		    toLisp(arg) + "' is not a list");
	}
	
	var list = rest.reduce(function (a, b) { return a.concat(b); });
	return list.join(sep);
});

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name list-concat
 * @lisp
 * @function
 * @member lisp.functions
 */
defun("array-concat", function (/* &rest */) {
	var args = argsToArray(arguments);
	return args.reduce(function (a, b) { return a.concat(b); });
});

/**
 * <pre>
 * Returns the type of the given value (the result of "typeof(value)").
 * 
 * TODO: Add examples
 * </pre>
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
	// Input validation
	assert(arguments.length === 1, "(typeof) requires 1 argument (got " +
		arguments.length + ")");
	
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
	// Input validation
	assert(arguments.length === 1, "(to-string) requires 1 argument (got " +
		arguments.length + ")");
	
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
	// Input validation
	assert(arguments.length === 1, "(to-number) requires 1 argument (got " +
		arguments.length + ")");
	
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
	// Input validation
	assert(arguments.length === 1, "(to-boolean) requires 1 argument (got " +
		arguments.length + ")");
	
	return Boolean(value);
});

/**
 * <pre>
 * Converts the given value to a symbol.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name to-symbol
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} value
 *     The value to turn into a symbol.
 */
defun("to-symbol", function (value) {
	// Input validation
	assert(arguments.length === 1, "(to-symbol) requires 1 argument (got " +
		arguments.length + ")");
	
	return new lisp.Symbol(String(value));
});

/**
 * <pre>
 * Converts the given value to a keyword.
 * 
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name to-keyword
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} value
 *     The value to turn into a keyword.
 */
defun("to-keyword", function (value) {
	// Input validation
	assert(arguments.length === 1, "(to-keyword) requires 1 argument (got " +
		arguments.length + ")");
	
	try {
		// Use the "to-keyword" method if it has one.
		return value['to-keyword']();
	} catch (e) {}
	
	try {
		// Use the "toKeyword" method if it has one.
		return value.toKeyword();
	} catch (e) {}
	
	return new lisp.Keyword(String(value));
});

/**
 * <pre>
 * Converts the given value to a json representation of that value.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
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
	// Input validation
	assert(arguments.length > 0, "(to-json) requires at least 1 argument");
	assert(arguments.length <= 3, "Too many arguments given to (to-json). " +
		"Expected no more than 3 (got " + arguments.length + ")");
	
	return toJSON(value, pretty, levels);
});

/**
 * <pre>
 * Returns a string that is the lisp representation of the given value.
 * </pre>
 * 
 * @tested
 * 
 * @name lisp-string
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {mixed} value
 *     The value whose lisp representation is returned.
 * 
 * @example Numbers
 *     >> (lisp-string 12)
 *     => "12"
 * 
 * @example Strings
 *     >> (lisp-string "hello")
 *     => "\"hello\""
 * 
 * @example Arrays
 *     >> (lisp-string [1, 2, [3, 4], 5, 6])
 *     => (1 2 (3 4) 5 6)
 * 
 * @example Objects
 * Note: The current (lisp-string) representation of objects can't
 * then be interpreted again in js-lisp.
 *     >> (lisp-string (object :one 2 :three 4))
 *     => "{
 *       \"one\": 2, 
 *       \"three\": 4
 *     }"
 */
defun("lisp-string", function (value) {
	// Input validation
	assert(arguments.length == 1, "(lisp-string) requires 1 argument " +
		"(got " + arguments.length + ")");
	
	return toLisp(value);
});

/**
 * <pre>
 * Converts the given string to uppercase.
 * 
 * FIXME: Should this support the :start and :end keys?
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
 * 
 * @example Basic usage
 *     >> (to-upper "hello")
 *     => "HELLO"
 * 
 * @example Practical usage
 *     >> (defun yell (message)
 *     ..   (concat (to-upper message) "!"))
 *     => function () { ... }
 *     >> (yell "hello")
 *     => "HELLO!"
 */
defun("to-upper", function (string) {
	// Input validation
	assert(arguments.length === 1, "(to-upper) requires 1 argument (got " +
		arguments.length + ")");
	assert(typeof(string) === "string", "(to-upper) requires a string argument (got " +
		toLisp(string) + ")");
	
	return string.toUpperCase();
});

/**
 * <pre>
 * Converts the given string to lowercase.
 * 
 * FIXME: Should this support the :start and :end keys?
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
 * 
 * @example Basic usage
 *     >> (to-lower "HELLO")
 *     => "hello"
 * 
 * @example Practical usage
 *     >> (defun whisper (message)
 *     ..   (setq message (message.replace "!" ""))
 *     ..   (concat "(" (to-lower message) ")"))
 *     => function () { ... }
 *     >> (whisper "HELLO!")
 *     => "(hello)"
 */
defun("to-lower", function (string) {
	// Input validation
	assert(arguments.length === 1, "(to-lower) requires 1 argument (got " +
		arguments.length + ")");
	assert(typeof(string) === "string", "(to-lower) requires a string argument (got " +
		toLisp(string) + ")");
	
	return string.toLowerCase();
});

/**
 * <pre>
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name starts-with
 * @lisp
 * @function
 * @member lisp.functions
 */
var _function_starts_with; // Defined in /src/lisp/functions.lisp

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name ends-with
 * @lisp
 * @function
 * @member lisp.functions
 */
var _function_ends_with; // Defined in /src/lisp/functions.lisp

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
	// Input validation
	assert(arguments.length > 0, "(/) requires at least 1 argument");
	
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
	// Input validation
	assert(arguments.length > 0, "(-) requires at least 1 argument");
	
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
	// Input validation
	assert(arguments.length >= 2, "(%) requires at least 2 arguments (got " +
		arguments.length + ")");
	
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
	// Input validation
	assert(arguments.length === 1, "(1+) requires 1 argument (got " +
		arguments.length + ")");
	assert(typeof(number) === "number", "(1+) requires a number argument (got " +
		toLisp(number) + ")");
	
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
 * @returns The result of subtracting 1 from the given number.
 * 
 * @param {number} number
 *     The number to subtract 1 from.
 */
defun("1-", function (number) {
	// Input validation
	assert(arguments.length === 1, "(1-) requires 1 argument (got " +
		arguments.length + ")");
	assert(typeof(number) === "number", "(1-) requires a number argument (got " +
		toLisp(number) + ")");
	
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
 * @returns The result of applying the given arguments to sprintf
 *          (in the vendor section) if print evaluates to true,
 *          otherwise nil.
 * 
 * @param {boolean} doPrint
 *     Whether to print the result, or return it.
 * @param {string} format
 *     The string format to which to apply the given arguments.
 * @param {[mixed]} rest
 *     The arguments to apply to the given format.
 * @rest rest
 */
defun("format", function (doPrint, format /*, &rest */) {
	// Input validation
	assert(arguments.length >= 2, "(format) expects at least 2 arguments (got " +
		arguments.length + ")");
	assert(typeof(format) === "string", "(format) expects a string format (got " +
		toLisp(format) + ")");
	
	var output = sprintf.apply(null, argsToArray(arguments).slice(1));
	
	if (doPrint) {
		lisp.log(output);
		return null;
	} else {
		return output;
	}
});

/**
 * <pre>
 * Calls apply on the given function and passes in the given
 * list as arguments.
 * </pre>
 * 
 * @tested
 * 
 * @name apply
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of applying the given list as the arguments to
 *          the given function.
 * 
 * @param {function} func
 *     The function to apply the list of arguments to.
 * @param {Array} list
 *     The list to give as arguments to the given function.
 * 
 * @example Add a list of numbers together
 *     >> (apply + '(1 2 3))
 *     => 6
 * 
 * @example Print a list of values
 *     >> (apply print '("arg1", "arg2", '("a" "list")))
 *     arg1 , arg2 , quote,a,list
 *     => nil
 */
defun("apply", function (func, list) {
	// Input validation
	assert(arguments.length === 2, "(apply) requires 2 arguments (got " +
		arguments.length + ")");
	assert(typeof(func) === "function", "(apply) requires a function as its " +
		"first argument (got " + toLisp(func) + ")");
	assert(list instanceof Array, "(apply) requires a list as its second " +
		"argument (got " + toLisp(list) + ")");
	
	return func.apply(null, list);
});

/**
 * <pre>
 * Run each of the items in the given list through the given
 * function and returns a new list with the given return values.
 * 
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name map
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns The result of mapping the given list to the given function.
 * 
 * @param {function} func
 *     The function which gets passed each value in the given list.
 * @param {Array} list
 *     The list of values to pass to the given function.
 */
defun("map", function (func, list) {
	// Input validation
	assert(arguments.length === 2, "(map) requires 2 arguments (got " +
		arguments.length + ")");
	assert(typeof(func) === "function", "(map) requires a function as its first " +
		"argument (got " + toLisp(func) + ")");
	assert(list instanceof Array, "(map) requires a list as its second argument (got " +
		toLisp(list) + ")");
	
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
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name props
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @returns A new object containing only the values on the given
 *          object specified by the list of keys.
 * 
 * @param {object} object
 *     The object containing the values to return (specified by the
 *     given list of keys).
 * @param {mixed} value
 *     The list of keys (or single key) specifying which values to
 *     return in the resulting object.
 */
defun("props", function (object, list) {
	// Input validation
	assert(arguments.length === 2, "(props) requires 2 arguments (got " +
		arguments.length + ")");
	
	// Allow the user to supply a single key that's not in a list. This
	// is simply for convenience.
	if (!(list instanceof Array)) {
		list = [list];
	}
	
	function makeObject (fromobj, toobj, keyset) {
		var value;
		var first;
		for (var i = 0; i < keyset.length; i++) {
			try {
				value = keyset[i];
				if (value instanceof Array && value.length == 1) {
					value = value[0];
				}
				if (value instanceof Array) {
					if (value.length === 0) {
						continue;
					}
					first = value[0];
					toobj[first] = toobj[first] || {};
					makeObject(fromobj[first], toobj[first], value.slice(1));
				} else {
					toobj[value] = fromobj[value];
				}
			} catch (e) {
				if (e instanceof TypeError) {
					return toobj;
				}
				throw e;
			}
		}
		return toobj;
	}
	return makeObject(object, {}, list.map(function (k) { return String(k).split("."); }));
});

/**
 * <pre>
 * Creates and returns a list of (key value) pairs from the given object.
 * </pre>
 * 
 * @tested
 * 
 * @name items
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {object} object
 *     The object from which to create the (key value) pairs.
 * 
 * @example Basic objects
 *     >> (items (object :one 2 :three 4 :five 6))
 *     => (("one" 2) ("three" 4) ("five" 6))
 * 
 * @example Arrays
 *     >> (items '(one two three))
 *     => (("0" one) ("1" two) ("2" three))
 */
defun("items", function (value) {
	// Input validation
	assert(arguments.length === 1, "(items) requires 1 argument (got " +
		arguments.length + ")");
	
	var items = [];
	for (var key in value) {
		items.push([key, value[key]]);
	}
	
	return items;
});

/**
 * <pre>
 * Returns the element at the given index in the given sequence.
 * 
 * FIXME: Should this accept a list of indices as well? e.g. (nth a '(1 3))
 * </pre>
 * 
 * @name nth
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {sequence} sequence
 *     The sequence that contains the value to return.
 * @param {number} index
 *     The index to return on the given sequence.
 * 
 * @example Basic usage
 *     >> (nth '(one two three) 1)
 *     => two
 */
defun("nth", function (sequence, index) {
	// Input validation
	assert(arguments.length === 2, "(nth) requires 2 arguments (got " +
		arguments.length + ")");
	assert(typeof(index) === "number", "(nth) requires a number as its second argument " +
		"(got " + toLisp(index) + ")");
	
	if (sequence.length === 0) {
		return null;
	}
	
	return sequence[index];
});

/**
 * <pre>
 * Returns all but the first element in the given sequence.
 * 
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name rest
 * @lisp
 * @function
 * @member lisp.functions
 */
var _function_rest; // Defined in /src/lisp/functions.lisp

/**
 * <pre>
 * Returns the first element in the given sequence.
 * 
 * TODO: Document me
 * TODO: Add examples
 * 
 * FIXME: This should be a macro: `(nth ,list 0)
 * </pre>
 * 
 * @tested
 * 
 * @name first
 * @lisp
 * @function
 * @member lisp.functions
 */
var _function_first; // Defined in /src/lisp/functions.lisp

/**
 * <pre>
 * Returns the second element in the given sequence.
 * 
 * TODO: Document me
 * TODO: Add examples
 * 
 * FIXME: This should be a macro: `(nth ,list 1)
 * </pre>
 * 
 * @tested
 * 
 * @name second
 * @lisp
 * @function
 * @member lisp.functions
 */
var _function_second; // Defined in /src/lisp/functions.lisp

/**
 * <pre>
 * Returns the third element in the given sequence.
 * 
 * TODO: Document me
 * TODO: Add examples
 * 
 * FIXME: This should be a macro: `(nth ,list 2)
 * </pre>
 * 
 * @tested
 * 
 * @name third
 * @lisp
 * @function
 * @member lisp.functions
 */
var _function_third; // Defined in /src/lisp/functions.lisp

/**
 * <pre>
 * TODO: Test me
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @name push
 * @lisp
 * @function
 * @member lisp.functions
 */
defun("push", function (list, value) {
	// Input validation
	assert(arguments.length === 2, "(push) requires 2 arguments (got " +
		arguments.length + ")");
	assert(list instanceof Array, "(push) requires an Array as its first " +
		"argument (got " + toLisp(list) + ")");
	
	list.push(value);
	return list;
});

/**
 * <pre>
 * Sorts and returns the given array. The original array is
 * actually modified by this function.
 * 
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name sort!
 * @lisp
 * @function
 * @member lisp.functions
 */
var _function_sort_exc; // Defined in /src/lisp/functions.lisp

/**
 * <pre>
 * Sorts and returns a copy of the given array. The original
 * array is left untouched.
 * 
 * TODO: Document me
 * TODO: Add examples
 * </pre>
 * 
 * @tested
 * 
 * @name sort
 * @lisp
 * @function
 * @member lisp.functions
 */
var _function_sort; // Defined in /src/lisp/functions.lisp

/**
 * <pre>
 * Takes an object and returns its length property. This is just another
 * way of doing "return object.length".
 * 
 * Works with arrays, strings, the arguments object, and anything else
 * that has a length property.
 * </pre>
 * 
 * @tested
 * 
 * @name length
 * @lisp
 * @function
 * @member lisp.functions
 * 
 * @param {object} object
 *     The object whose length is to be returned.
 * 
 * @example Get the length of a string
 *     >> (length "hello, lisp")
 *     => 11
 * 
 * @example Get the length of an array
 *     >> (items this)
 *     => (("ps1" ">> ") ...) ; 8 more items
 *     >> (length (items this))
 *     => 9
 */
defun("length", function (object) {
	// Input validation
	assert(arguments.length === 1, "(length) requires 1 argument (got " +
		arguments.length + ")");
	assert(object.hasOwnProperty("length"), "(length) requires a sequence " +
		"argument (got " + toLisp(object) + ")");
	
	return object.length;
});

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

})((typeof(window) != "undefined") ? window : global); // For compatibility with node.js

// Set this library up to work with node.js
if ((typeof(window) == "undefined") &&
	(typeof(global) == "object") && global && // Make sure it isn't null
	(typeof(require) == "function") &&
	(typeof(exports) == "object") && exports) {
	// We are probably running in node.js now.
	// FIXME: Find a better way to tell we're running in node.js
	
	var sys = require("sys"),
		fs  = require("fs"),
		path = require("path");
	
	lisp.log = sys.puts;
	
	function FileNotFound (message) {
		this.toString = function () {
			return "FileNotFound: " + message;
		};
	}

	lisp.load = function (filepath, paths) {
		paths = paths || require.paths;
		paths.unshift(""); // A way to check filepath on it's own (with the
		                   // least amount of extra code).
		for (var i = 0; i < paths.length; i++) {
			var p = path.normalize(path.join(paths[i], filepath));
			var contents = null;
			try {
				contents = fs.readFileSync(p);
			} catch (e) {
				if (e instanceof FileNotFound) {
					throw e;
				}
			}
			if (contents) {
				return lisp.eval(contents);
			}
		}
		throw new FileNotFound("File '" + filepath + "' not found");
	};
	
	for (var key in lisp) {
		if (lisp.hasOwnProperty(key)) {
			exports[key] = lisp[key];
		}
	}
}

