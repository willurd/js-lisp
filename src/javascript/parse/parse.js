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
