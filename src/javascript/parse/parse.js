var parse = {
	NUMBER_FORMATS: [
		(/^(([+-]{1})?[0-9]+(?:\.(?:[0-9]+))?(?:e([0-9]+))?)(?:\s+|\)|$)/),
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
			throw new parse.ParserException("Invalid number at position " + stream.position +
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
