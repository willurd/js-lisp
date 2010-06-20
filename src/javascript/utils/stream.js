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
