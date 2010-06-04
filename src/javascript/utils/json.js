function toJSON (object, pretty, levels, level) {
	levels = levels || 2; // Default levels
	level = level || 0;
	var done = level >= levels;
	var newline = pretty ? '\n' : '';
	var singleprefix = times(' ', 2);
	var prefix  = pretty ? times(singleprefix, level) : '';
	
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
			
			if (!done) {
				for (var i = 0; i < object.length; i++) {
					json += toJSON(object[i], pretty, levels, level+1) + ', ';
				}
			} else {
				json += ' ... ';
			}
			
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
			
			if (!done) {
				json = json + newline;
				for (var key in object) {
					if (object.hasOwnProperty(key)) {
						json += prefix + singleprefix + '"' + key + '": ' +
							((object[key] == window) ? "[window]" : toJSON(object[key], pretty, levels, level+1)) +
							', ' + newline;
					}
				}
				json = json.replace(/,\s*$/, '') + newline + prefix;
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
		return object.toString();
	}
}
