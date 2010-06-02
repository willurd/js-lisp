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
