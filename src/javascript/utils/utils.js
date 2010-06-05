/**
 * The method used for (equal) equality in js-lisp.
 * 
 * @return Whether a and b are equal from js-lisp's perspective.
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
