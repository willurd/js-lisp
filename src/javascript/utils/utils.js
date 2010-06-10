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
