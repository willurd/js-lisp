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
