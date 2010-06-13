require.paths.unshift(__dirname);

var sys   = require("sys"),
	path  = require("path"),
	stdin = process.openStdin(),
	lisp  = require("../../build/lisp");

// Setup the lisp environment
lisp.env.set("lisp", lisp);
lisp.env.set("this", repl);
lisp.load("../../build/core.lisp");
lisp.load("utils.lisp");
lisp.load("repl.lisp");

// Print the welcome message
sys.puts("Welcome to the js-lisp REPL\n" +
		 "Type ^C or ^D to exit");

// Setup the repl
var Repl = lisp.env.get("Repl");
var repl = new Repl();

// Setup the stdin listeners
stdin.addListener('data', function (data) {
	repl.handleData(data);
	return true;
});
stdin.addListener('end', function () {
	sys.puts("");
});
