require.paths.unshift(__dirname);

var sys   = require("sys"),
	path  = require("path"),
	lisp  = require("../../build/lisp");

function usage () {
	command = process.argv.slice(0, 2).join(" ");
	sys.puts("Usage: " + command + " [script.lisp]");
	process.exit(1);
}

function loadLispCore () {
	lisp.load("../../build/core.lisp");
	lisp.load("../../build/lib/path.lisp");
}

if (process.argv.length > 3) {
	sys.puts("Too many arguments");
	usage();
} else if (process.argv.length === 3) {
	// Load the given script.
	var filename = process.argv[2];
	loadLispCore();
	lisp.load(filename);
} else {
	var stdin = process.openStdin();
	
	// Setup the lisp environment
	lisp.env.set("lisp", lisp);
	loadLispCore();
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
}
