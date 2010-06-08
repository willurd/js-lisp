var sys   = require("sys"),
	fs    = require("fs"),
	stdin = process.openStdin(),
	lisp  = require("../../build/lisp"),
	Repl  = require("./Repl").Repl;

// Print the welcome message
sys.puts("Welcome to the js-lisp REPL\n" +
		 "Type ^C or ^D to exit");

// Setup the repl
var repl = new Repl();
repl.newCommand();

// Setup the lisp environment
lisp.env.set("lisp", lisp);
lisp.env.set("this", repl);
lisp.eval(fs.readFileSync("utils.lisp"));

// Setup the stdin listeners
stdin.addListener('data', function (data) {
	repl.handleData(data);
	return true;
});
stdin.addListener('end', function () {
	sys.puts("");
});
