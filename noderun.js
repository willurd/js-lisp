require.paths.unshift(__dirname);

var sys  = require("sys"),
	path = require("path"),
	lisp = require("build/lisp"),
	exec = require("child_process").exec;

function usage () {
	command = process.argv.slice(0, 2).join(" ");
	sys.puts("Usage: " + command + " script.lisp");
	process.exit(1);
}

function strip (string) {
	return string.replace(/(^\s+|\s+$)/ig, "");
}

exec("pwd", function (error, stdout, stderr) {
	require.paths.unshift(strip(stdout));
	
	if (process.argv.length > 3) {
		sys.puts("Too many arguments");
		usage();
	} else if (process.argv.length === 3) {
		lisp.env.set("lisp", lisp);
		lisp.env.set("require", require);
		lisp.load("build/core.lisp");
		lisp.load("build/lib/path.lisp");
		// Load the given script.
		var filename = process.argv[2];
		lisp.load(filename);
	} else {
		sys.puts("Too few arguments");
		usage();
	}
});
