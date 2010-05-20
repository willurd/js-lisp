var sys  = require("sys"),
	lisp = require("../build/lisp");

sys.puts(lisp.eval('(puts "hello!")'));
