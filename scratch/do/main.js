require.paths.unshift(__dirname);

var sys   = require("sys"),
	path  = require("path"),
	lisp  = require("../../build/lisp"),
	Script = process.binding('evals').Script;

lisp.env.set("lisp", lisp);

lisp.load("../../build/core.lisp");
lisp.load("main.lisp");
