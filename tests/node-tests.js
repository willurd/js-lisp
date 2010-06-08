var sys    = require("sys"),
	fs     = require("fs"),
	JSTest = require("../support/vendor/JSTest/src/jstest"),
	lisp   = require("../build/lisp");

const JS_TESTS   = "./tests";
const LISP_TESTS = "tests.lisp";

// Include our javascript tests
require(JS_TESTS);

// Eval our lisp tests
lisp.env.set("JSTest", JSTest);
lisp.eval(fs.readFileSync(LISP_TESTS));

with (JSTest) {
	run(false, handlers.NodeJSHandler);
}
