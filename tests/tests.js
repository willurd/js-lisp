// Set this library up to work with node.js
if ((typeof(window) == "undefined") &&
	(typeof(global) == "object") && global && // Make sure it isn't null
	(typeof(require) == "function") &&
	(typeof(exports) == "object") && exports) {
	// We are probably running in node.js now.
	// FIXME: Find a better way to tell we're running in node.js
	var JSTest = require("../support/vendor/JSTest/src/jstest"),
		lisp   = require("../build/lisp");
} else {
	var global = window;
}

JSTest.TestCase({
	name: 'Parser',
	
	// string
	
	testParseString: function () {
		this.assertEqual(lisp.parse.string('"this is a string"'), "this is a string");
		this.assertEqual(lisp.parse.string(' \t"this too "'), "this too ");
		this.assertEqual(lisp.parse.string(' \t\r " \nand  this"  \n\n  '), " \nand  this");
	},
	
	testParseStringBadInput: function () {
		this.assertRaises(lisp.parse.ParserException, lisp.parse.string, null, '(hello)');
		this.assertRaises(lisp.parse.ParserException, lisp.parse.string, null, 'test');
		this.assertRaises(lisp.parse.ParserException, lisp.parse.string, null, '123.45');
		this.assertRaises(lisp.parse.ParserException, lisp.parse.string, null, null);
		this.assertRaises(lisp.parse.ParserException, lisp.parse.string, null);
	},
	
	// number
	
	testParseNumber: function () {
		this.assertEqual(lisp.parse.number("345"), 345);
		this.assertEqual(lisp.parse.number("34.5"), 34.5);
		this.assertEqual(lisp.parse.number("-12"), -12);
		this.assertEqual(lisp.parse.number("  3.45e2   "), 3.45e2);
		this.assertEqual(lisp.parse.number("0377\n\r"), 0377);
		this.assertEqual(lisp.parse.number("\t0xFF\n"), 0xFF);
	},
	
	// symbol
	
	testParseSymbol: function () {
		this.assertEqual(lisp.parse.symbol("abcd").value, "abcd");
		this.assertEqual(lisp.parse.symbol("document.getElementById").value,
		 	"document.getElementById");
		this.assertEqual(lisp.parse.symbol("  space-before").value, "space-before");
		this.assertEqual(lisp.parse.symbol("space-after  \t\n ").value, "space-after");
	},
	
	// keyword
	
	testParseKeyword: function () {
		this.assertEqual(lisp.parse.keyword(":hello").value, "hello");
		this.assertEqual(lisp.parse.keyword(":good-bye").value, "good-bye");
		this.assertEqual(lisp.parse.keyword(" \t :sayonara \n").value, "sayonara");
	},
	
	// sexp
	
	testParseSexp: function () {
		this.assertEqual(lisp.parse.script('(print "hello!")')[0], ["print", "hello!"]);
	},
	
	// any
	
	testParseAny: function () {
		this.assertEqual(lisp.parse.any('"hello"'), "hello");
	},
});

JSTest.TestCase({
	name: 'Environment (Env class)',
	
	setup: function () {
		this.env = new lisp.Env(new lisp.Env(null, global), {});
	},
	
	testBasicGet: function () {
		this.assertUndefined(this.env.get("bob"));
	},
	
	testBasicSet: function () {
		this.env.set("aname", "Bill");
		this.assertEqual(this.env.get("aname"), "Bill");
	},
	
	testGlobalGet: function () {
		this.assertTrue(this.env.get("global") == global);
	},
	
	testHas: function () {
		this.assertFalse(this.env.has("bob"));
		this.assertTrue(this.env.has("global"));
		
		this.env.set("bob", false);
		this.assertTrue(this.env.has("bob"));
	},
	
	testGetDotPath: function () {
		this.assertEqual(this.env.get("document.getElementById"), document.getElementById);
		this.assertEqual(this.env.get("global.Array.prototype"), global.Array.prototype);
	},
	
	testSetDotPath: function () {
		this.assertNotEqual(global.firstName, "Bill");
		this.env.set("global.firstName", "Bill");
		this.assertEqual(firstName, "Bill");
	}
});
