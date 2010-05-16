JSTest.TestCase({
	name: 'Parser Tests',
	
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
	
	// symbol
	
	testParseSymbol: function () {
		this.assertEqual(lisp.parse.symbol("abcd").value, "abcd");
		this.assertEqual(lisp.parse.symbol("document.getElementById").value,
		 	"document.getElementById");
		this.assertEqual(lisp.parse.symbol("MyClass/myvar").value, "MyClass/myvar");	
		this.assertEqual(lisp.parse.symbol("  space-before").value, "space-before");
		this.assertEqual(lisp.parse.symbol("space-after  \t\n ").value, "space-after");
	},
	
	// sexp
	
	testParseSexp: function () {
		this.assertEqual(lisp.parse.script('(puts "hello!")')[0], ["puts", "hello!"]);
	},
	
	// any
	
	testParseAny: function () {
		this.assertEqual(lisp.parse.any('"hello"'), "hello");
	},
});
