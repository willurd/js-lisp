JSTest.TestCase({
	name: 'Parser Tests',
	
	testParseSexp: function () {
		this.assertEqual(lisp.parse('(puts "hello!")')[0], ["puts", "hello!"]);
	},
});
