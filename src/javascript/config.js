var WHITESPACE = " \t\n\r";

var ROOT_ENV = new Env(new Env(null, global), {
	"t": true,
	"true": true,
	"f": false,
	"false": false,
	"nil": null,
	"null": null,
	"undefined": undefined,
	"NaN": NaN,
	
	/**
	 * This is here because values defined in the lisp env clobber the
	 * global namespace (this is on purpose, so those values can be used
	 * by other JavaScript code). Howver, one thing we definitely don't
	 * want to do is clobber standard JavaScript functions, like eval.
	 * 
	 * TODO: Test me
	 */
	"eval": function (expression) {
		// Input validation
		assert(arguments.length === 1, "(eval) requires 1 argument (got " +
			arguments.length + ")");
		
		return resolve(expression);
	},
	
	"*features*": [_K("notmuch")]
});
