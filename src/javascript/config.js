const WHITESPACE = " \t\n\r";

var ROOT_ENV = new Env(new Env(null, global), {
	"t": true,
	"true": true,
	"false": false,
	"nil": null,
	"null": null,
	"undefined": undefined,
	
	"*features*": [new Keyword("notmuch")],
});
