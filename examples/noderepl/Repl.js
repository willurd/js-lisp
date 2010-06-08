var sys  = require("sys"),
	lisp = require("../../build/lisp");

exports.Repl = lisp.Class.extend({
	init: function (props) {
		props = props || {};
		this.ps1 = props.ps1 || ">> ";
		this.ps2 = props.ps2 || ".. ";
		this.ps3 = props.ps3 || "=> ";
		this.multilineCommand = false;
		this.data = '';
	},
	handleData: function (data) {
		this.data += data.toString();
		if (data == '\n') {
			this.newCommand();
			return;
		}
		try {
			var ret = lisp.eval(this.data);
			var output = lisp.env.get("repl-represent")(ret); // repl-represent is defined in utils.lisp
			sys.puts(this.ps3 + output);
			this.multilineCommand = false;
			this.data = '';
			this.newCommand();
		} catch (e) {
			if (e instanceof lisp.exception.StreamEOFException) {
				this.multilineCommand = true;
				this.newCommand();
			} else {
				this.multilineCommand = false;
				this.data = '';
				this.handleError(e);
			}
		}
	},
	handleError: function (error) {
		sys.puts(error);
		this.newCommand();
	},
	newCommand: function () {
		var prompt = this.multilineCommand ? this.ps2 : this.ps1;
		process.stdout.write(prompt);
	}
});
