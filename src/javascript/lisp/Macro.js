var _inMacro = false; // So we can have expressions that only work inside macros
var Macro = Class.extend({
	init: function (callable) {
		this.callable = function () {
			_inMacro = true;
			var ret = callable.apply(null, arguments);
			_inMacro = false;
			return ret;
		};
	}
});
