/*jsl:ignore*/ // Suppress jsl warnings

(function () {
	var initializing = false;
	var fnTest = (/xyz/).test(function(){xyz;}) ? (/\b_super\b/) : /.*/;
	
	/**
	 * <p>Defines a base class from which to create new classes that can be
	 * extended into new classes as well.</p>
	 * 
	 * <p>This constructor does nothing.</p>
	 * 
	 * <p>Modified from: http://ejohn.org/blog/simple-javascript-inheritance/</p>
	 * 
	 * @class
	 * @name Class
	 */
	this.Class = function () {};
	
	/**
	 * <p>Creates a new class that inherits from the calling class.</p>
	 * 
	 * @function
	 * 
	 * @param {string, object} classNameOrProps
	 *     Either the name of the class, or an object containing the class' properties.
	 * @param {object} props
	 *     An object containing the class' properties (only if classNameOrProps
	 *     specifies the class name).
	 */
	Class.extend = function (classNameOrProps, props) {
		var _super = this.prototype;
		var className = props ? classNameOrProps : "Class";
		props = props || classNameOrProps;
		
		// Instantiate a base class (but only create the instance,
		// don't run the init constructor)
		initializing = true;
		var prototype = new this();
		initializing = false;
		
		// Copy the properties over onto the new prototype
		for (var name in props) {
			// Check if we're overwriting an existing function
			if (typeof props[name] == "function" &&
				typeof _super[name] == "function" &&
				fnTest.test(props[name])) {
				prototype[name] = (function(name, fn){
					return function() {
					var tmp = this._super;
					
					// Add a new ._super() method that is the same method
					// but on the super-class
					this._super = _super[name];
					
					// The method only need to be bound temporarily, so we
					// remove it when we're done executing
					var ret = fn.apply(this, arguments);        
					this._super = tmp;
					
					return ret;
					};
				})(name, props[name]);
			} else {
				prototype[name] = props[name];
			}
		}
		
		// The new class
		var NewClass = function () {
			// All construction is actually done in the init method
			if (!initializing && this.init)
				this.init.apply(this, arguments);
		}
		
		NewClass.className = className;
		NewClass.prototype = prototype;
		NewClass.constructor = Class;
		NewClass.extend = arguments.callee;
		
		return NewClass;
	};
})();

/*jsl:end*/
