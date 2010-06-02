# TODO

* Look through Practical Common Lisp for ideas on what to implement next
    * On chapter: http://www.gigamonkeys.com/book/practical-a-simple-database.html
* Take a look at the CL, scheme, elisp, and closure specs for ideas. OR just conform to one language. OR make an implementation per language.
* Start putting together a library of lisp methods and macros
* Good reference: http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/

* Testing
    * Write tests for any lisp functions that have not been tested yet
	* Write tests for each function/macro for invalid input
	* Test return values of sexps (including null when there is no return)
* Tools
    * Make a repl (using jquery-console perhaps)
* Write better/more documentation for current functions/macros
* Write macro: (cond)
* Write macro: (unless) (opposite of (when))
* Write function: (load) (loads and evaluates a lisp file inline, synchronously)
* Think about the fact that 'nil' is actually the empty list (right now it's 'null')
* Write macro: (defmacro) (and corresponding parsers: ",", "`", "@")
* Make more error classes for each type of error (ArgumentError, TypeError, etc)
* Make some proofs of concept (or "examples"). Some good ones would be:
	* A lisp repl in the browser (javascript vt100 emulator)
	* An interactive UI using a popular framework (or several UIs using different frameworks)
	* A game (a graphical one)
	* A graphics-intensive simulation
	* An animation library or game-development framework
	* An in-browser text editor (js-emacs or, more likely, something much simpler)
	* A dynamic form (like a multi-"page" survey)
	* An audio sequencer
	* Something with this perhaps: http://sizzlejs.com/
	* Any others?
* Look at how different lisps handle users trying to define both functions and macros (and vars for that matter) that have already been defined
* Make a Cons class for sexp's (instead of using Array)
	* Write related functions: (first), (rest), (second), (third), (last)
	* OR: (car), (cdr), (caar), (cadr), (caaar), etc
* Write macro: (is-list) (for Cons')
* Write function: (is-array) (because at this point lists will be Cons')
* Make a webpage at the root of /src that runs on `rake run`
	* This webpage should link off to the test runner(s) and all examples
	* As a demonstration, all the interactions of this page should be written in lisp
* Update the README big time
* Think about how to get line numbers for lisp scripts for debugging
* Make browser extensions:
	* Chrome
	* Firefox (using Jetpack)
	* Greasemonkey

Add macro: (prop <object> <prop.dot.path>)
	Example: (prop ($ this) some.func)
OR
Add macro: (get "string.dot.path" [object])
	Examples:
		- (get "aVar")
		- (let ((prop "some.prop"))
			(get prop))
		- (get "some.prop" this)

Research how closures are implemented in languages

Think about changing how Env works:
	Current:
		* Javascript libs can't access (let)'d variables
	New:
		* There's only one Env instance which uses global for everything
		* Env has "scopes"
			* Each scope is a set of vars that have changed and their old values so the Env can be reset when that scope goes out
		* scope = Scope(parentScope, env)
		* Closures copy scopes (which takes the current values, instead of the old values)

Handle empty list: if an sexp has no arguments, it's an empty list

Think about Cons' and how they'll fit in (or if they will...)
