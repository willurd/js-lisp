# TODO

* Port all (JSTest.TestCase ...) to (test ...)
* Finish testing (and preferably documenting) the functionality that already exists.
* Rewrite cond with (defmacro)
* Create ! as a shortcut for (not ...)
* Create !! as a shortcut for (not (not ...))
* Create +=, -=, /=, *=, %=, &&= (and-equal?)
* Create <<, >>, &, | (bitwise operations)
* Create <<=, >>=, &=, |=
* Write the generic (do) macro
* Add regex literals /.../..
* Think about adding the concept of generators
  * (yield 'some 'values) throws a lisp.exception.YieldException
* Create all of the rest of the functions/macros that have TODOs in the project ((case) or (switch), etc)
* Think of ways to break the current (defmacro) implementation because I don't trust it.
* Start thinking of all of the \*features* that can be set. For example:
  * What OS you're on: (:os-windows, :linux, :macos, etc)
  * What OS flavor you're running: (:os-flavor-xp, :os-flavor-ubuntu, :os-flavor-leopard, etc)
  * What version your OS is: (:os-version-sp2, :os-version-2.62, etc)
  * What browser you're in: (:browser-chrome, :browser-safari, :browser-firefox, etc)
  * What version your browser is: (:browser-version-1.2, etc)
  * Supported features: (:web-sockets, :css3, :web-workers, etc)
  * Whether you're in the browser or a console: (:browser, :console)
  * What JavaScript runtime you're in: (:runtime-node, :runtime-rhino, etc)
* Add support for feature expressions
  * #+firefox (do something for firefox)
  * #-windows (only do this if we're not on windows)
  * #+(or unix macos)
* Add a couple functions and macros to make working with features nicer
  * Function: (feature :feature), (feature '(:list :of :features))
    * (defun feature (feature-or-list)
        (!! (find-any (ensure-list feature-or-list) \*features*)))
  * Function: (when (feature :feature-name) ...), (when (feature '(:feature :list)) ...)
  * Macro: (when-feature :feature-name ...) -> `(when (feature ,feature-name) ,@body)
* Put an online book or tutorial/example set into the roadmap (in the distant future). Also, make a roadmap.
* Put a website into the roadmap (for the not-so-distant future).
* Fix backticks, unquotes (commas), and list expanders (@).
* Create SpecialForm and convert all relevant "macros" to special forms. Update Macro to return expanded code on .expand(), and resolved code on .call() and .apply().
* Do &opt (&optional) and &key stuff.
* Make the browser REPL evaluate expressions each on their own line if it receives more than one
* Add a transcript to the REPL (just use controller.history)
* Ideas
    * Look through Practical Common Lisp for ideas on what to implement next
        * On chapter: http://www.gigamonkeys.com/book/practical-a-simple-database.html
    * Take a look at the CL, scheme, elisp, and closure specs for ideas. OR just conform to one language. OR make an implementation per language.
	* Start putting together a library of lisp methods and macros
	* Good reference: http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/
* Testing
	* Test return values of sexps (including null when there is no return)
* Write better/more documentation for current functions/macros
* Write macro: (unless) (opposite of (when))
* Write macros: (incf), (decf)
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
* Think about how to get line numbers from lisp scripts for debugging
* Make browser extensions:
	* Chrome
	* Firefox (using Jetpack)
	* Greasemonkey
* Add macro: (prop <object> <prop.dot.path>)
	* Example: (prop ($ this) some.func)
	* Or Add macro: (get "string.dot.path" [object])
	  Examples:
		- (get "aVar")
		- (let ((prop "some.prop"))
			(get prop))
		- (get "some.prop" this)
* Research how closures are implemented in languages
* Think about changing how Env works:
	Current:
		* Javascript libs can't access (let)'d variables
	New:
		* There's only one Env instance which uses global for everything
		* Env has "scopes"
			* Each scope is a set of vars that have changed and their old values so the Env can be reset when that scope goes out
		* scope = Scope(parentScope, env)
		* Closures copy scopes (which takes the current values, instead of the old values)
* Handle empty list: if an sexp has no arguments, it's an empty list
* Think about Cons' and how they'll fit in (or if they will...)
* Test (Class.extend (object ...))
* Test defining a class from scratch (using the prototype property)
* Think about defining a (class) macro (or maybe reusing CL's defclass, or at least borrowing from it)
* Write the generic do macro
* Think about beginning to tackle (loop)
* Think about a (formatcl) function that uses CL's format language
* Think about writing a mini app/game/widget/whatever in 100% lisp (or maybe just using jQuery?)
* Find some better ways to do documentation. Latex? Automatically-generated latex files from jsdoc perhaps? Convert the latex to PDF _and_ HTML?
