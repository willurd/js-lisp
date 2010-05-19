# TODO

* Look through Practical Common Lisp for ideas on what to implement next
	* On chapter: http://www.gigamonkeys.com/book/practical-a-simple-database.html
* Start putting together a library of lisp methods and macros

* Test return values of sexps (including null when there is no return)
* Think about the fact that 'nil' is actually the empty list (right now it's 'null')
* Write tests for any lisp functions that have not been tested yet
* Write macros: (if), (when), (cond)
* Write function: (load) (loads and evaluates a lisp file inline, synchronously)
* Write macro: (defmacro)
* Make some proofs of concept (or "examples"). Some good ones would be:
	* A lisp repl in the browser (javascript vt100 emulator)
	* An interactive UI using a popular framework (or several UIs using different frameworks)
	* A game (a graphical one)
	* A graphics-intensive simulation
	* An animation library or game-development framework
	* An in-browser text editor (js-emacs or, more likely, something much simpler)
	* A dynamic form (like a multi-"page" survey)
	* An audio sequencer
	* Any others?
* Look at how different lisps handle users trying to define both functions and macros (and vars for that matter) that have already been defined
* Figure out a better place to put all the library functions and macros
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
* Take a look at the CL, scheme, elisp, and closure specs for ideas. OR just conform to one language. OR make an implementation per language.
