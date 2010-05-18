# TODO

* Look through Practical Common Lisp for ideas on what to implement next
	* On chapter: http://www.gigamonkeys.com/book/practical-a-simple-database.html

* Write function: (format)
	* (format t) writes to the console
	* (format nil) returns the value
	* Possibly use one of these implementations (or use them for reference):
		* http://www.diveintojavascript.com/projects/sprintf-for-javascript
		* http://phpjs.org/functions/sprintf:522
* Write functions: (to-upper), (to-lower)
* Think about the fact that 'nil' is actually the empty list (right now it's 'null')
* Test return values of sexps (including null when there is no return)
* Write tests for any lisp functions that have not been tested yet
* Write macros: (if), (when), (cond)
* Write function: (to-boolean)
* Write function: (load) (loads and evaluates a lisp file inline, synchronously)
* Write macro: (defmacro)
* Look at how different lisps handle users trying to define both functions and macros (and vars for that matter) that have already been defined
* Figure out a better place to put all the library functions and macros
* Do lambda unit tests
* Make a Cons class for sexp's (instead of using Array)
	* Write related functions: (first), (rest), (second), (third), (last)
	* OR: (car), (cdr), (caar), (cadr), (caaar), etc
* Write macro: (is-list) (for Cons')
* Parse comments
* Make a webpage at the root of /src that runs on `rake run`
	* This webpage should link off to the test runner(s) and all examples
	* As a demonstration, all the interactions of this page should be written in lisp
* Update the README big time
* Think about how to get line numbers for lisp scripts for debugging
* Take a look at the CL, scheme, elisp, and closure specs for ideas. OR just conform to one language. OR make an implementation per language.
