# TODO

* Write some Env tests
* Update Env to handle all lookup requests (even if it's from window)
* Parse numbers
* Do lambda unit tests
* Make a Cons class for sexp's (instead of using Array)
* Parse comments
* Make a webpage at the root of /src that runs on `rake run`
	* This webpage should link off to the test runner(s) and all examples
	* As a demonstration, all the interactions of this page should be written in lisp
* Think about maintaining line numbers for lisp scripts for debugging


Allow code in a <script type="text/lisp"> tag that has a src specified. eval the code in the tag after the src script has been loaded and eval'd, and only after that.

lisp.eval() is how scripts should be run. You should be allowed to specify your own initial environment (a simple object containing state) when eval'ing lisp code. The default will just be "window".

js-lisp is a good step toward a port of emacs to the browser. The hope is that all the emacs extensions and modes and scripts will only need to be modified ever-so-slightly, if at all, to be used with a browser emacs.

Get scoping right. (let) is setting the scope, but not resetting when it drops out.
