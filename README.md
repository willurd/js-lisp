# js-lisp: A 100% JavaScript lisp interpreter for browser scripting

Disclaimer: DO NOT attempt to use this project for any production code whatsoever. This project is still VERY young; if there were such a thing as negative version numbers, it would have one. For now, this is a toy project. It is my hope that js-lisp will one day grow up to become a solid library devs around the world can use to sefely get their Lisp fix while they get their JavaScript fix, but until that time, DO NOT use this in production code (and if you do, I'm not responsible for the craziness that will most certainly ensue thereafter).

## What lisp are you using?

Currently, it's a very basic lisp that doesn't even run on Cons lists (it runs on JavaScript Arrays behind the scenes). The focus so far has been on going through the Common Lisp spec and porting as many applicable constructs as possible, but the language has already strayed from CL in a couple of ways (view the CL port document at /docs/port/, after running rake webserver).

The options are to make a lisp just for JavaScript, as Clojure has done for Java, or to port an existing lisp (CL, Scheme, etc).

## Which files do I use?

Unless you want to make modifications to js-lisp, the file you care about is:

* /build/lisp.js

or if you don't care about looking at the code:

* /build/lisp.min.js

## How do I view the tests and html documentation?

This project comes with a little helper script that runs a SimpleHTTPServer (Python) at the project root, displaying the main html page of the project's documentation.

The web server makes available the test suite, documentation, and all of the examples contained within this project.

To run the server:

    rake webserver

You will need to install rake in order to run that command:

    gem install rake

Don't forget to init and update the submodules after cloning this project:

    git submodule init
	git submodule update

## Developing js-lisp

If you're feeling adventurous enough to modify js-lisp, there's one specific rake command you'll want to run:

    rake watch

js-lisp, the project, is actually a bunch of JavaScript files that are all combined into one when the project is built. `rake watch` will run a program that automatically builds the project when there are modifications anywhere under the /src directory. This will make development go much faster for you.

If you feel like building the project manually, its

    rake build

and for the minified version:

    rake minify

Also, if you add any new javascript files, make sure to add them to the lisp.js list in /build.yaml so they will get compiled into the final /build/lisp.js file.

## You said js-lisp is so buggy I shouldn't use it in production code. What gives?

Ok, you misunderstood me. js-lisp is young. Really young. Having said that, it works. Pretty well actually. It's also got a pretty solid test suite, with pretty high code coverage. In fact, in terms of LOC the test-to-code ratio is almost 1:1. On top of that, 99% of the test code is actually written in lisp and interpreted by js-lisp. So use it. Do awesome things with it. Share it with your friends. Share what you're doing with me. Just don't use it in production code. Chances are there are many places in it that are weak as a feather, and I garuntee you your users would find them. And be mad at you.

## Anything else?

This project has only been tested in Chrome on a Mac. Like I said, don't use this in production code.
