# js-lisp: A 100% JavaScript lisp interpreter for browser scripting

Disclaimer: DO NOT attempt to use this project for any production code whatsoever. This project is still VERY young; if there were such a thing as negative version numbers, it would have one. For now, this is a toy project. It is my hope that js-lisp will one day grow up to become a solid library devs around the world can use to sefely get their Lisp fix while they get their JavaScript fix, but until that time, DO NOT use this in production code (and if you do, I'm not responsible for the craziness that will most certainly ensue thereafter).

## Usage

*Step 1: Include js-lisp*

Include the js-lisp code (either lisp.js or lisp.min.js from inside /build/) _before_ you try to evaluate any of your lisp code.

    <script src="/path/to/lisp.js"></script>

*Step 2: Include your lisp code*

    <script type="text/lisp" src="/path/to/code.lisp" onload="lisp.dotag(this)">
    ; Lisp code can go here too. It will be evaluated after the lisp code
    ; pointed to in 'src' (if there is any).
    </script>

The `onload` part is the magic part. This is what evaluates the lisp code. Alternatively, if you really want to, you can call `lisp.run()` in JavaScript (again, _after_ including lisp.js) which will evaluate all script tags with `type="text/lisp"` that haven't been evaluated yet. It's important to note that `lisp.dotag()` and `lisp.run()` both remove each tag they evaluate from the page so those tags won't get evaluated again.

## Playing around at the REPL

You can play around with the repl here: [http://williambowers.net/projects/js-lisp/examples/repl/][0].

The REPL is located at /examples/repl/ (follow the steps under the section "How do I view the tests and html documentation?" to start a simple webserver at the root of the project), and works just like any other REPL. The console emulator is a modified version of [jquery-console][1].

Here are a few shots of the REPL in action:

![](http://github.com/willurd/js-lisp/raw/master/docs/readme/repl1.png)

![](http://github.com/willurd/js-lisp/raw/master/docs/readme/repl2.png)

![](http://github.com/willurd/js-lisp/raw/master/docs/readme/repl3.png)

Have fun!

[0]: http://williambowers.net/projects/js-lisp/examples/repl/
[1]: http://github.com/chrisdone/jquery-console

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

First thing, make sure you init and update the submodules after cloning this project:

    git submodule init
	git submodule update

Next, you'll need to install rake in order to run the command that starts the webserver:

    gem install rake

Now start the server:

    rake webserver

The webserver will be located at localhost:8000 (or 127.0.0.1:8000 if that doesn't work).

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

Ok, you misunderstood me. js-lisp is young. Really young. Having said that, it works. Pretty well actually. It's also got a pretty solid test suite, with pretty a lot of code coverage. On top of that, 99% of the test code is actually written in lisp and interpreted by js-lisp. So use it. Do awesome things with it. Share it with your friends. Share what you're doing with me. Just don't use it in production code. Chances are there are many places in it that are weak as a feather, and I garuntee you your users would find them. And be mad at you.

## Browser Support

The following is a list of browsers where all of the examples worked and all of the tests passed, the last time that browser was checked.

**OS X Snow Leopard**

* Chrome 5 (first stable mac release)
* Safari 4
* Firefox 3.5

**Windows Vista**

* Chrome 5
* Firefox 3.5

**Linux, 2.6.33-ARCH**

* Chrome 5
* Firefox 3.7 (nightly)
