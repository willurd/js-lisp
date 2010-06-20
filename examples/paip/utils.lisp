(setq ArgumentError lisp.exception.ArgumentError)

(defmacro test (name & plist)
  "This is just a nicer syntax for defining a JSTest test case."
  `(JSTest.TestCase (object
	:name ,name
    ,@plist)))
