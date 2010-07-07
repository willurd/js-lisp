(setq ArgumentError lisp.exception.ArgumentError)

(defmacro test (name & plist)
  "This is just a nicer syntax for defining a JSTest test case."
  `(JSTest.TestCase (object
	:name ,name
    ,@plist)))

(defmacro divider (name)
  `(JSTest.Divider ,name))

(defun arg-name-part (argname:lisp.Symbol)
  (let ((argname (to-string argname)))
    (to-symbol (first (argname.split ":")))))
