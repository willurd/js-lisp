(JSTest.TestCase (object
	:name "this Object in Lambdas"
	:testThisWorks (lambda ()
		(this.message "It works!")))) ;; The very fact the this.message works _is_ the test

(JSTest.TestCase (object
	:name "Numbers"
	:testOctals (lambda ()
		(this.assertEqual 0100 64))
	:testHex (lambda ()
		(this.assertEqual 0x40 64))
	:testNegative (lambda ()
		(this.assertTrue (< -.2 0))
		(this.assertEqual -0.2 -.2))
	:testPositive (lambda ()
		(this.assertTrue (> .2 0))
		(this.assertEqual 0.2 .2))
	:testScientificNotation (lambda ()
		(this.assertEqual 20e2 2000)
		(this.assertEqual 20e-2 0.2))))

(JSTest.TestCase (object
	:name "Strings"
	:testHardNewline (lambda ()
		(this.assertEqual "a
string" "a\nstring"))
	:testHardTab (lambda ()
		(this.assertEqual "a	string" "a\tstring"))))

(JSTest.TestCase (object
	:name "Functions As Function Calls"
	:testFunctionCallAsFirstArg (lambda ()
		(let ((o (object :func (lambda (x) (1+ x)))))
			(this.assertEqual 2 ((getkey o :func) 1))))
	:testLambdaAsFirstArg (lambda ()
		(this.assertEqual 2 ((lambda (x) (1+ x)) 1)))))

;; ================================================================================
;; MACROS
;; ================================================================================

(JSTest.Divider "macros")

(JSTest.TestCase (object
	:name "macro (quote)"
	:testNoArguments (lambda ()
		(this.assertRaises Error #'quote nil))
	:testOneArgument (lambda ()
		(this.assertNotRaises Error #'quote nil good))
	:testManyArguments (lambda ()
		(this.assertRaises Error #'quote nil))
	:testQuoteSymbol (lambda ()
		(this.assertEqual (quote hello) (quote hello) nil #'equal)
		(this.assertEqual (quote hello) (lisp.parse.any "hello") nil #'equal)
		(this.assertEqual (quote hello) (new lisp.Symbol "hello") nil #'equal)
		(this.assertNotEqual (quote hello) (quote goodbye)) nil #'not-equal)
	:testQuoteList (lambda ()
		(this.assertEqual (quote (one two)) (quote (one two)) nil #'equal)
		(this.assertEqual (quote (one two)) (lisp.parse.any "(one two)") nil #'equal)
		(this.assertEqual (quote (one two)) (list "one" "two")) nil #'equal)
	:testQuoteKeywords (lambda ()
		(this.assertEqual ':hello :hello nil #'equal))
	:testEvalQuoted (lambda ()
		(this.assertEqual (eval (quote (format nil "%0.2f" 10.123)))
						  (format nil "%0.2f" 10.123)))
	:testQuoteLiteral (lambda ()
		(this.assertEqual (quote hello!) 'hello! nil #'equal)
		(this.assertEqual (quote (hello!)) '(hello!) nil #'equal)
		(this.assertEqual (quote (quote hello!)) ''hello! nil #'equal))))

;; TODO: Test (resolve)
;; TODO: Test (explode)

(JSTest.TestCase (object
	:name "macro (defmacro)"
	:testBasic (lambda ()
		(let ((collect nil))
			(defmacro my-collect ((itemName lst) & body)
			  `(let ((set (list)))
		        (foreach (,itemName ,lst)
		          (when (progn ,@body)
		            (push set ,itemName)))
				set))
			(let ((lt3 (my-collect (item (list 1 2 3))
						 (< (second item) 3))))
				(this.assertEqual (map second lt3) '(1 2)))))
	:testRestArgsAtAllLevels (lambda ()
		(let ((testmacro nil)
			  (values nil))
			(defmacro testmacro (((a b & c) d & e) f g & h)
				`(list a b c d e f g h))
			(setq values (testmacro ((1 2 3 4 5) 6 7 8) 9 10 11))
			(this.assertEqual (length values) 8)
			(this.assertEqual (nth values 0) 1)
			(this.assertEqual (nth values 1) 2)
			(this.assertEqual (nth values 2) '(3 4 5))
			(this.assertEqual (nth values 3) 6)
			(this.assertEqual (nth values 4) '(7 8))
			(this.assertEqual (nth values 5) 9)
			(this.assertEqual (nth values 6) 10)
			(this.assertEqual (nth values 7) '(11))))
	:testReturnValue (lambda ()
		(this.assertInstanceOf (defmacro test) lisp.Macro))))

(JSTest.TestCase (object
	:name "macro (lambda)"
	:testNoArguments (lambda ()
		(this.assertNotRaises Error #'lambda nil))
	:testOneArgument (lambda ()
		(this.assertNotRaises Error #'lambda nil '()))
	:testManyArguments (lambda ()
		(this.assertNotRaises Error #'lambda nil '() 'hello 'joe))
	:testNonListFirstExpression (lambda ()
		(this.assertRaises Error #'lambda nil 'hello))
	:testUnsuppliedArguments (lambda ()
		(let ((func (lambda (one))))
			(this.assertNotRaises Error #'func nil))
		(let ((func (lambda (one two three))))
			(this.assertNotRaises Error #'func nil "arg 1" "arg 2"))
		((lambda (this two three)
			(this.assertUndefined three))
		 this "arg 2"))
	:testExtraArguments (lambda ()
		(let ((func (lambda ())))
			(this.assertNotRaises Error #'func nil "arg 1"))
		(let ((func (lambda (one two))))
			(this.assertNotRaises Error #'func nil "arg 1" "arg 2" "arg 3")))
	:testArgumentsVariable (lambda ()
		((lambda (this)
			(this.assertNotUndefined arguments)
			(this.assertEqual arguments.length 3))
		 this "arg 2" "arg 3"))
	:testEmptyLambda (lambda ()
		(this.assertType (lambda) "function"))
	:testBasicArguments (lambda ()
		(let ((func   (lambda (one two three four) (list one two three four)))
			  (values (func 1 2 3 4)))
			(this.assertEqual (nth values 0) 1)
			(this.assertEqual (nth values 1) 2)
			(this.assertEqual (nth values 2) 3)
			(this.assertEqual (nth values 3) 4)))
	:testNoRestArgumentSymbol (lambda ()
		(this.assertRaises Error (lambda () (lambda (&))))
		(this.assertRaises Error (lambda () (lambda (one two &)))))
	:testArgumentsAfterRestArgumentSymbol (lambda ()
		(this.assertRaises Error (lambda () (lambda (& rest one))))
		(this.assertRaises Error (lambda () (lambda (one two & rest one)))))
	:testRestArgumentWithValues (lambda ()
		(let ((func   (lambda (one two & rest) (list one two rest)))
			  (values (func 1 2 3 4 5)))
			(this.assertEqual (nth values 2) '(3 4 5))))
	:testClosures (lambda ()
		(let ((func nil))
			(let ((x 0))
				(setq func (lambda () (setq x (1+ x)))))
			(this.assertUndefined x)
			(this.assertEqual (func) 1)
			(this.assertEqual (func) 2)
			(this.assertEqual (func) 3)
			(this.assertUndefined x)))))

;; TODO: Test (defun)
;;   * Test defuns as closures

(JSTest.TestCase (object
    :name "macro (try)"
	:testEmptyExpression (lambda ()
        (this.assertNotRaises Error #'try nil))
	:testOneExpression (lambda ()
        (let ((x 0))
		  (try
		      (setq x 10))
		  (this.assertEqual x 10)))
	:testManyExpressions (lambda ()
        (let ((x 0)
			  (y 0))
		  (try
		      (setq x 10)
			  (setq y 20))
		  (this.assertEqual x 10)
		  (this.assertEqual y 20)))
	:testEmptyCatchBlock (lambda ()
        (let ((func (lambda ()
					  (try
					      (throw (new Error))
						(catch)))))
		  (this.assertNotRaises Error func nil)))
	:testCompleteCatchBlock (lambda ()
        (let ((x nil)
			  (func (lambda ()
					  (try
					      (throw (new Error))
						(catch (e)
						  (setq x e))))))
		  (this.assertNotRaises Error func nil)
		  (this.assertInstanceOf x Error)))
	:testEmptyFinallyBlock (lambda ()
        (let ((func (lambda ()
					  (try
						(finally
						  (throw (new Error)))))))
		  (this.assertRaises Error func nil)))
	:testCompleteFinallyBlock (lambda ()
        (let ((x 0))
		  (try
		    (finally
			  (setq x 10)))
		  (this.assertEqual x 10)))
	:testTryCatchFinally (lambda ()
        (let ((x 0)
			  (y 0))
		  (try
		      (throw (new Error))
			(catch ()
			  (setq x 10))
		    (finally
			  (setq y 20)))
		  (this.assertEqual x 10)
		  (this.assertEqual y 20)))
	:testReturnValueWithNoException (lambda ()
        (let ((ret (try
					   (format nil "%0.2f" 10.254))))
		  (this.assertEqual ret "10.25")))
	:testReturnValueWithCaughtException (lambda ()
        (let ((ret (try
					   (format nil "%0.2f" 10.254)
					   (throw (new Error))
					 (catch))))
		  (this.assertEqual ret "10.25")))
	:testCatchFinallySymbolsAsKeywords (lambda ()
        (let ((x 0)
			  (y 0))
		  (try
		      (throw (new Error))
			(:catch ()
			  (setq x 10))
		    (:finally
			  (setq y 20)))
		  (this.assertEqual x 10)
		  (this.assertEqual y 20)))))

;; TODO: Test (funcall)

(JSTest.TestCase (object
	:name "macro (let)"
	:testScoping (lambda ()
		(this.assertUndefined somevar)
		(let ((somevar nil))
			(this.assertNotUndefined somevar))
		(this.assertUndefined somevar))
	:testMultipleLevels (lambda ()
		(this.assertUndefined somevar)
		(let ((somevar "first value"))
			(this.assertEqual somevar "first value")
			(let ((somevar "second value"))
				(this.assertEqual somevar "second value"))
			(this.assertEqual somevar "first value"))
		(this.assertUndefined somevar))
	:testClosures (lambda ()
		(let ((test))
	    	(let ((x 3))
				(setq test (lambda () (setq x (1+ x)))))
	        (this.assertEqual (test) 4)	
	        (this.assertEqual (test) 5)	
	        (this.assertEqual (test) 6)))))

;; TODO: There are probably more things to test here
(JSTest.TestCase (object
    :name "macro (setq)"
    :testScoping (lambda ()
        (setq x 1)
        (this.assertEqual x 1)
        (let ((x 2))
            (this.assertEqual x 2)
            (setq x 3)
            (this.assertEqual x 3))
        (this.assertEqual x 1))
    :testReturnValue (lambda ()
        (this.assertEqual (setq somevar "hello") "hello"))))

(JSTest.TestCase (object
    :name "macro (progn)"
	:testNoExpressions (lambda ()
        (this.assertNotRaises Error #'progn nil))
	:testOneExpression (lambda ()
        (this.assertNotRaises Error #'progn nil (list format nil "hello")))
	:testManyExpressions (lambda ()
        (let ((x 0)
			  (y 0)
			  (z 0))
		  (progn
			(setq x 10)
			(setq y 20)
			(setq z 30))
		  (this.assertEqual x 10)
		  (this.assertEqual y 20)
		  (this.assertEqual z 30)))
	:testReturnValue (lambda ()
        (let ((return-value (progn
							  (format nil "hello")
							  (format nil "goodbye"))))
		  (this.assertEqual "goodbye" return-value)))
	:testShortCircuiting (lambda ()
        (let ((x 0)
			  (y 0))
		  (try
		      (setq x 10)
			  (throw (new Error))
			  (setq y 20)
			(catch)) ;; Just silence the error
		  (this.assertEqual x 10)
		  (this.assertEqual y 0)))))

;; TODO: Test (cond)

(JSTest.TestCase (object
    :name "macro (if)"
	:testNoArguments (lambda ()
        (this.assertRaises Error #'if nil))
	:testOneArgument (lambda ()
        (this.assertRaises Error #'if nil t))
	:testManyArguments (lambda ()
        (this.assertNotRaises Error #'if nil t t))
	:testTrueTestExpression (lambda ()
        (let ((x 0))
		  (if t
			  (setq x 5)
			(setq x 10))
		  (this.assertEqual x 5)))
	:testFalseTestExpression (lambda ()
        (let ((x 0))
		  (if nil
			  (setq x 5)
			(setq x 10))
		  (this.assertEqual x 10)))
	:testNonEvaluationOfExpressions (lambda ()
        (let ((x 0)
			  (y 0))
		  ;; Only sets x
		  (if t
			  (setq x 10)
			(setq y 10))
		  (this.assertEqual x 10)
		  (this.assertEqual y 0)
		  ;; Only sets y
		  (if nil
			  (setq x 20)
			(setq y 20))
		  (this.assertEqual x 10)
		  (this.assertEqual y 20)))
	:testOnlyTruthExpression (lambda ()
		(this.assertEqual (if t "one") "one")
		(this.assertEqual (if nil "one") nil))
	:testReturnValues (lambda ()
        (this.assertEqual (if t "one" "two") "one")
		(this.assertEqual (if nil "one" "two") "two"))))

(JSTest.TestCase (object
	:name "macro (when)"
	:testNoArguments (lambda ()
		(this.assertRaises Error #'when null))
	:testOneArgument (lambda ()
		(this.assertNotRaises Error #'when null t)
		(this.assertNotRaises Error #'when null nil))
	:testWhenTrue (lambda ()
		(let ((x 5))
			(when t
				(setq x 10))
			(this.assertEqual x 10)))
	:testWhenFalse (lambda ()
		(let ((x 5))
			(when nil
				(setq x 10))
			(this.assertEqual x 5)))
	:testManyArguments (lambda ()
		(let ((x 5))
			(when t
				(setq x 10)
				(setq x 20))
			(this.assertEqual x 20)))
	:testReturnValues (lambda ()
		(let ((x 5))
			(this.assertEqual (when t (setq x 20)) 20)))))

(JSTest.TestCase (object
	:name "macro (not)"
	:testNoArguments (lambda ()
		;; The nil is for 'custom message' in JSTest, so it doesn't get
		;; applied to (not). This is testing that (not) will throw an error
		;; when it isn't given any arguments.
		(this.assertRaises Error #'not nil))
	:testOneArgument (lambda ()
		(this.assertTrue (not nil))
		(this.assertFalse (not true))
        (this.assertFalse (not "string"))
        (this.assertFalse (not :keyword))
        (this.assertFalse (not (not nil)))
        (this.assertFalse (not (not false))))
	:testManyArguments (lambda ()
		(this.assertTrue (not nil false null))
		(this.assertFalse (not nil false null t))) ;; The t at the end makes it false
	:testShortCircuiting (lambda ()
        (let ((x 5))
            (not nil false t (setq x 10)) ;; The t makes it cut short
            (this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (or)"
	:testNoArguments (lambda ()
		(this.assertFalse (or)))
	:testOneArgument (lambda ()
        (this.assertFalse (or nil))
        (this.assertTrue (or t)))
	:testManyArguments (lambda ()
        (this.assertTrue (or nil false t))
        (this.assertFalse (or nil false null)))
	:testShortCircuiting (lambda ()
        (let ((x 5))
            (or nil false t (setq x 10)) ;; The t makes it cut short
            (this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (and)"
	:testNoArguments (lambda ()
		(this.assertTrue (and)))
	:testOneArgument (lambda ()
        (this.assertFalse (and nil))
        (this.assertTrue (and t)))
	:testManyArguments (lambda ()
        (this.assertTrue (and t "hi" :hello))
        (this.assertFalse (and t :keyword nil)))
	:testShortCircuiting (lambda ()
        (let ((x 5))
            (and t :keyword nil (setq x 10)) ;; The nil makes it cut short
            (this.assertEqual x 5)))))

;; TODO: Test (equal)
;; TODO: Test (not-equal)

(JSTest.TestCase (object
	:name "macro (==)"
	:testOneArgument (lambda ()
		(this.assertRaises Error #'== nil 2))
	:testTwoArguments (lambda ()
		(this.assertTrue (== 2 2))
		(this.assertFalse (== 2 3)))
	:testManyArguments (lambda ()
		(this.assertTrue (== 2 2.0 (/ 4 2))))
	:testTypeConversion (lambda ()
		(this.assertTrue (== 2 "2"))
		(this.assertTrue (== "2" 2.0)))
	:testShortCircuiting (lambda ()
		(let ((x 5))
			(this.assertFalse (== 2 3 (setq x 10)))
			(this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (===)"
	:testOneArgument (lambda ()
		(this.assertRaises Error #'=== nil 2))
	:testTwoArguments (lambda ()
		(this.assertTrue (=== 2 2))
		(this.assertFalse (=== 2 3))
		(this.assertTrue (=== "a
string" "a\nstring")))
	:testManyArguments (lambda ()
		(this.assertTrue (=== 2 2.0 (/ 4 2))))
	:testNoTypeConversion (lambda ()
		(this.assertFalse (=== 2 "2"))
		(this.assertFalse (=== 2 "2" 2.0)))
	:testShortCircuiting (lambda ()
		(let ((x 5))
			(this.assertFalse (=== 2 "2" (setq x 10)))
			(this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (!=)"
	:testOneArgument (lambda ()
		(this.assertRaises Error #'!= nil 2))
	:testTwoArguments (lambda ()
		(this.assertTrue (!= 2 3))
		(this.assertFalse (!= 2 "2")))
	:testManyArguments (lambda ()
		(this.assertTrue (!= 2 3 4)))
	:testTypeConversion (lambda ()
		(this.assertFalse (!= 2 "2")))
	:testShortCircuiting (lambda ()
		(let ((x 5))
			(this.assertFalse (!= 2 "2" (setq x 10)))
			(this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (!==)"
	:testOneArgument (lambda ()
		(this.assertRaises Error #'!== nil 2))
	:testTwoArguments (lambda ()
		(this.assertTrue (!== 2 3))
		(this.assertTrue (!== 2 "2")))
	:testManyArguments (lambda ()
		(this.assertTrue (!== 2 3 4)))
	:testNoTypeConversion (lambda ()
		(this.assertTrue (!== 2 "2")))
	:testShortCircuiting (lambda ()
		(let ((x 5))
			(this.assertFalse (!== 2 2 (setq x 10)))
			(this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (<)"
	:testOneArgument (lambda ()
		(this.assertRaises Error #'< nil 2))
	:testTwoArguments (lambda ()
		(this.assertTrue (< 2 3))
		(this.assertFalse (< 2 2)))
	:testManyArguments (lambda ()
		(this.assertTrue (< 2 3 (/ 10 1.5))))
	:testTypeConversion (lambda ()
		(this.assertTrue (< 2 "3"))
		(this.assertTrue (< "2" 3.0)))
	:testShortCircuiting (lambda ()
		(let ((x 5))
			(this.assertFalse (< 2 2 (setq x 10)))
			(this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (>)"
	:testOneArgument (lambda ()
		(this.assertRaises Error #'> nil 2))
	:testTwoArguments (lambda ()
		(this.assertTrue (> 3 2))
		(this.assertTrue (> 1 -1))
		(this.assertFalse (> 2 2)))
	:testManyArguments (lambda ()
		(this.assertTrue (> (/ 10 1.5) 3 2)))
	:testTypeConversion (lambda ()
		(this.assertTrue (> 3 "2"))
		(this.assertTrue (> "3.0" 2)))
	:testShortCircuiting (lambda ()
		(let ((x 5))
			(this.assertFalse (> 2 2 (setq x 10)))
			(this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (<=)"
	:testOneArgument (lambda ()
		(this.assertRaises Error #'<= nil 2))
	:testTwoArguments (lambda ()
		(this.assertTrue (<= 2 3))
		(this.assertTrue (<= 2 2)))
	:testManyArguments (lambda ()
		(this.assertTrue (<= 2 (/ 4 2) 3 (/ 9 3))))
	:testTypeConversion (lambda ()
		(this.assertTrue (<= 2 "3"))
		(this.assertTrue (<= 2 "2"))
		(this.assertTrue (<= "2" 3.0))
		(this.assertTrue (<= "2" 2)))
	:testShortCircuiting (lambda ()
		(let ((x 5))
			(this.assertFalse (<= 2 1 (setq x 10)))
			(this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (>=)"
	:testOneArgument (lambda ()
		(this.assertRaises Error #'>= nil 2))
	:testTwoArguments (lambda ()
		(this.assertTrue (>= 3 2))
		(this.assertTrue (>= 1 -1))
		(this.assertTrue (>= 2 2)))
	:testManyArguments (lambda ()
		(this.assertTrue (>= (/ 9 3) 3 2)))
	:testTypeConversion (lambda ()
		(this.assertTrue (>= 3 "2"))
		(this.assertTrue (>= 3 "3"))
		(this.assertTrue (>= "3.0" 3)))
	:testShortCircuiting (lambda ()
		(let ((x 5))
			(this.assertFalse (>= 2 3 (setq x 10)))
			(this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (is-true)"
	:testBasic (lambda ()
		(this.assertTrue (is-true t))
		(this.assertTrue (is-true true))
		(this.assertTrue (is-true t true)))
	:testShortCircuiting (lambda ()
		(let ((x 5))
			(this.assertFalse (is-true t nil (setq x 10)))
			(this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (is-false)"
	:testBasic (lambda ()
		(this.assertTrue (is-false false))
		(this.assertTrue (is-false false false)))
	:testShortCircuiting (lambda ()
		(let ((x 5))
			(this.assertFalse (is-false false nil (setq x 10)))
			(this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (is-null)"
	:testBasic (lambda ()
		(this.assertTrue (is-null nil))
		(this.assertTrue (is-null null))
		(this.assertTrue (is-null nil null)))
	:testShortCircuiting (lambda ()
		(let ((x 5))
			(this.assertFalse (is-null nil t (setq x 10)))
			(this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (is-undefined)"
	:testBasic (lambda ()
		(this.assertTrue (is-undefined undefined))
		(this.assertTrue (is-undefined undefined undefined)))
	:testShortCircuiting (lambda ()
		(let ((x 5))
			(this.assertFalse (is-undefined undefined nil (setq x 10)))
			(this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (is-string)"
	:testBasic (lambda ()
		(this.assertTrue (is-string "hello"))
		(this.assertTrue (is-string "these" "are" "strings"))
		(this.assertFalse (is-string nil))
		(this.assertFalse (is-string t))
		(this.assertFalse (is-string false))
		(this.assertFalse (is-string undefined)))
	:testShortCircuiting (lambda ()
		(let ((x 5))
			(this.assertFalse (is-string "hello" 2 (setq x 10)))
			(this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (is-number)"
	:testBasic (lambda ()
		(this.assertTrue (is-number 345))
		(this.assertTrue (is-number 34.5))
		(this.assertTrue (is-number 3.45e2))
		(this.assertTrue (is-number 0377))
		(this.assertTrue (is-number 0xFF))
		(this.assertTrue (is-number 1 2 3 4)))
	:testShortCircuiting (lambda ()
		(let ((x 5))
			(this.assertFalse (is-number 2 "hello" (setq x 10)))
			(this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (is-boolean)"
	:testBasic (lambda ()
		(this.assertTrue (is-boolean false))
		(this.assertTrue (is-boolean true))
		(this.assertTrue (is-boolean false true)))
	:testShortCircuiting (lambda ()
		(let ((x 5))
			(this.assertFalse (is-boolean t nil (setq x 10)))
			(this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (is-function)"
	:testBasic (lambda ()
		(this.assertTrue (is-function this.assertTrue))
		(this.assertTrue (is-function /))
		(this.assertTrue (is-function (lambda ()))))
	:testShortCircuiting (lambda ()
		(let ((x 5))
			(this.assertFalse (is-function (lambda ()) nil (setq x 10)))
			(this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (is-object)"
	:testBasic (lambda ()
		(this.assertTrue (is-object window))
		(this.assertTrue (is-object this))
		(this.assertTrue (is-object (object))))
	:testShortCircuiting (lambda ()
		(let ((x 5))
			(this.assertFalse (is-object (object) "hi" (setq x 10)))
			(this.assertEqual x 5)))))

(JSTest.TestCase (object
	:name "macro (dolist)"
	;; TODO: Test error conditions
	;; TODO: Test explicit return value
	:testBasic (lambda ()
		(let ((x 0)
			  (y 0))
			(dolist (x '(1 2 3 4 5))
				(setq y (1+ y))
				(this.assertTrue (is-number x))
				(this.assertTrue (and (> x 0) (< x 6))))
			(this.assertEqual x 0)	
			(this.assertEqual y 5)))))

;; TODO: Test (foreach)

(JSTest.TestCase (object
	:name "macro (collect)"
	:testNoArguments (lambda ()
		(this.assertRaises Error #'collect nil))
	:testOneArgument (lambda ()
		(this.assertNotRaises Error #'collect nil '(name '(1 2 3))))
	:testBadFirstArgument (lambda ()
		(this.assertRaises Error #'collect nil '('(name) '(1 2 3))))
	:testBadSecondArgument (lambda ()
		(this.assertRaises Error #'collect nil '(name "bad arg")))
	:testBasic (lambda ()
		(let ((lt3 (collect (item (list 1 2 3))
					 (< (second item) 3))))
			(this.assertEqual (map second lt3) '(1 2))))))

;; ================================================================================
;; FUNCTIONS
;; ================================================================================

(JSTest.Divider "functions")

(JSTest.TestCase (object
	:name "function (jseval)"
	:testNoArguments (lambda ()
		(this.assertNotRaises Error #'jseval nil)
		(this.assertUndefined (jseval)))
	:testEvalJSON (lambda ()
		(let ((obj (jseval "({'key1': 1, 'key2': 2})")))
			(this.assertType obj "object")
			(this.assertEqual obj.key1 1)
			(this.assertEqual obj.key2 2)))
	:testEvalNewFunction (lambda ()
		(let ((func (jseval "new Function('x', 'return x + 1')")))
			(this.assertType func "function")
			(this.assertEqual (func 2) 3)))))

(JSTest.TestCase (object
	:name "function (assert)"
	:testNoArguments (lambda ()
		(this.assertRaises Error #'assert nil))
	:testOneArgument (lambda ()
		(this.assertNotRaises Error #'assert nil t))
	:testTwoArguments (lambda ()
		(this.assertNotRaises Error #'assert nil t "hi"))
	:testManyArguments (lambda ()
		(this.assertRaises Error #'assert nil t "hi" 'hi))
	:testTrueAssertions (lambda ()
		(this.assertNotRaises Error #'assert nil t)
		(this.assertNotRaises Error #'assert nil "true")
		(this.assertNotRaises Error #'assert nil 10)
		(this.assertNotRaises Error #'assert nil '())
		(this.assertNotRaises Error #'assert nil (object)))
	:testFalseAssertions (lambda ()
		(this.assertRaises Error #'assert nil false)
		(this.assertRaises Error #'assert nil nil)
		(this.assertRaises Error #'assert nil undefined)
		(this.assertRaises Error #'assert nil "")
		(this.assertRaises Error #'assert nil 0))))

(JSTest.TestCase (object
	:name "function (gensym)"
	:testNoArguments (lambda ()
		(this.assertNotRaises Error #'gensym nil))
	:testOneArgument (lambda ()
		(this.assertRaises Error #'gensym nil "arg 1"))
	:testManyArguments (lambda ()
		(this.assertRaises Error #'gensym nil "arg 1" "arg 2"))
	:testReturnType (lambda ()
		(this.assertInstanceOf (gensym) lisp.Symbol))
	:testLength (lambda ()
		(this.assertEqual (length (to-string (gensym))) 36))))

(JSTest.TestCase (object
    :name "function (new)"
	:testNoArguments (lambda ()
        (this.assertRaises Error #'new nil))
	:testOneArgument (lambda ()
        (this.assertNotRaises Error #'new nil Object))
	:testNonFunctionFirstArgument (lambda ()
        (this.assertRaises Error #'new nil "hello"))
	:testNewness (lambda ()
        (this.assertInstanceOf (new Date) Date))
	:testConstructorArguments (lambda ()
        (this.assertEqual (funcall (new Date 1234567890) getTime) 1234567890)
		(let ((SomeClass (lambda (arg1 arg2)
						   (setq this.arg1 arg1)
						   (setq this.arg2 arg2)))
			  (instance  (new SomeClass "hello" "goodbye")))
		  (this.assertEqual instance.arg1 "hello")
		  (this.assertEqual instance.arg2 "goodbye")))))

(JSTest.TestCase (object
	:name "function (instanceof)"
	:testNoArguments (lambda ()
		(this.assertRaises Error #'instanceof nil))
	:testOneArgument (lambda ()
		(this.assertRaises Error #'instanceof nil "value"))
	:testTwoArguments (lambda ()
		(this.assertNotRaises Error #'instanceof nil "value" Function))
	:testBadClassArgument (lambda ()
		(this.assertRaises Error #'instanceof nil "value" "not a class"))
	:testBasic (lambda ()
		(this.assertTrue (instanceof (new Error) Error))
		(this.assertFalse (instanceof "not an error" Error)))))

(JSTest.TestCase (object
    :name "function (throw)"
	:testDefaultValue (lambda ()
        (let ((self this))
		  (try
		      (throw)
			(catch (e)
			  (self.assertInstanceOf e Error)))))
	:testSuppliedValue (lambda ()
        (let ((self this))
		  (try
		      (throw (new Error "hello"))
			(catch (e)
			  (self.assertEqual (to-string e) "Error: hello")))))
	:testManyArguments (lambda ()
        (let ((self this))
		  (try
		      (throw (new Error) "arg 2")
			(catch (e)
			  (self.assertNotEqual (to-string e) "Error")))))
	:testNonErrorClassValue (lambda ()
        (let ((self this))
		  (try
		      (throw 12)
			(catch (e)
			  (self.assertTrue (=== e 12))))))))

(JSTest.TestCase (object
	:name "function (array)"
	:testNoArguments (lambda ()
		(this.assertNotRaises Error #'array nil))
	:testEmptyArray (lambda ()
		(this.assertEqual (array) (jseval "[]")))
	:testOneValue (lambda ()
		(this.assertEqual (array 'hello) '(hello) nil #'equal))
	:testManyValues (lambda ()
		(this.assertEqual (array 1 2 3) '(1 2 3) nil #'equal))))

(JSTest.TestCase (object
	:name "function (list)"
	:testNoArguments (lambda ()
		(this.assertNotRaises Error #'list nil))
	:testEmptyArray (lambda ()
		(this.assertEqual (list) (jseval "[]")))
	:testOneValue (lambda ()
		(this.assertEqual (list 'hello) '(hello) nil #'equal))
	:testManyValues (lambda ()
		(this.assertEqual (list 1 2 3) '(1 2 3) nil #'equal))))

(JSTest.TestCase (object
    :name "function (object)"
    :testGetValue (lambda ()
        (let ((o (object :key "value")))
            (this.assertEqual (getkey o :key) "value")))
    :testSetValue (lambda ()
        (let ((o (object)))
            (this.assertUndefined (getkey o :key))
            (setkey o :key "value")
            (this.assertNotUndefined (getkey o :key))
            (this.assertEqual (getkey o :key) "value")))
    :testKeyTypes (lambda ()
        (let ((cls  Object)
              (obj  (object))
              (func (lambda ()))
              (o (object
                    :keyword   1
                    "string"   2
                    t          3
                    false      4
                    nil        5
                    undefined  6
                    cls        7
                    obj        8
                    func       9)))
            (this.assertEqual (getkey o :keyword ) 1)        
            (this.assertEqual (getkey o "string" ) 2)
            (this.assertEqual (getkey o t        ) 3)
            (this.assertEqual (getkey o false    ) 4)
            (this.assertEqual (getkey o nil      ) 5)
            (this.assertEqual (getkey o undefined) 6)
            (this.assertEqual (getkey o cls      ) 7)
            (this.assertEqual (getkey o obj      ) 8)
            (this.assertEqual (getkey o func     ) 9)))
    :testNew (lambda ()
        (let ((d (new Date)))
            (this.assertNotUndefined d)
            (this.assertType (d.getTime) "number")))
    :testNewWithArgs (lambda ()
        (let ((d (new Date 1234567890000)))
            (this.assertNotUndefined d)
            (this.assertEqual (d.getTime) 1234567890000)))))

(JSTest.TestCase (object
	:name "function (function)"
	:testNoArguments (lambda ()
		(this.assertRaises Error #'function nil))
	:testOneArgument (lambda ()
		(this.assertNotRaises Error #'function nil 'function))
	:testManyArguments (lambda ()
		(this.assertRaises Error #'function nil 'function 'function))
	:testFunctionCall (lambda ()
		(this.assertEqual (function 'function) function)
		(this.assertNotEqual (function 'function) 'function))
	:testSpecialOperator (lambda ()
		(this.assertEqual #'function function)
		(this.assertNotEqual #'function 'function))
	:testMacros (lambda ()
		(this.assertEqual #'cond cond.callable)
		(this.assertNotEqual #'cond cond))))

(JSTest.TestCase (object
	:name "function (getkey)"
	:testNoArguments (lambda ()
		(this.assertRaises Error #'getkey nil))
	:testOneArgument (lambda ()
		(this.assertRaises Error #'getkey nil (object)))
	:testTwoArguments (lambda ()
		(this.assertNotRaises Error #'getkey nil (object) :key))
	:testManyArguments (lambda ()
		(this.assertRaises Error #'getkey nil (object) :key "arg 3"))
	:testObjectHasKey (lambda ()
		(let ((obj (object :key1 1 :key2 2)))
			(this.assertEqual (getkey obj :key1) 1)
			(this.assertEqual (getkey obj :key2) 2)))
	:testObjectDoesNotHaveKey (lambda ()
		(let ((obj (object)))
			(this.assertUndefined (getkey obj :key1))))))

(JSTest.TestCase (object
	:name "function (setkey)"
	:testNoArguments (lambda ()
		(this.assertRaises Error #'setkey nil))
	:testOneArgument (lambda ()
		(this.assertRaises Error #'setkey nil (object)))
	:testTwoArguments (lambda ()
		(this.assertRaises Error #'setkey nil (object) :key))
	:testThreeArguments (lambda ()
		(this.assertNotRaises Error #'setkey nil (object) :key "value"))
	:testManyArguments (lambda ()
		(this.assertRaises Error #'setkey nil (object) :key "value" "arg 4"))
	:testNewKey (lambda ()
		(let ((obj (object)))
			(this.assertUndefined (getkey obj :key1))
			(setkey obj :key1 "value")
			(this.assertNotUndefined (getkey obj :key1))
			(this.assertEqual (getkey obj :key1) "value")))
	:testReplaceKey (lambda ()
		(let ((obj (object :key1 1)))
			(this.assertEqual (getkey obj :key1) 1)
			(setkey obj :key1 2)
			(this.assertEqual (getkey obj :key1) 2)))))

(JSTest.TestCase (object
    :name "function (print)"
	:testNoArguments (lambda ()
        (this.assertNotRaises Error #'print nil))
	:testOneArgument (lambda ()
        (this.assertNotRaises Error #'print nil "(print) test"))
	:testManyArguments (lambda ()
        (this.assertNotRaises Error #'print nil "(print) test" "arg 2"))
	:testReturnValue (lambda ()
        (this.assertEqual (print) nil))))

(JSTest.TestCase (object
    :name "function (concat)"
	:testNoArguments (lambda ()
        (this.assertNotRaises Error #'concat nil)
		(this.assertEqual (concat) ""))
	:testOneArgument (lambda ()
        (this.assertNotRaises Error #'concat nil "arg 1")
		(this.assertEqual (concat "hello") "hello"))
	:testManyArguments (lambda ()
        (this.assertNotRaises Error #'concat nil "arg 1" "arg 2")
		(this.assertEqual (concat "one, " "two") "one, two"))))

(JSTest.TestCase (object
    :name "function (join)"
	:testNoArguments (lambda ()
        (this.assertRaises Error #'join nil))
	:testOneArgument (lambda ()
        (this.assertRaises Error #'join nil))
	:testTwoArguments (lambda ()
		(this.assertNotRaises Error #'join nil "sep" (list "the" "list")))
	:testManyArguments (lambda ()
		(this.assertNotRaises Error #'join nil "sep" (list 1) (list 2)))
	:testNonListArguments (lambda ()
        (this.assertRaises Error #'join nil "sep" "hello")
		(this.assertRaises Error #'join nil "sep" (list 1) 2))
	:testNumbers (lambda ()
		(this.assertEqual (join ", " (list 1 2)) "1, 2")
		(this.assertEqual (join ", " (list 1) (list 2)) "1, 2"))
	:testStrings (lambda ()
		(this.assertEqual (join ", " (list "one" "two")) "one, two")
		(this.assertEqual (join ", " (list "one") (list "two")) "one, two"))))

(JSTest.TestCase (object
    :name "function (typeof)"
	:testNoArguments (lambda ()
        (this.assertRaises Error #'typeof nil))
	:testManyArguments (lambda ()
        (this.assertRaises Error #'typeof nil "arg 1" 2 "arg 3"))
	:testBoolean (lambda ()
        (this.assertEqual (typeof t) "boolean")
		(this.assertEqual (typeof false) "boolean"))
	:testNumber (lambda ()
        (this.assertEqual (typeof 0) "number"))
	:testString (lambda ()
        (this.assertEqual (typeof "a string") "string"))
	:testObject (lambda ()
        (this.assertEqual (typeof (object)) "object"))
	:testFunction (lambda ()
        (this.assertEqual (typeof (lambda ())) "function"))
	:testNull (lambda ()
        (this.assertEqual (typeof nil) "object"))
	:testUndefined (lambda ()
		(this.assertEqual (typeof undefined) "undefined"))))

(JSTest.TestCase (object
	:name "function (to-string)"
	:testNoArguments (lambda ()
	    (this.assertRaises Error #'to-string nil))
	:testOneArgument (lambda ()
		(this.assertNotRaises Error #'to-string nil 0))
	:testManyArguments (lambda ()
        (this.assertRaises Error #'to-string nil 1 2))
	:testBasic (lambda ()
		(this.assertTrue (=== "3" (to-string 3))))))

(JSTest.TestCase (object
	:name "function (to-number)"
	:testStringToNumber (lambda ()
		(this.assertTrue (=== 3 (to-number "3")))
		(this.assertTrue (isNaN (to-number "hello"))))
	:testBooleanToNumber (lambda ()
		(this.assertEqual 1 (to-number t))
		(this.assertEqual 0 (to-number false)))
	:testNullToNumber (lambda ()
		(this.assertEqual 0 (to-number nil))
		(this.assertEqual 0 (to-number null)))))

(JSTest.TestCase (object
	:name "function (to-boolean)"
	:testNoArguments (lambda ()
	    (this.assertRaises Error #'to-boolean nil))
	:testOneArgument (lambda ()
		(this.assertNotRaises Error #'to-boolean nil "arg 1"))
	:testManyArguments (lambda ()
        (this.assertRaises Error #'to-boolean nil "arg 1" "arg 2"))
	:testBasic (lambda ()
		(this.assertTrue (is-true (to-boolean "hi")))
		(this.assertTrue (is-true (to-boolean (object))))
		(this.assertTrue (is-false (to-boolean nil)))
		(this.assertTrue (is-false (to-boolean 0))))))

(JSTest.TestCase (object
	:name "function (to-json)"
	:testNoArguments (lambda ()
		(this.assertRaises Error #'to-json nil))
	:testOneArgument (lambda ()
		(this.assertNotRaises Error #'to-json nil (object)))
	:testTwoArguments (lambda ()
		(this.assertNotRaises Error #'to-json nil (object) nil))
	:testThreeArguments (lambda ()
		(this.assertNotRaises Error #'to-json nil (object) nil 1))
	:testManyArguments (lambda ()
		(this.assertRaises Error #'to-json nil (object) nil 1 "arg 4"))
	:testNumbers (lambda ()
		(this.assertEqual (to-json 10) "10")
		(this.assertEqual (to-json 1e-4) "1e-4")
		(this.assertEqual (to-json (/ 5 2)) "2.5"))
	:testStrings (lambda ()
		(this.assertEqual (to-json "a string") "\"a string\""))
	:testArrays (lambda ()
		(this.assertEqual (to-json '(1 2 3)) "[1, 2, 3]")
		(this.assertEqual (to-json '(1 2 (3 4) 5)) "[1, 2, [3, 4], 5]"))
	:testObjects (lambda ()
		(this.assertEqual (to-json (object)) "{}")
		(this.assertEqual (to-json (object :one 1 :two 2)) "{\"one\": 1, \"two\": 2}"))
	:testDates (lambda ()
		(this.assertEqual (to-json (new Date 1276242254313)) "\"2010-06-11 07:44:14\""))))

(JSTest.TestCase (object
	:name "function (lisp-string)"
	:testNoArguments (lambda ()
		(this.assertRaises Error #'lisp-string nil))
	:testOneArgument (lambda ()
		(this.assertNotRaises Error #'lisp-string nil "hello"))
	:testManyArguments (lambda ()
		(this.assertRaises Error #'lisp-string nil "arg 1" "arg 2"))
	:testNumbers (lambda ()
		(this.assertEqual (lisp-string 10) "10")
		(this.assertEqual (lisp-string 1e-4) "1e-4")
		(this.assertEqual (lisp-string (/ 5 2)) "2.5"))
	:testStrings (lambda ()
		(this.assertEqual (lisp-string "a string") "\"a string\""))
	:testArrays (lambda ()
		(this.assertEqual (lisp-string '(1 2 3)) "(1 2 3)")
		(this.assertEqual (lisp-string '(1 2 (3 4) 5)) "(1 2 (3 4) 5)"))
	:testObjects (lambda ()
		(this.assertEqual (lisp-string (object)) "{}")
		(this.assertEqual (lisp-string (object :one 1 :two 2)) "{
  \"one\": 1, 
  \"two\": 2
}"))
	:testFormatDirective (lambda ()
		(dolist (value '(one "two" (object) (list 1 2 3) 12))
			(this.assertEqual (lisp-string value) (format nil "%l" value) nil #'equal)))))

(JSTest.TestCase (object
    :name "function (to-upper)"
	:testNoArguments (lambda ()
	    (this.assertRaises Error #'to-upper nil))
	:testOneArgument (lambda ()
		(this.assertNotRaises Error #'to-upper nil "arg 1"))
	:testManyArguments (lambda ()
        (this.assertRaises Error #'to-upper nil "arg 1" "arg 2"))
	:testNonStringArgument (lambda ()
        (this.assertRaises Error #'to-upper nil 0))
    :testBasic (lambda ()
		(this.assertEqual (to-upper "hello") "HELLO"))))

(JSTest.TestCase (object
    :name "function (to-lower)"
	:testNoArguments (lambda ()
	    (this.assertRaises Error #'to-lower nil))
	:testOneArgument (lambda ()
		(this.assertNotRaises Error #'to-lower nil "arg 1"))
	:testManyArguments (lambda ()
        (this.assertRaises Error #'to-lower nil "arg 1" "arg 2"))
	:testNonStringArgument (lambda ()
        (this.assertRaises Error #'to-lower nil 0))
	:testBasic (lambda ()
		(this.assertEqual (to-lower "HELLO") "hello"))))

(JSTest.TestCase (object
    :name "function (/)"
	:testNoArguments (lambda ()
        (this.assertRaises Error #'/ nil))
	:testOneArgument (lambda ()
        (this.assertNotRaises Error #'/ nil 1))
	:testManyArguments (lambda ()
        (this.assertNotRaises Error #'/ nil 2 3))
	:testDividingOneNumber (lambda ()
	    (this.assertEqual (/ 2) 0.5))
	:testDividingTwoNumbers (lambda ()
        (this.assertEqual (/ 4 2) 2))
	:testDividingManyNumbers (lambda ()
        (this.assertEqual (/ 4 2 4) 0.5))))

(JSTest.TestCase (object
    :name "function (*)"
	:testNoArguments (lambda ()
        (this.assertNotRaises Error #'* nil))
	:testOneArgument (lambda ()
        (this.assertNotRaises Error #'* nil 1))
	:testManyArguments (lambda ()
        (this.assertNotRaises Error #'* nil 2 3))
	:testMultiplyingNoNumbers (lambda ()
        (this.assertEqual (*) 1))
	:testMultiplyingOneNumber (lambda ()
	    (this.assertEqual (* 2) 2))
	:testMultiplyingTwoNumbers (lambda ()
        (this.assertEqual (* 4 2) 8))
	:testMultiplyingManyNumbers (lambda ()
        (this.assertEqual (* 2 3 4) 24))))

(JSTest.TestCase (object
    :name "function (+)"
	:testNoArguments (lambda ()
        (this.assertNotRaises Error #'+ nil))
	:testOneArgument (lambda ()
        (this.assertNotRaises Error #'+ nil 1))
	:testManyArguments (lambda ()
        (this.assertNotRaises Error #'+ nil 2 3))
	:testAddingNoNumbers (lambda ()
        (this.assertEqual (+) 0))
	:testAddingOneNumber (lambda ()
	    (this.assertEqual (+ 2) 2))
	:testAddingTwoNumbers (lambda ()
        (this.assertEqual (+ 4 2) 6))
	:testAddingManyNumbers (lambda ()
        (this.assertEqual (+ 2 3 4) 9))))

(JSTest.TestCase (object
    :name "function (-)"
	:testNoArguments (lambda ()
        (this.assertRaises Error #'- nil))
	:testOneArgument (lambda ()
        (this.assertNotRaises Error #'- nil 1))
	:testManyArguments (lambda ()
        (this.assertNotRaises Error #'- nil 2 3))
	:testSubtractingOneNumber (lambda ()
	    (this.assertEqual (- 2) -2))
	:testSubtractingTwoNumbers (lambda ()
        (this.assertEqual (- 4 2) 2))
	:testSubtractingManyNumbers (lambda ()
        (this.assertEqual (- 2 3 4) -5))))

(JSTest.TestCase (object
    :name "function (%)"
	:testNoArguments (lambda ()
        (this.assertRaises Error #'% nil))
	:testOneArgument (lambda ()
        (this.assertRaises Error #'% nil 1))
	:testTwoArguments (lambda ()
        (this.assertNotRaises Error #'% nil 1 2))
	:testManyArguments (lambda ()
        (this.assertNotRaises Error #'% nil 1 2 3))
	:testBasic (lambda ()
	    (this.assertEqual (% 3 2) 1))))

(JSTest.TestCase (object
    :name "function (1+)"
	:testNoArguments (lambda ()
        (this.assertRaises Error #'1+ nil))
	:testOneArgument (lambda ()
        (this.assertNotRaises Error #'1+ nil 1))
	:testManyArguments (lambda ()
        (this.assertRaises Error #'1+ nil 1 2))
	:testNonNumberArgument (lambda ()
        (this.assertRaises Error #'1+ nil "hi"))
	:testBasic (lambda ()
	    (this.assertEqual (1+ 2) 3))))

(JSTest.TestCase (object
    :name "function (1-)"
	:testNoArguments (lambda ()
        (this.assertRaises Error #'1- nil))
	:testOneArgument (lambda ()
        (this.assertNotRaises Error #'1- nil 1))
	:testManyArguments (lambda ()
        (this.assertRaises Error #'1- nil 1 2))
	:testNonNumberArgument (lambda ()
        (this.assertRaises Error #'1- nil "hi"))
	:testBasic (lambda ()
	    (this.assertEqual (1- 2) 1))))

(JSTest.TestCase (object
	:name "function (format)"
	:testNoArguments (lambda ()
		(this.assertRaises Error #'format nil))
	:testOneArgument (lambda ()
		(this.assertRaises Error #'format nil t))
	:testManyArguments (lambda ()
		(this.assertNotRaises Error #'format nil nil "hello"))
	:testNumberFormatting (lambda ()
		(this.assertEqual (format nil "%01.2f" 123.1) "123.10")
		(this.assertEqual (format nil "%x" 15) "f")
		(this.assertEqual (format nil "%b" 255) "11111111"))
	:testStringFormatting (lambda ()
		(this.assertEqual (format nil "[%10s]" "string") "[    string]")
		(this.assertEqual (format nil "There are %d monkeys in the %s" 5 "tree")
									  "There are 5 monkeys in the tree")
		(this.assertEqual (format nil "The %2$s contains %1$d monkeys" 5 "tree")
									  "The tree contains 5 monkeys")
		(this.assertEqual (format nil "I like %s; %1$s are good." "apples")
									  "I like apples; apples are good.")
		(this.assertEqual (format nil "I like %1$s; %1$s are good." "apples")
									  "I like apples; apples are good."))))

(JSTest.TestCase (object
	:name "function (apply)"
	:testNoArguments (lambda ()
		(this.assertRaises Error #'apply nil))
	:testOneArgument (lambda ()
		(this.assertRaises Error #'apply nil +))
	:testTwoArguments (lambda ()
		(this.assertNotRaises Error #'apply nil + '(1 2)))
	:testMoreThanTwoArguments (lambda ()
		(this.assertRaises Error #'apply nil + '(1 2) '(3 4)))
	:testNonFunctionFirstArgument (lambda ()
		(this.assertRaises Error #'apply nil "hi" '(1 2)))
	:testNonListSecondArgument (lambda ()
		(this.assertRaises Error #'apply nil + 1))
	:testBasic (lambda ()
		(this.assertEqual (apply + '(1 2 3)) 6))
))

(JSTest.TestCase (object
	:name "function (map)"
	:testNoArguments (lambda ()
		(this.assertRaises Error #'map nil))
	:testOneArgument (lambda ()
		(this.assertRaises Error #'map nil #'(lambda)))
	:testTwoArguments (lambda ()
		(this.assertNotRaises Error #'map nil #'(lambda) '()))
	:testManyArguments (lambda ()
		(this.assertRaises Error #'map nil #'(lambda) '() "too many arguments"))
	:testBadFunctionArgument (lambda ()
		(this.assertRaises Error #'map nil "not a func" '()))
	:testBadListArgument (lambda ()
		(this.assertRaises Error #'map nil (lambda) "not a list"))
	:testBasic (lambda ()
		(this.assertEqual (map (lambda (x) (1+ x)) '(1 2 3))
						  '(2 3 4) nil #'equal))))

(let ((testobj (object :one 1 :two 2 :three 3 :four 4 :five (object :six 6 :seven 7 :eight 8)))
	  (v (lambda (o) (map #'second (items o)))))
	(JSTest.TestCase (object
		:name "function (props)"
		:testNoArguments (lambda ()
			(this.assertRaises Error #'props nil))
		:testOneProperty (lambda ()
			(this.assertEqual (v (props testobj 'one)) '(1)))
		:testOnePropertyInList (lambda ()
			(this.assertEqual (v (props testobj '(one))) '(1)))
		:testManyProperties (lambda ()
			(this.assertEqual (v (props testobj '(one two four))) '(1 2 4)))
		:testPropertyChains (lambda ()
			(let ((newobj (props testobj '(five.six five.eight))))
				(this.assertEqual (length (items newobj.five)) 2)
				(this.assertEqual newobj.five.six 6)
				(this.assertEqual newobj.five.eight 8))))))

(JSTest.TestCase (object
	:name "function (items)"
	:testNoArguments (lambda ()
		(this.assertRaises Error #'items nil))
	:testOneArgument (lambda ()
		(this.assertNotRaises Error #'items nil (object)))
	:testManyArguments (lambda ()
		(this.assertRaises Error #'items nil (object) "arg 2"))
	:testNonObjectArgument (lambda ()
		(this.assertRaises Error #'items nil 1))
	:testBasicObjects (lambda ()
		(let ((obj (object :one 2 :three 4 :five 6)))
			(this.assertEqual (items obj) '(("one" 2) ("three" 4) ("five" 6)) nil #'equal)))
	:testArrays (lambda ()
		(let ((obj '(one two three)))
			(this.assertEqual (items obj) '(("0" one) ("1" two) ("2" three)) nil #'equal)))))

(JSTest.TestCase (object
	:name "function (nth)"
	:testNoArguments (lambda ()
		(this.assertRaises Error #'nth nil))
	:testOneArgument (lambda ()
		(this.assertRaises Error #'nth nil (array)))
	:testTwoArguments (lambda ()
		(this.assertNotRaises Error #'nth nil (array 1) 0))
	:testManyArguments (lambda ()
		(this.assertRaises Error #'nth nil (array 1) 0 "arg 3"))
	:testNonArrayFirstArgument (lambda ()
		(this.assertRaises Error #'nth nil "arg 1" 0))
	:testNonNumberSecondArgument (lambda ()
		(this.assertRaises Error #'nth nil (array 1) "arg 2"))
	:testIndexArrayDoesntHave (lambda ()
		(this.assertNotRaises Error #'nth nil (array) 10)
		(this.assertEqual (nth (array) 10) nil))
	:testBasic (lambda ()
		(this.assertEqual (nth '(one two) 1) 'two nil #'equal))))

;; TODO: Test (first)
;; TODO: Test (second)
;; TODO: Test (third)
;; TODO: Test (push)
;; TODO: Test (sort!)
;; TODO: Test (sort)
;; TODO: Test (length)
