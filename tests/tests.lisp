(JSTest.TestCase (object
	:name "this Object in Lambdas"
	:testThisWorks (lambda ()
		(this.message "It works!"))))

(JSTest.TestCase (object
	:name "Numbers"
	:testOctals (lambda ()
		(this.assertEqual 0100 64))
	:testHex (lambda ()
		(this.assertEqual 0x40 64))))

(JSTest.TestCase (object
	:name "Strings"
	:testHardNewline (lambda ()
		(this.assertEqual "a
string" "a\nstring"))
	:testHardTab (lambda ()
		(this.assertEqual "a	string" "a\tstring"))))

(JSTest.TestCase (object
	:name "Equality Operators"
	:testDoubleEquals (lambda ()
		(this.assertTrue (== 2 2))
		(this.assertTrue (== 2 "2"))
		(this.assertTrue (== 2 "2" 2.0))
		(this.assertFalse (== 2 3))
		(this.assertFalse (== 2 "2" "a string"))
		(let ((x 5))
			(this.assertFalse (== 2 3 (setq x 10)))
			(this.assertEqual x 5)))
	:testTripleEquals (lambda ()
		(this.assertTrue (=== 2 2))
		(this.assertFalse (=== 2 "2"))
		(this.assertTrue (=== 2 2.0 (/ 4 2))
		(this.assertTrue (=== "a
string" "a\nstring")))
		(let ((x 5))
			(this.assertFalse (=== 2 "2" (setq x 10)))
			(this.assertEqual x 5)))
	:testDoubleNotEquals (lambda ()
		(this.assertTrue (!= 2 3))
		(this.assertFalse (!= 2 "2"))
		(this.assertFalse (!= 2 (/ 4 "2")))
		(let ((x 5))
			(this.assertFalse (!= 2 "2" (setq x 10)))
			(this.assertEqual x 5)))
	:testTripleNotEquals (lambda ()
		(this.assertTrue (!== 2 "2"))
		(this.assertFalse (!== 10 10.0))
		(let ((x 5))
			(this.assertFalse (!== 2 2 (setq x 10)))
			(this.assertEqual x 5)))))

(JSTest.TestCase (object
    :name "Conditions"))

(JSTest.TestCase (object
	:name "Scope"
	:testLetScoping (lambda ()
		(this.assertUndefined somevar)
		(let ((somevar t))
			(this.assertNotUndefined somevar))
		(this.assertUndefined somevar))
	:testLetMultipleLevels (lambda ()
		(this.assertUndefined somevar)
		(let ((somevar "first value"))
			(this.assertEqual somevar "first value")
			(let ((somevar "second value"))
				(this.assertEqual somevar "second value"))
			(this.assertEqual somevar "first value"))
		(this.assertUndefined somevar))
	:testClosures (lambda ()
	    (let ((x 3)
	          (test (lambda () (setq x (1+ x)))))
	        (this.assertEqual x 3)
	        (test)
	        (test)
	        (this.assertEqual x 5))
	    (let ((x 3)
	          (test (lambda (x) (setq x (1+ x)))))
	        (this.assertEqual x 3)
	        (test x)
	        (test x)
	        (this.assertEqual x 3)))))

(JSTest.TestCase (object
    :name "Objects"
    :testGetValue (lambda ()
        (let ((o (object :key "value")))
            (this.assertEqual (getkey :key o) "value")))
    :testSetValue (lambda ()
        (let ((o (object)))
            (this.assertUndefined (getkey :key o))
            (setkey :key o "value")
            (this.assertNotUndefined (getkey :key o))
            (this.assertEqual (getkey :key o) "value")))
    :testKeyTypes (lambda ()
        (let ((Class Object)
              (obj   (object))
              (func  (lambda ()))
              (o (object
                    :keyword   1
                    "string"   2
                    t          3
                    false      4
                    nil        5
                    undefined  6
                    Class      7
                    obj        8
                    func       9)))
            (this.assertEqual (getkey :keyword  o) 1)        
            (this.assertEqual (getkey "string"  o) 2)
            (this.assertEqual (getkey t         o) 3)
            (this.assertEqual (getkey false     o) 4)
            (this.assertEqual (getkey nil       o) 5)
            (this.assertEqual (getkey undefined o) 6)
            (this.assertEqual (getkey Class     o) 7)
            (this.assertEqual (getkey obj       o) 8)
            (this.assertEqual (getkey func      o) 9)))
    :testNew (lambda ()
        (let ((d (new Date)))
            (this.assertNotUndefined d)
            (this.assertType (d.getTime) "number")))
    :testNewWithArgs (lambda ()
        (let ((d (new Date 1234567890000)))
            (this.assertNotUndefined d)
            (this.assertEqual (d.getTime) 1234567890000)))))

(JSTest.TestCase (object
	:name "Functions As Function Calls"
	:testFunctionCallAsFirstArg (lambda ()
		(let ((o (object :func (lambda (x) (1+ x)))))
			(this.assertEqual 2 ((getkey :func o) 1))))
	:testLambdaAsFirstArg (lambda ()
		(this.assertEqual 2 ((lambda (x) (1+ x)) 1)))))

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
	:name "macro (not)"
	:testNoArguments (lambda ()
		;; The nil is for 'custom message' in JSTest, so it doesn't get
		;; applied to (not). This is testing that (not) will throw an error
		;; when it isn't given any arguments.
		(this.assertRaises Error (getfunc not) nil))
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
    :name "function (not)"
    :testBasic (lambda ()
        (this.assertTrue (not nil))
        (this.assertTrue (not (not t)))
        (this.assertTrue (=== (not (not "hello")) true)))))

(JSTest.TestCase (object
	:name "function (to-string)"
	:testBasic (lambda ()
		(this.assertTrue (=== "3" (to-string 3))))))

(JSTest.TestCase (object
	:name "function (to-number)"
	:testBasic (lambda ()
		(this.assertTrue (=== 3 (to-number "3"))))))

(JSTest.TestCase (object
	:name "function (to-boolean)"
	:testToBoolean (lambda ()
		(this.assertTrue (is-true (to-boolean "hi")))
		(this.assertTrue (is-true (to-boolean (object))))
		(this.assertTrue (is-false (to-boolean nil)))
		(this.assertTrue (is-false (to-boolean 0))))))

(JSTest.TestCase (object
    :name "function (to-upper)"
    :testBasic (lambda ()
		(this.assertEqual (to-upper "hello") "HELLO"))))

(JSTest.TestCase (object
    :name "function (to-lower)"
	:testToLower (lambda ()
		(this.assertEqual (to-lower "HELLO") "hello"))))

(JSTest.TestCase (object
	:name "function (format)"
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
