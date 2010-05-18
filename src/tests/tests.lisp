(JSTest.TestCase (object
	:name "this Object in Lambdas"
	:testThisWorks (lambda ()
		(this.message "It works!"))))

(JSTest.TestCase (object
	:name "Predicates"
	:testIsString (lambda ()
		(this.assertTrue (is-string "hello"))
		(this.assertTrue (is-string "these" "are" "strings"))
		(this.assertFalse (is-string nil))
		(this.assertFalse (is-string t))
		(this.assertFalse (is-string false))
		(this.assertFalse (is-string undefined)))
	:testIsNumber (lambda ()
		(this.assertTrue (is-number 345))
		(this.assertTrue (is-number 34.5))
		(this.assertTrue (is-number 3.45e2))
		(this.assertTrue (is-number 0377))
		(this.assertTrue (is-number 0xFF))
		(this.assertTrue (is-number 1 2 3 4)))
	:testIsTrue (lambda ()
		(this.assertTrue (is-true t))
		(this.assertTrue (is-true true))
		(this.assertTrue (is-true t true)))
	:testIsFalse (lambda ()
		(this.assertTrue (is-false false))
		(this.assertTrue (is-false false false)))
	:testIsNull (lambda ()
		(this.assertTrue (is-null nil))
		(this.assertTrue (is-null null))
		(this.assertTrue (is-null nil null)))
	:testIsUndefined (lambda ()
		(this.assertTrue (is-undefined undefined))
		(this.assertTrue (is-undefined undefined undefined)))))

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
    :name "Logical Operators"
    :testNot (lambda ()
        (this.assertFalse (not true))
        (this.assertFalse (not "string"))
        (this.assertFalse (not :keyword))
        (this.assertFalse (not (not nil)))
        (this.assertFalse (not (not false))))
    :testOr (lambda ()
        (this.assertTrue (or nil t))
        (this.assertTrue (not (or nil false))))
    :testOrShortCircuit (lambda ()
        (let ((x 5))
            (or true (setq x 10))
            (this.assertEqual x 5)))
    :testAnd (lambda ()
        (this.assertTrue (and t true "hi"))
        (this.assertTrue (not (and t true nil))))
    :testAndShortCircuit (lambda ()
        (let ((x 5))
            (and nil (setq x 10))
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
