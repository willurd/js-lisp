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
