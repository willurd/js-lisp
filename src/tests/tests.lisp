(JSTest.TestCase (object
	:name "Lisp Tests > this Object in Lambdas (written in lisp)"
	:testThisWorks (lambda ()
		(this.message "It works!"))))

(JSTest.TestCase (object
	:name "Lisp Tests > Predicates (written in lisp)"
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
	:name "Lisp Tests > Numbers (written in lisp)"
	:testOctals (lambda ()
		(this.assertEqual 0100 64))
	:testHex (lambda ()
		(this.assertEqual 0x40 64))))

(JSTest.TestCase (object
	:name "Lisp Tests > Strings (written in lisp)"
	:testHardNewline (lambda ()
		(this.assertEqual "a
string" "a\nstring"))
	:testHardTab (lambda ()
		(this.assertEqual "a	string" "a\tstring"))))
