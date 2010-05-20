(print "js-lisp Lambdas - Example 1")

(setq window.onload
	(lambda (e g)
		(print "Inside a lambda")
		(print "Printing an arg: " e)
		(print "And printing an undefined arg: " g)))
