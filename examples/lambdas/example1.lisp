(puts "js-lisp Lambdas - Example 1")

(setq window.onload
	(lambda (e g)
		(puts "Inside a lambda")
		(puts "Printing an arg: " e)
		(puts "And printing an undefined arg: " g)))
