(setq say-hi
	(lambda (element)
		(element.addChild (document.createTextNode "hi!"))))

(setq say-hi2
	(lambda (name)
		(puts "Hi " name)))

(say-hi2 "bob")

; (setq window.onload
; 	(lambda ()
; 		(say-hi test-results)))
