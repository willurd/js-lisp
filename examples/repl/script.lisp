(defun repl-represent (value)
	(cond ((is-null value) "nil")
		  ((is-true value) "t")
		  ((instanceof value lisp.Keyword) (concat ":" value))
		  (t (to-json value t))))

(setq keycodes (object
	:0 48  :1 49  :2 50  :3 51  :4 52  :5 53
	:6 54  :7 55  :8 56  :9 57  :a 65  :b 66
	:c 67  :d 68  :e 69  :f 70  :g 71  :h 72
	:i 73  :j 74  :k 75  :l 76  :m 77  :n 78
	:o 79  :p 80  :q 81  :r 82  :s 83  :t 84
	:u 85  :v 86  :w 87  :x 88  :y 89  :z 90))

(setq controller nil) ;; This is a hack because js-lisp doesn't have closures yet

($ (lambda ()
	(let ((repl-id "#console")
		  (repl    ($ repl-id)))
		(when (=== repl.length 0)
			(throw (new Error (format nil "Console with id %s does not exist" repl-id))))
		(setq controller (repl.console (object
				:welcomeMessage "js-lisp REPL"
				:ps1 ">> "
				:ps2 ".. "
				:ps3 "=> "
				:autofocus t ;; Automatically sets focus on the console when the page loads
				:animateScroll nil
				:promptHistory t ;; Maintains a history of input given at the prompt
				:historyPreserveColumn t ;; Preserves the column you were on for each history line
				:commandValidate (lambda (line)
					(!= line ""))
				:commandHandle (lambda (line)
					(let ((ret nil))
						(try
							(setq ret (array (object :msg (repl-represent (lisp.eval line))
													 :className "jquery-console-message-value")))
						  (:catch (e)
							(setq ret (when (not (instanceof e lisp.exception.StreamEOFException))
										(controller.message (to-string e) "jquery-console-message-error")
										t))))
						ret))))))
	
	(defun controller.consoleControl (event)
		(cond (event.ctrlKey
				(cond ((== event.keyCode keycodes.h) (help) (controller.commandResult))
					  ((== event.keyCode keycodes.k) (clear))))
			  (t (controller.defaultConsoleControl event))))
	
	(defun help ()
		(print "Commands:
    (help)  [ctrl-h]   - Display this help
    (clear) [ctrl-k]   - Clear the screen"))
	
	(defun clear ()
		(controller.reset))
	
	(defun refresh ()
		(setq window.location ""))
	
	(defun lisp.log (&rest rest)
		(try
			(controller.message (join " " (if rest rest (array))) "jquery-console-stdout")
		  (:catch (e)
			(controller.message (to-string e) "jquery-console-message-error"))))))
