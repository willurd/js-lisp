(defun repl-represent (value)
	(cond ((is-null value) "nil")
		  ((is-true value) "t")
		  ((instanceof value lisp.Keyword) (concat ":" value))
		  (t (to-json value))))

($ (lambda ()
	(let ((repl-id "#console")
		  (repl    ($ repl-id))
		  (ps1     ">> ")
		  (ps2     ".. "))
		(when (=== repl.length 0)
			(throw (new Error (format nil "Console with id %s does not exist" repl-id))))
		(let ((controller (repl.console (object
				:welcomeMessage "js-lisp REPL"
				:promptLabel ">> "
				:autofocus t ;; Automatically sets focus on the console when the page loads
				:animateScroll t
				:promptHistory t ;; Maintains a history of input given at the prompt
				:historyPreserveColumn t ;; Preserves the column you were on for each history line
				:commandValidate (lambda (line)
					(!= line ""))
				:commandHandle (lambda (line)
					(let ((ret nil))
						(try
							(setq ret (array (object :msg (repl-represent (lisp.eval line))
													 :className "jquery-console-message-value")))
							(setq controller.promptLabel ps1)
						  (:catch (e)
							(if (instanceof e lisp.exception.StreamEOFException)
								(progn
									(setq ret nil)
									(setq controller.promptLabel ps2))
							  (setq ret (array (object :msg (to-string e)
													   :className "jquery-console-message-error"))))))
						ret))))))))))
