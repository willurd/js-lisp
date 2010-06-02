(defun repl-represent (value)
	;; TODO: Add a (cond) statement here
	; (cond
	; 	((is-instanceof value Keyword) (concat ":" (to-string value)))
	; 	(t (to-json value))))
	
	;; TODO: Or modify (to-json) to include representations for symbols and keywords
	(to-json value))

($ (lambda ()
	(let ((console-id "#console")
		  (console ($ console-id)))
		(when (=== console.length 0)
			(throw (new Error (format nil "Console with id %s does not exist" console-id))))
		(let ((controller (console.console (object
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
						  (catch (e)
							(setq ret (array (object :msg (to-json e)
													 :className "jquery-console-message-error")))))
						ret))))))
			;(controller.promptText ":name")
			))))
