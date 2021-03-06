(defun repl-represent (value)
	"Returns a lispy string representation of the given value"
	(cond ((is-null value) "nil")
		  ((is-true value) "t")
		  ((is-false value) "f")
		  ((instanceof value lisp.Symbol) (to-string value))
		  ((instanceof value lisp.Keyword) (concat ":" (to-string value)))
		  ((instanceof value Array)
			(concat "(" (join " " (map repl-represent value)) ")"))
		  (t (to-json value t))))

(setq keycodes (object
	:backspace 8  :delete 46
	:left 37  :up 38  :right 39  :down 40
	:0 48  :1 49  :2 50  :3 51  :4 52  :5 53
	:6 54  :7 55  :8 56  :9 57  :a 65  :b 66
	:c 67  :d 68  :e 69  :f 70  :g 71  :h 72
	:i 73  :j 74  :k 75  :l 76  :m 77  :n 78
	:o 79  :p 80  :q 81  :r 82  :s 83  :t 84
	:u 85  :v 86  :w 87  :x 88  :y 89  :z 90))

(defun help ()
	"Prints the help text"
	(print "Commands:
        [ctrl-h] - Display this help
        [ctrl-k] - Clear the screen
        [ctrl-a] - Move to start of line
        [ctrl-e] - Move to end of line
      [alt-left] - Move left one word
     [alt-right] - Move right one word
 [alt-backspace] - Delete one word to the left
    [alt-delete] - Delete one word to the right
"))

(defun clear ()
	(controller.reset))

(defun refresh ()
	(setq window.location ""))

(defun lisp.log (& rest)
	(try
		(controller.message (join " " (if rest rest (array))) "jquery-console-stdout")
	  (:catch (e)
		(controller.message (to-string e) "jquery-console-message-error"))))

(setq controller nil) ;; This is how we control the repl

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
			;:historyPreserveColumn t ;; Preserves the column you are on while scrolling through history
			:commandValidate (lambda (line)
				(!= line ""))
			:commandHandle (lambda (line)
				(let ((this controller)
					  (ret nil))
					(try
						(setq ret (array (object :msg (repl-represent (lisp.eval line))
												 :className "jquery-console-message-value")))
					  (:catch (e)
						(setq ret (when (not (instanceof e lisp.exception.StreamEOFException))
									(controller.message (to-string e) "jquery-console-message-error")
									t))))
					ret))))))
	
	(controller.notice "Hit Ctrl-h for help")
	
	(defun controller.consoleControl (event)
		;; TODO: Make these (cond) expressions into (case) expressions
		;; whenever (case) is written.
		(cond (event.ctrlKey
				(controller.cancelKey event.keyCode)
				(cond ((== event.keyCode keycodes.h) (help) (controller.commandResult))
					  ((== event.keyCode keycodes.k) (clear))
					  ((== event.keyCode keycodes.a) (controller.moveToStart))
					  ((== event.keyCode keycodes.e) (controller.moveToEnd))
					  (t (controller.defaultConsoleControl event))))
			  (event.altKey
				(controller.cancelKey event.keyCode)
				(cond ((== event.keyCode keycodes.left)      (controller.moveWordLeft))
					  ((== event.keyCode keycodes.right)     (controller.moveWordRight))
					  ((== event.keyCode keycodes.backspace) (controller.deleteWordLeft))
					  ((== event.keyCode keycodes.delete)    (controller.deleteWordRight))
					  (t (controller.defaultConsoleControl event))))
			  (event.metaKey
				(controller.cancelKey event.keyCode)
				(cond ((== event.keyCode keycodes.r) (setq window.location ""))
					  (t (controller.defaultConsoleControl event))))
			  (t (controller.defaultConsoleControl event))))))
