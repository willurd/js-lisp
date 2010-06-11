(defun clear ()
	"Clears the screen and returns the cursor to the home position."
	;; Clear the entire screen
	(process.stdout.write "\x1b[2J")
	;; Move the cursor the the top-left of the screen
	(process.stdout.write "\x1b[H"))
