(defun window.onload (e)
	"Updates an HTML element using the DOM"
	(let ((div (document.getElementById "thediv")))
	  (print "before:" div.textContent)
	  (setq div.textContent "it works!")
	  (print "after:" div.textContent)))
