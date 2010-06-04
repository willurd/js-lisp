(defun say-hi (id)
	(let ((element (document.getElementById id)))
		(element.appendChild (document.createTextNode "hi!"))))

(defun window.onload ()
	(say-hi "test-results"))
