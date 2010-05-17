(defun say-hi (id)
	(let ((element (document.getElementById id)))
		(element.appendChild (document.createTextNode "hi!"))))
