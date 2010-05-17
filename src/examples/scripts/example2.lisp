(setq say-hi
	(lambda (id)
		(let ((element (document.getElementById id)))
			(element.appendChild (document.createTextNode "hi!")))))
