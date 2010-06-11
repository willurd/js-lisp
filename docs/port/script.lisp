(defun toggle-non-planned-items ()
  (let ((items            ($ ".noPlans"))
		(first            (first items))
		(tool-non-planned ($ "#tool-non-planned")))
	(if (== first.style.display "none")
		(progn
		  (items.show)
		  (tool-non-planned.html "Hide non-planned items"))
	  (items.hide)
	  (tool-non-planned.html "Show non-planned items")))
  ;; This is so the browser won't actually change to the url of
  ;; the link that was clicked to call this function.
  false)

;; This will run when the page finishes loading.
($ (lambda ()
  ;; Count all of the finished and unfinished symbols and place
  ;; their counts in the page header.
  (let ((done             ($ ".done"))
		(not-done         ($ ".notDone"))
		(done-label       ($ "#done-count"))
		(not-done-label   ($ "#not-done-count"))
		(tool-non-planned ($ "#tool-non-planned")))
	;; Set the labels for the done/not done counts
	(done-label.html done.length)
	(not-done-label.html not-done.length)
	
	;; Initialize the "non-planned" toggler and do the initial toggle
	(tool-non-planned.click toggle-non-planned-items)
	(toggle-non-planned-items))))
