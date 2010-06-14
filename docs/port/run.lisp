;; This will run when the page finishes loading.
($ (lambda ()
  ;; Count all of the finished and unfinished symbols and place
  ;; their counts in the page header.
  (let ((done             ($ ".done"))
		(not-done         ($ ".notDone"))
		(done-label       ($ "#done-count"))
		(not-done-label   ($ "#not-done-count"))
		(tool-non-planned ($ "#tool-non-planned")))
	;; Add all the symbols to the page
	(setup-symbols)
	
	;; Set the labels for the done/not done counts
	(done-label.html done.length)
	(not-done-label.html not-done.length)
	
	;; Initialize the "non-planned" toggler and do the initial toggle
	(tool-non-planned.click toggle-non-planned-items)
	(toggle-non-planned-items))))
