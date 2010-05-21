;; This will run when the page finishes loading.
($ (lambda ()
	;; Count all of the finished and unfinished symbols and place
	;; their counts in the page header.
	(let ((done           ($ ".done"))
		  (not-done       ($ ".notDone"))
		  (done-label     ($ "#done-count"))
		  (not-done-label ($ "#not-done-count")))
		(done-label.html done.length)
		(not-done-label.html not-done.length))))
