(let ((i 0))
  (labels
    (:one
      (print i)
      (when (> i 2)
        (goto :three)))
    (:two
      (setq i (1+ i))
      (goto :one))
    (:three
      (print "Done!")))
  (assert (== i 3) (concat "i is not three, it's " i)))

;; The Art of Computer Programming Volume 2, Page 145
(defun shuffle! (lst)
  (let ((j)
        (u)  ;; Random number between 0 and 1
        (k)) ;; Random number between 1 and j
    (labels
      (:p1 ;; Initialize
        (setq j (1- (length lst))))
      (:p2 ;; Generate
        (setq u (Math.random)))
      (:p3 ;; Exchange
        (setq k (Math.round (* u j)))
        (let ((temp (nth lst k)))
          (setkey lst k (nth lst j))
          (setkey lst j temp)))
      (:p4 ;; Decrease
        (setq j (1- j))
        (when (> j 1)
          (goto :p2)))))
  lst)

(let ((lst '(1 2 3 4 5 6 7 8 9)))
  (format t "%l" lst)
  (format t "%l" (shuffle! lst)))
