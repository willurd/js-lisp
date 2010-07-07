(defmacro generator (arglist & body)
  `(new lisp.Generator (lambda ,arglist
    (let ((stop-iteration (lambda ()
            (throw (new lisp.exception.StopIteration)))))
      ,@body))))

(defun xrange (a::number &opt b::number step::number)
  ;; Sanatize the input
  (when (is-undefined b)
    (setq b a)
    (setq a 0))
  (when (is-undefined step)
    (setq step 1))
  ;; Return the generator
  (let ((last  a))
    (generator ()
      (let ((ret last))
        (setq last (+ last step))
        (if (> ret b)
            (stop-iteration)
          ret)))))
