(defun sort (array)
  "Returns a sorted copy of the given Array."
  ;; Input validation
  (assert (=== (length arguments) 1)
          (format nil "(sort) requires 1 argument (got %l)" (length arguments)))
  (assert (is-array array)
          (format nil "(sort) requires an Array as its argument (got %l)" array))
  (sort! (lst.concat)))
