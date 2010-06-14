(defun sort (array compareFunc)
  "Returns a sorted copy of the given Array."
  ;; Input validation
  (assert (or (>= (length arguments) 1) (<= (length arguments) 2))
          (format nil "(sort) requires 1 or 2 arguments (got %l)" (length arguments)))
  (assert (is-array array)
          (format nil "(sort) requires an Array as its argument (got %l)" array))
  (sort! (array.concat) compareFunc))
