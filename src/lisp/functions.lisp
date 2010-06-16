(defun sort! (array:Array &opt compare::function)
  "Sorts and returns the given array. This function uses
parameter type checking to automatically make sure it gets
the right kinds of arguments. In this function, for example,
you know that when you reach the function body you have
an Array as your first argument and a function as your
second (or undefined for either, as &opt has not been
implemented yet)."
  (||= compare (lambda (a b) (if (> a b) 1 -1)))
  (array.sort compare))

(defun sort (array:Array &opt compareFunc::function)
  "Returns a sorted copy of the given array."
  (sort! (array.concat) compareFunc))

;; This was the old (sort) before parameter type-checking and &opt:
; (defun sort (array compareFunc)
;   "Returns a sorted copy of the given Array."
;   ;; Input validation
;   (assert (and (>= (length arguments) 1) (<= (length arguments) 2))
;           (format nil "(sort) requires 1 or 2 arguments (got %l)" (length arguments)))
;   (assert (is-array array)
;           (format nil "(sort) requires an Array as its argument (got %l)" array))
;   (sort! (array.concat) compareFunc))
