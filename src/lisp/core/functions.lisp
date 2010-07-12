(defun load (path::string)
  (lisp.load path))


(defun eval-string (str::string)
  "Takes a string and evaluates it in the current lisp environment."
  (lisp.eval str))


(defun doc (callable)
  "Returns the documentation of a given function or macro."
  (when (or (is-function callable)
            (is-macro callable))
    (getkey callable :documentation)))


(defun !! (value)
  "An alias for (not (not value)) or (and value)."
  (and value))


(setq ! #'not) ;; Alias, just because


(defun regex (flags & rest)
  (assert (or (is-null flags)
              (is-string flags)
              (is-undefined flags)) (format nil "(regex) \
expects a string or nil as its first argument (got %l)" flags))
  (new RegExp (apply #'concat rest) flags))


(defun starts-with (str::string pattern &opt flags::string)
  (when (instanceof pattern RegExp)
    (setq pattern pattern.source))
  (assert (is-string pattern) (format nil "starts-with expects a string or \
regex as its second argument (got %l)" pattern))
  (!! (str.match (regex flags "^" pattern))))


(defun ends-with (str::string pattern &opt flags::string)
  (when (instanceof pattern RegExp)
    (setq pattern pattern.source))
  (assert (is-string pattern) (format nil "ends-with expects a string or \
regex as its second argument (got %l)" pattern))
  (!! (str.match (regex flags pattern "$"))))


(defun rest (seq)
  (if (== sequence.length 0)
      nil
    (seq.slice 1)))


(defun first (seq)
  (if (== sequence.length 0)
      nil
    (nth seq 0)))


(defun second (seq)
  (if (== sequence.length 0)
      nil
    (nth seq 1)))


(defun third (seq)
  (if (== sequence.length 0)
      nil
    (nth seq 2)))


(defun sort! (array:Array &opt compare::function)
  "Sorts and returns the given array. This function uses parameter
type checking to automatically make sure it gets the right kinds of
arguments. In this function, for example, you know that when you
reach the function body you have an Array as your first argument
and a function as your second (or undefined, because it's optional)."
  (array.sort (|| compare (lambda (a b) (if (> a b) 1 -1)))))


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


(defun xrange (a::number &opt b::number step::number)
  "Returns a generator that returns a number each time it is
called, starting with `a', ending when it is greater than `b',
and incrementing by `step'."
  ;; Sanatize the input
  (when (is-undefined b)
    (setq b a)
    (setq a 0))
  (||= step 1)
  ;; Return the generator
  (let ((last  a))
    (generator ()
      (let ((ret last))
        (setq last (+ last step))
        (if (> ret b)
            (stop-iteration)
          ret)))))


(defun is-even (num::number)
  "Returns true if the given number is even."
  (== 0 (% num 2)))


(defun round (num::number)
  "A shortcut for Math.round."
  (Math.round num))


(defun random ()
  "A shortcut for Math.random."
  (Math.random))
