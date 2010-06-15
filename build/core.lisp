(defmacro defclass (name & params)
  "Creates a new class, using lisp.Class, passing in the given
arguments as a plist to lisp.Class.extend()."
  ;; Check for the existence of a doc string
  (when (and (== (% (length params) 2) 1)
             (is-string (first params)))
    (params.shift))
  `(setq ,name (lisp.Class.extend (object ,@params))))

; (defmacro dolist ((var lst) & body)
;   (let ((indexname (gensym))
;         (count (length lst)))
;     (print indexname)
;     `(do ((,indexname 0 (1+ ,indexname))
;           (,var (nth ,indexname ,lst)))
;          ((>= ,indexname ,count))
;       ,@body)))
; 
; (defmacro dotimes ((var count) & body)
;   `(do ((,var 0 (1+ ,var)))
;        ((>= ,var ,count))
;     ,@body))

(defmacro collect ((item-name lst) & body)
  "Creates a list of (key value) pairs from `object', iterates over
each one, evaluating all of the expressions in `body' for every
iteration, and collecting into a list every item where the last
`body' expression evaluates to true."
  ;; Input validation
  (let ((basemsg "(collect) expects arguments in the form: ((item-name lst) &body)."))
    (assert (is-symbol item-name)
            (format nil "%s item-name must be a symbol (got %s)" basemsg item-name)))
  `(let ((set (list)))
    (foreach (,item-name ,lst)
      (when (progn ,@body)
        (push set ,item-name)))
    set))

(defun sort (array compareFunc)
  "Returns a sorted copy of the given Array."
  ;; Input validation
  (assert (or (>= (length arguments) 1) (<= (length arguments) 2))
          (format nil "(sort) requires 1 or 2 arguments (got %l)" (length arguments)))
  (assert (is-array array)
          (format nil "(sort) requires an Array as its argument (got %l)" array))
  (sort! (array.concat) compareFunc))

(defun typed-sort (seq:Array compareFunc::function)
  (sort! (seq.concat) compareFunc))

