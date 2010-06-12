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
  "Iterates over each (key,value) pair in `lst' (denoted by
`itemName'), evaluating each expression in `body' for every
iteration, and collecting the items, whose last `body' expression
evaluates to true, into a final list that is returned."
  ;; Input validation
  (let ((basemsg "(collect) expects arguments in the form: ((itemName lst) &body)."))
    (assert (is-symbol item-name)
            (format nil "%s Got bad symbol argument: %s" basemsg item-name)))
  `(let ((set (list)))
    (foreach (,item-name ,lst)
      (when (progn ,@body)
        (push set ,item-name)))
  set))

(defun sort (lst)
	(sort! (lst.concat)))

