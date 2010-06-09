(defmacro collect ((itemName lst) &body body)
  "Iterates over each (key,value) pair in `lst' (denoted by
`itemName'), evaluating each expression in `body' for every
iteration, and collecting the items, whose last `body' expression
evaluates to true, into a final list that is returned."
  (let ((basemsg "(collect) expects arguments in the form: ((itemName lst) &body)."))
    (when (not (is-symbol itemName))
      (throw (new Error (format nil "%s Got bad symbol argument: %s" basemsg lst))))
    (when (not (is-list lst))
      (throw (new Error (format nil "%s Got bad list argument: %s" basemsg lst)))))
  `(let ((set (list)))
    (foreach (,itemName ,lst)
      (when (progn ,@body)
        (push set ,itemName)))
	set))

