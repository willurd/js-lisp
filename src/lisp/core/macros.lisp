(setq case switch) ;; Adds an alias to switch for case


(defmacro defclass (name & params)
  "Creates a new class, using lisp.Class, passing in the given
arguments as a plist to lisp.Class.extend()."
  ;; Check for the existence of a doc string
  (when (and (== (% (length params) 2) 1)
             (is-string (first params)))
    (params.shift))
  `(setq ,name (lisp.Class.extend (object ,@params))))


(defmacro defobject (name & props)
  "Creates an object from the given arguments (after name) as
a plist."
  ;; Check for the existence of a doc string
  (when (and (== (% (length props) 2) 1)
             (is-string (first props)))
    (props.shift))
  `(setq ,name (object ,@props)))


(defmacro dotimes ((name count) & body)
  `(do ((,name 0 (1+ ,name)))
       ((>= ,name ,count))
    ,@body))


(defmacro dolist ((name lst) & body)
  (let ((indexname (gensym)))
    `(do ((,indexname 0 (1+ ,indexname))
          (,name (nth ,lst ,indexname)))
         ((>= ,indexname (length ,lst)))
      ,@body)))


(defmacro collect ((item-name lst) & body)
  "Creates a list of (key value) pairs from `object', iterates over
each one, evaluating all of the expressions in `body' for every
iteration, and collecting into a list every item where the last
`body' expression evaluates to true."
  ;; Input validation
  (let ((basemsg "(collect) expects arguments in the form: ((item-name lst) & body)."))
    (assert (is-symbol item-name)
            (format nil "%s item-name must be a symbol (got %s)" basemsg item-name)))
  `(let ((set (list)))
    (foreach (,item-name ,lst)
      (when (progn ,@body)
        (push set ,item-name)))
    set))


(defmacro generator (arglist & body)
  "A shortcut for defining a new generator. The new generator
closes over a shortcut function for throwing a StopIteration."
  `(let ((stop-iteration (lambda ()
            (throw (new lisp.exception.StopIteration)))))
     (new lisp.Generator (lambda ,arglist
       ,@body))))
