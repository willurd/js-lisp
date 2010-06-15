# Ideas for typed arguments in functions and macros

If given a single atom, the argument must match that atom. If given a list of atoms, the argument must match one of them. The atoms can be one of two things, a Symbol or a Keyword. If the atom is a Symbol, the match is performed as such: typeof(argument) == atom.toString(). If the atom is a Keyword, the match is performed like so: (argument instanceof lisp.env.get(atom.toString())). If a given atom is neither a Symbol nor a Keyword an error will be thrown ("Argument type specification must be a Symbol or Keyword"). If any given argument during a function (or macro) call does not match the required argument type (again, only if a required type is specified) an error will be thrown ("Argument " + toLisp(argument) + " does not match the required type " + toLisp(type), or "Argument " + toLisp(argument) + " does not match one of the required types " + toLisp(types)).

(defun first (seq:Array)
  (nth 0 lst))

(defun push (seq:Array item) ; seq must be an Array, whereas item can be anything
  (seq.push item))

(defun starts-with (str:String pattern:(String RegExp) &opt start::number end::number)
  (when (is-regexp pattern)
    (setq pattern pattern.source))
  (!! (str.match (new RegExp (concat "^" pattern)))))

(defun request (url &key method callback:function)
  ...)

(request "google.com" :method "GET" :callback (lambda (req:XMLHttpRequest) (print e.responseText)))

(defun starts-with ((str String) (pattern (String RegExp))
                    &opt ((start :number) 0) ((end :number) -1))
  ...)

(defun starts-with ([str String]) ...)

(defun starts-with (str->String) ...)

(defun starts-with (str<String>) ...)
(defun starts-with (str<:number>) ...)
(defun starts-with (str<String,RegExp>) ...)
