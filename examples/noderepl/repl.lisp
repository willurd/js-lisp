(defclass Repl
  "A \"read-eval-print loop\" written in lisp, for lisp."
  :init (lambda (props)
    "Initializes the repl with the given properties, or the defaults."
    (||= props (object))
    (setq this.ps1 (|| props.ps1 ">> "))
    (setq this.ps2 (|| props.ps2 ".. "))
    (setq this.ps3 (|| props.ps3 "=> "))
    (setq this.multilineCommand false)
    (setq this.data "")
    (this.newCommand))
  
  :handleError (lambda (error)
    "Prints the given error and a new prompt."
    (print error)
    (this.newCommand))
  
  :newCommand (lambda ()
    "Prints a new prompt."
    (let ((prompt (if this.multilineCommand
                      this.ps2
                    this.ps1)))
      (process.stdout.write prompt)))
  
  :handleData (lambda (data)
    "Handles data from stdin."
    (setq this.data (concat this.data "\n" (to-string data)))
    (if (== data "\n")
        (this.newCommand)
      (try
          (let ((expressions (lisp.parse.script this.data))
                (ret         nil)
                (str         nil))
            (when (> (length expressions) 1)
              (format t "** evaluating %s expressions **" (length expressions)))
            (dolist (expression expressions)
              (setq str (lisp-string expression))
              (when (> expressions.length 1)
                (this.newCommand)
                (print str))
              (try
                  (setq ret (lisp-string (lisp.eval str)))
                  (format t "%s%s" this.ps3 ret)
                (:catch (e)
                  (cond ((instanceof e lisp.exception.StreamEOFException) (throw e))
                        (t (print e)))))))
            (setq this.multilineCommand false)
            (setq this.data "")
            (this.newCommand)
          (:catch (e)
            (cond ((instanceof e lisp.exception.StreamEOFException)
                   (setq this.multilineCommand true)
                   (this.newCommand))
                  (t (setq this.multilineCommand false)
                     (setq this.data "")
                     (format t "%l" this)
                     (this.handleError e))))))))
