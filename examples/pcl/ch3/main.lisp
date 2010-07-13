;; js-lisp/node implementation of http://www.gigamonkeys.com/book/practical-a-simple-database.html

(setq defvar setq)

(defvar sys (require "sys"))
(defvar fs  (require "fs"))

(defvar *db* (list))
(defvar *db-filename* "cds.db")

(defmacro with-open-file ((fd-name flags filename) & body)
  (when (not fs)
    (setq fs (require "fs")))
  `(let ((,fd-name (fs.openSync ,filename ,flags)))
    ,@body
    (fs.close ,fd-name)))

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd)
  (push *db* cd))

(defun dump-db ()
  (dolist (cd *db*)
    (print "-------------------------")
    (dotimes (i (length cd))
      (let ((key (nth cd i))
            (value (nth cd (inc i))))
        (format t "%8s:  %s" key value)))
    (print "")))

(defun write (& rest)
  (rest.unshift nil)
  (process.stdout.write (apply #'format rest)))

(defun save-db ()
  (with-open-file (fd "w" *db-filename*)
    (fs.writeSync fd (format nil "%l\n" *db*))))

(defun load-db ()
  (setq *db* (eval-string (concat "'" (fs.readFileSync *db-filename*)))))

(load-db)
(dump-db)
