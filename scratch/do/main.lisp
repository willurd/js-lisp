(print "------- 1 -------")

(do ((i 0 (1+ i)))
    ((>= i 4))
  (print i))

(print "------- 2 -------")

(dotimes (i 4)
  (print i)
  (inc i))

(print "------- 3 -------")

(dolist (item '(:one :two :three))
  (format t "%l" item))
