(defobject path
  :sep "/"
  
  :remove-sep (lambda (p)
    "Removes path.sep from the end of the given path if
it is there."
    (if (ends-with p utils.path.sep)
        (p.slice 0 (1- p.length))
      p))
  
  :join (lambda (& paths)
    "Joins the given paths together with path.sep."
    (join utils.path.sep (map utils.path.remove-sep paths))))

