(defun toggle-non-planned-items ()
  (let ((items            ($ ".noPlans"))
		(first            (first items))
		(tool-non-planned ($ "#tool-non-planned")))
	(if (== first.style.display "none")
		(progn
		  (items.show)
		  (tool-non-planned.html "Hide non-planned items"))
	  (items.hide)
	  (tool-non-planned.html "Show non-planned items")))
  ;; This is so the browser won't actually change to the url of
  ;; the link that was clicked to call this function.
  false)

(defmacro object-from-plist (plist)
	`(object @,,plist))

(defun setup-symbols ()
	(dolist (set symbols)
		(let ((body (document.getElementById "body"))
			  (name (first set))
			  (syms (rest  set))
			  (html "")
			  (div (document.createElement "div")))
			;; Make a div to put all the html into.
			(div.setAttribute "class" "symbolSet")
			;; Loop over each symbol and add the evaluated symbol-template to html.
			(dolist (sym syms)
				(setq html (concat html (tmpl symbol-template (object-from-plist sym)))))
			;; Evaluate the set-template.
			(setq div.innerHTML (tmpl set-template (object :setname name :symbols html)))
			(body.appendChild div))))
