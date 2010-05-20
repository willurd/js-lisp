($ (lambda ()
	;; Initialize the UI elements
	(let ((accordion       ($ "#accordion"))
		  (tabs            ($ "#tabs"))
		  (dialog          ($ "#dialog"))
		  (dialog-link     ($ "#dialog_link"))
		  (datepicker      ($ "#datepicker"))
		  (slider          ($ "#slider"))
		  (progressbar     ($ "#progressbar"))
		  (static-widgets  ($ "#dialog_link, ul#icons li")))
		;; Accordion
		(accordion.accordion (object :header "h3"))
		;; Tabs
		(tabs.tabs)
		;; Dialog
		(let ((close (lambda ()
				(let ((dialog ($ this)))
					(dialog.dialog "close")))))
			(dialog.dialog (object
				:autoOpen false
				:width    600
				:buttons  (object
					"Ok"     close
					"Cancel" close))))
		(dialog-link.click (lambda ()
			(dialog.dialog "open")
			false))
		;; Slider
		(slider.slider (object
			:range  t
			:values (array 17 64)))
		;; Date Picker
		(datepicker.datepicker (object :inline t))
		;; Progress Bar
		(progressbar.progressbar (object
			:value 20))
		;; "hover states on the static widgets"
		(static-widgets.hover
			(lambda ()
				(let ((widget ($ this)))
					(widget.addClass "ui-state-hover")))
			(lambda ()
				(let ((widget ($ this)))
					(widget.removeClass "ui-state-hover")))))))

($ (lambda ()
	(let ((update-progressbar (lambda ()
			(let ((progressbar     ($ "#progressbar")))
				(progressbar.progressbar (object
					:value (% (1+ (progressbar.progressbar "option" "value")) 100))))
			(window.setTimeout (getfunc update-progressbar) 100))))
		(update-progressbar))))
