					; scan-setsubs code - change it some & put into utilities directory as scan-setsubs.lisp
					; with a scan-setsubs.doc file

; scan a list of proof files to see if the proof uses a setsub
(defun scan-setsubs (proof-files)
  (let ((info nil))
    (dolist (pff proof-files info)
      (restoreproof pff)
      (let ((s (nd-setsub-p)))
	(when s
	  (msgf "Proof " dproof " in file " pff " uses setsubs:")
	  (dolist (a s (msgf t))
	    (msgf "Line " (car a) " uses setsub " ((cadr a) . gwff)))
	  (push (list pff dproof (get dproof 'gwff-name) s) info))))))

(defun nd-setsub-p ()
  (let ((setsub-info nil))
    (dolist (line (proof-lines dproof) setsub-info)
    (when (and (member (car (line-justification line))
		       '("UI" "EGen") :test #'equal)
	       (auto::compound-formula-p (caadr (line-justification line))))
      (push (list (linealias line) (caadr (line-justification line)))
	    setsub-info)))))


