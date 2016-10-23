;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of review-flags)

(context flag-review)

(defvar *func-hash* nil)
(defvar *flag-hash* nil)
(defvar *flag-rel-hash* nil)
(defvar *flag-paths-hash* nil)

(defmenuitem save-flag-relevancy-info
  (display-name "SAVE-FLAG-RELEVANCY-INFO")
  (placement 13.1)
  (command "SAVE-FLAG-RELEVANCY-INFO")
  (parent REVIEW)
  (remote-expert T)
  (etps t)
  (mhelp ""))

(defreview save-flag-relevancy-info
  (argnames filename)
  (argtypes filespec)
  (defaultfns (lambda (filename)
		(list (if (eq filename '$) "flagrel" filename))))
  (arghelp "File to save Flag Relevancy Info built from Lisp Source Files.")
  (mhelp "Save Flag Relevancy Info built from Lisp Source Files

SEE ALSO:  UPDATE-RELEVANT, SHOW-RELEVANCE-PATHS"))

(defutil analyze-flag-dependencies
  (form-type function)
  (keywords flags)
  (mhelp "Analyze the functions defined in TPS lisp files
and print a list of search related flags and conditions under 
which they are relevant."))

(defun save-flag-relevancy-info (filename)
  (unless (and (hash-table-p *flag-hash*) (hash-table-p *flag-paths-hash*)
	       (hash-table-p *flag-rel-hash*))
    (when (query "Examine Lisp Files To Build Flag Relevancy Hash Tables?" t)
      (analyze-flag-dependencies)))
  (when (and (hash-table-p *flag-hash*) (hash-table-p *flag-paths-hash*)
	     (or (not *running-remotely*) *expert-running-remotely*))
    (with-open-file
     (str filename :direction :output :if-exists :supersede :if-does-not-exist :create)
     (format str "~S~%; flag-hash~%(" (stringdt nil))
     (maphash #'(lambda (key value)
		  (format str "(~S~%~S)~%" key value))
	      *flag-hash*)
     (format str ")~%; flag-rel-hash~%(")
     (maphash #'(lambda (key value)
		  (format str "(~S~%~S)~%" key value))
	      *flag-rel-hash*)
     (format str ")~%; flag-path-hash ~%(")
     (maphash #'(lambda (key value)
		  (format str "(~S~%~S)~%" key value))
	      *flag-paths-hash*)
     (format str ")~%"))))

(defun load-flag-relevancy (filename)
  (with-open-file 
   (str filename :direction :input)
   (let* ((date (read str nil nil))
	  (fhi (read str nil nil))
	  (frhi (read str nil nil))
	  (fphi (read str nil nil)))
     (when date
       (msgf "Loading Flag Relevancy Info Saved " t date t))
     (setq *flag-hash* (make-hash-table :test #'equal))
     (setq *flag-rel-hash* (make-hash-table :test #'equal))
     (setq *flag-paths-hash* (make-hash-table :test #'equal))
     (dolist (kv fhi)
	     (setf (gethash (car kv) *flag-hash*) (cadr kv)))
     (dolist (kv frhi)
	     (setf (gethash (car kv) *flag-rel-hash*) (cadr kv)))
     (dolist (kv fphi)
	     (setf (gethash (car kv) *flag-paths-hash*) (cadr kv))))))

(defun choose-relevancy-data ()
  (msgf "1)  Use Flag Relevancy Information Currently In Memory.")
  (msgf "2)  Load Flag Relevancy Information From A File.")
  (msgf "3)  Build Flag Relevancy Information From Lisp Source Files.  (This will take a while.)")
  (let ((i (get-a-number 3)))
    (cond ((equal i 2) 
	   (let ((filename nil)) 
	     (prompt-read filename nil (msgf "Flag Relevancy Filename") 'filespec "flagrel"
			  ((? (msgf "Update-Relevant Allows the User to Update a Flag and Then Other Relevant Flags"))
			   (?? (msgf "Filename Containing Flag Relevancy Info"))))
	     (load-flag-relevancy filename)))
	  ((equal i 3)
	   (analyze-flag-dependencies)))))

 ; useful for debugging
(defun print-flag-relevancy ()
  (when (hash-table-p *flag-hash*)
    (msgf t "Flags Which Are Always Relevant:")
    (let ((always-relevant nil))
      (maphash #'(lambda (key value)
		   (when (equal value '(T))
		     (push (intern key) always-relevant)))
	       *flag-hash*)
      (terpri)
      (print-unordered-symbol-table always-relevant))
    (msgf t "Flag Dependencies:")
    (maphash #'(lambda (key value)
		 (unless (equal value '(T))
		   (msgf t key ":")
		   (if (> (length value) 2)
		       (pprint (cons 'or value))
		     (pprint (car value)))))
	     *flag-hash*)))

(defun analyze-flag-dependencies ()
  (setq *func-hash* (make-hash-table :test #'equal))
  (setq *flag-hash* (make-hash-table :test #'equal))
  (setq *flag-rel-hash* (make-hash-table :test #'equal))
  (setq *flag-paths-hash* (make-hash-table :test #'equal))
  (let ((eof 'eof-fn-read)
	(all-flags 
	 (remove-if-not #'(lambda (f)
			    (and (symbolp f)
				 (not (member (symbol-name f)
					      '("FIRST-ORDER-MODE-MS" ; this gets dynamically changed a lot
						"TREAT-HLINES-AS-DLINES" "LAST-MODE-NAME"
						"LEAF-NAME" "EXPANSION-NAME" "EPROOF-NAME" "MATINGSTREE-NAME"
						"SKOLEM-SELECTION-NAME" "NEG-NAME" "IMP-NAME" "TRUE-NAME" "REWRITE-NAME"
						"ECONJ-NAME" "FALSE-NAME" "PRINT-COMMENTS" "SELECTION-NAME"
						"HLINE-JUSTIFICATION" "MATING-NAME" "EDISJ-NAME")
					      :test #'equal))
				 (set-difference (get f 'subjects)
						 '(printing editor test-top otl-vars
							    internal-names saving-work
							    suggests jforms
							    wff-prims parsing
							    events gr-filenames gr-misc
							    maintain system rules-mod 
							    library))))
			global-flaglist))
	(all-fns nil)
	(analyzed-fns nil))
    (declare (special analyzed-fns))
    (msgf "Reading Lisp Files.")
    (dolist (file global-filelist)
	    (when (symbolp file)
	      (let* ((fname (format nil "~d.lisp"
				    (string-downcase (symbol-name file))))
		     (fpath (locate-tps-file fname nil)))
		(when fpath
		  (let ((f (open fpath :direction :input)))
		    (do ((r (read f nil eof) (read f nil eof)))
			((equal r eof) (close f))
			(when (and (consp r) (member (car r) '(defun defmacro)))
			  (setf (gethash (symbol-name (cadr r)) *func-hash*) (cddr r))
			  (push (cadr r) all-fns))))))))
    (msgf "Analyzing Functions.")
    (dolist (f all-fns)
	    (setf (gethash (symbol-name f) *func-hash*)
		  (analyze-flags-in-fn (gethash (symbol-name f) *func-hash*)
				       all-fns all-flags)))
    (format t "Building Flag Dependencies.~%")
    (analyze-relevancy-of-flags 'auto::diy '(auto::diy))
    (fill-in-flag-rel-hash all-flags)))

(defun analyze-flags-in-fn (fn-def all-fns all-flags &optional (conds T))
  (if (consp fn-def)
      (cond ((and (member (car fn-def) '(let let*))
		  (consp (cdr fn-def))
		  (listp (cadr fn-def)))
	     (let ((all-flags2 all-flags) res)
	       (dolist (e (cadr fn-def))
		       (when (and (consp e) (consp (cdr e)))
			 (setq res
			       (append res
				       (analyze-flags-in-fn (cadr e) all-fns
							    (if (eq (car fn-def) 'let)
								all-flags
							      all-flags2)
							    conds)))
			 (setq all-flags2 (remove (car e) all-flags2))))
	       (append res
		       (analyze-flags-in-fn (cddr fn-def) all-fns all-flags2 conds))))
	    ((and (eq (car fn-def) 'if)
		  (consp (cdr fn-def)))
	     (let* ((con (cadr fn-def))
		    (mcon (modify-lisp-condition con all-flags))
		    (res (analyze-flags-in-fn mcon all-fns all-flags conds)))
	       (let ((conds2 (if (easy-flag-condition-p mcon all-flags)
				 (if (eq conds 'T) mcon (list 'AND mcon conds))
			       conds)))
		 (when (consp (cddr fn-def))
		   (setq res (append res
				     (analyze-flags-in-fn (caddr fn-def) all-fns all-flags conds2)))
		   (when (consp (cdddr fn-def))
		     (let ((nconds2 (if (easy-flag-condition-p mcon all-flags)
					(if (eq conds 'T) (list 'NOT mcon)
					  (list 'AND (list 'NOT mcon) conds))
				      conds)))
		       (setq res
			     (append res
				     (analyze-flags-in-fn (cadddr fn-def)
							  all-fns all-flags nconds2)))))))
	       res))
	    ((eq (car fn-def) 'when)
	     (let* ((con (cadr fn-def))
		    (res (analyze-flags-in-fn con all-fns all-flags conds)))
	       (let ((conds2 (if (easy-flag-condition-p con all-flags)
				 (if (eq conds 'T) con (list 'AND con conds))
			       conds)))
		 (when (consp (cddr fn-def))
		   (append res
			   (analyze-flags-in-fn (caddr fn-def) all-fns all-flags conds2))))))
	    ((eq (car fn-def) 'unless)
	     (let* ((con (cadr fn-def))
		    (res (analyze-flags-in-fn con all-fns all-flags conds)))
	       (let ((conds2 (if (easy-flag-condition-p con all-flags)
				 (if (eq conds 'T) (list 'NOT con) 
				   (list 'AND (list 'NOT con) conds))
			       conds)))
		 (when (consp (cddr fn-def))
		   (append res
			   (analyze-flags-in-fn (caddr fn-def) all-fns all-flags conds2))))))
	    ((eq (car fn-def) 'cond)
	     (analyze-flags-in-cond (cdr fn-def) all-fns all-flags conds))
	    ((and (eq (car fn-def) 'case) (consp (cdr fn-def)))
	     (let ((c (cadr fn-def)))
	       (if (member c all-flags)
		   (analyze-flags-in-case (cddr fn-def) c all-fns all-flags conds)
		 (analyze-flags-in-fn (cdr fn-def) all-fns all-flags conds))))
	    (t
	     (append
	      (analyze-flags-in-fn (car fn-def) all-fns all-flags conds)
	      (analyze-flags-in-fn (cdr fn-def) all-fns all-flags conds))))
    (if (member fn-def all-flags)
	(list (acons 'flag conds fn-def))
      (when (member fn-def all-fns)
	(list (acons 'func conds fn-def))))))

(defun analyze-flags-in-cond (cond-body all-fns all-flags conds)
  (if (consp cond-body)
      (if (consp (car cond-body))
	  (let* ((mcon (modify-lisp-condition (caar cond-body) all-flags))
		 (conds2 (if (easy-flag-condition-p mcon all-flags)
			    (if (eq conds 'T) mcon (list 'AND mcon conds))
			   conds))
		 (nconds2 (if (easy-flag-condition-p mcon all-flags)
			      (if (eq conds 'T) (list 'NOT mcon)
				(list 'AND (list 'NOT mcon) conds))
			    conds)))
	    (append
	     (analyze-flags-in-fn mcon all-fns all-flags conds)
	     (analyze-flags-in-fn (cdar cond-body) all-fns all-flags conds2)
	     (analyze-flags-in-cond (cdr cond-body) all-fns all-flags nconds2)))
	(analyze-flags-in-cond (cdr cond-body) all-fns all-flags conds))
    nil))

(defun analyze-flags-in-case (cas flag all-fns all-flags conds)
  (if (consp cas)
      (if (consp (car cas))
	  (if (member (caar cas) '(T OTHERWISE))
	      (analyze-flags-in-fn (cdar cas) all-fns all-flags conds)
	    (let ((cas1 (if (consp (caar cas)) (caar cas) (list (caar cas))))
		  (cond1 nil))
	      (dolist (val cas1)
		      (if cond1
			  (setq cond1 (list 'OR (list 'eq flag (list 'quote val)) cond1))
			(setq cond1 (list 'eq flag (list 'quote val)))))
	      (append
	       (analyze-flags-in-fn (cdar cas) all-fns all-flags
				    (list 'AND cond1 conds))
	       (analyze-flags-in-case (cdr cas) flag all-fns all-flags
				      (list 'AND (list 'NOT cond1) conds)))))
	(append
	 (analyze-flags-in-fn (car cas) all-fns all-flags conds)
	 (analyze-flags-in-case (cdr cas) flag all-fns all-flags
				(list 'NOT conds))))
    (analyze-flags-in-fn cas all-fns all-flags conds)))

					; turn and's and or's into if's 
					; since and's and or's aren't
					; operationally commutative
(defun modify-lisp-condition (con all-flags &optional (pos t))
  (if (consp con)
      (case (car con)
	((AND OR) (if (cdr con)
		      (if (consp (cdr con))
			  (if (cddr con)
			      (list 'if 
				    (modify-lisp-condition
				     (cadr con) all-flags pos)
				    (if (or (and (eq (car con) 'AND) pos)
					    (and (eq (car con) 'OR) (not pos)))
					(modify-lisp-condition
					 (cons (car con) (cddr con))
					 all-flags pos)
				      pos)
				    (if (or (and (eq (car con) 'AND) (not pos))
					    (and (eq (car con) 'OR) pos))
					(modify-lisp-condition
					 (cons (car con) (cddr con))
					 all-flags pos)
				      (not pos)))
			    (modify-lisp-condition (cadr con) all-flags pos))
			(if pos con (list 'NOT con)))
		    pos))
	(NOT 
	 (if (consp (cdr con))
	     (modify-lisp-condition (cadr con) all-flags (not pos))
	   (if pos con (list 'NOT con))))
	(IF 
	 (if (and (consp (cdr con)) (consp (cddr con)))
	     (if (consp (cdddr con))
		 (list 'IF 
		       (modify-lisp-condition (cadr con) all-flags pos)
		       (modify-lisp-condition (caddr con) all-flags pos)
		       (modify-lisp-condition (cadddr con) all-flags pos))
	       (list 'IF 
		     (modify-lisp-condition (cadr con) all-flags pos)
		     (modify-lisp-condition (caddr con) all-flags pos)))
	   (if pos con (list 'NOT con))))
	((MEMQ MEMBER)
	 (if (and (consp (cdr con)) (consp (cddr con))
		  (member (cadr con) all-flags)
		  (consp (caddr con))
		  (eq (car (caddr con)) 'QUOTE)
		  (consp (cdr (caddr con))))
	     (do ((elts (cadr (caddr con)) (cdr elts))
		  (rcon nil 
			(if rcon ; this OR is commutative since cases are disjoint
			    (list 'OR (list 'eq (cadr con) (list 'quote (car elts))) rcon)
			  (list 'eq (cadr con) (list 'quote (car elts))))))
		 ((not (consp elts))
		  (let ((r (if elts con rcon)))
		    (if pos r (list 'NOT r)))))
	   (if pos con (list 'NOT con))))
	(t (if pos con (list 'NOT con))))
    (if pos con (list 'NOT con))))

(defun easy-flag-condition-p (con all-flags)
  (or (eq con 'T)
      (member con all-flags)
      (if (consp con)
	  (if (and (eq (car con) 'NOT) (consp (cdr con)))
	      (easy-flag-condition-p (cadr con) all-flags)
	    (if (member (car con) '(AND OR IF))
		(easy-flag-conditions-p (cdr con) all-flags)
	      (if (member (car con) '(= < > <= >= eq eql equal neq))
		  (and (consp (cdr con)) (consp (cddr con))
		       (easy-flag-term-p (cadr con) all-flags)
		       (easy-flag-term-p (caddr con) all-flags))
		nil)))
	nil)))

(defun easy-flag-conditions-p (conds all-flags)
  (if (consp conds)
      (and (easy-flag-condition-p (car conds) all-flags)
	   (easy-flag-conditions-p (cdr conds) all-flags))
    (not conds)))

(defun easy-flag-term-p (trm all-flags)
  (or (member trm all-flags)
      (member trm '(NIL T))
      (numberp trm)
      (and (consp trm)
	   (eq (car trm) 'quote))))

(defun analyze-relevancy-of-flags (f path &optional (conds T))
  (declare (special analyzed-fns))
  (let ((rel (gethash (symbol-name f) *func-hash*)))
    (unless (member rel analyzed-fns)
      (push rel analyzed-fns)
      (dolist (a rel)
	      (let ((path2 (cons (cdar a) path))
		    (conds2 (if (eq conds 'T)
				(cdar a) 
			      (if (eq (cdar a) 'T)
				  conds
				(list 'AND (cdar a) conds)))))
		(unless (clearly-inconsistent-conds-p conds2)
		  (let ((conds3 (remove-recursive-conds
				 (symbol-name (cdr a)) conds2)))
		    (if (eq (caar a) 'flag)
			(unless (member T (gethash (symbol-name (cdr a)) *flag-hash*))
			  (setf (gethash (symbol-name (cdr a)) *flag-paths-hash*)
				(adjoin path2
					(gethash (symbol-name (cdr a)) *flag-paths-hash*)
					:test #'equal))
			  (if (eq conds3 T)
			      (setf (gethash (symbol-name (cdr a)) *flag-hash*) '(T))
			    (unless (member conds3
					    (gethash (symbol-name (cdr a)) *flag-hash*)
					    :test #'equal)
			      (push conds3
				    (gethash (symbol-name (cdr a)) *flag-hash*)))))
		      (analyze-relevancy-of-flags (cdr a) 
						  (cons (cdr a) path2)
						  conds3)))))))))

(defun clearly-inconsistent-conds-p (conds &optional easy-conds)
  (if (and (consp conds) (eq (car conds) 'AND))
      (if (find-if #'(lambda (x)
		       (incompatible-easy-conds (cadr conds) x)) easy-conds)
	  t
	(clearly-inconsistent-conds-p (caddr conds) (cons (cadr conds) easy-conds)))
    nil))

(defun incompatible-easy-conds (con1 con2)
  (or (equal (list 'NOT con1) con2)
      (equal con1 (list 'NOT con2))
      (and (consp con1) (consp con2)
	   (or (and (eq (car con1) 'eq) (eq (car con2) 'neq)
		    (equal (cdr con1) (cdr con2)))
	       (and (eq (car con1) 'neq) (eq (car con2) 'eq)
		    (equal (cdr con1) (cdr con2)))
	       (and (eq (car con1) 'eq) (eq (car con2) 'eq)
		    (equal (cadr con1) (cadr con2))
		    (consp (caddr con1)) (consp (caddr con2))
		    (eq (caaddr con1) 'QUOTE)
		    (eq (caaddr con2) 'QUOTE)
		    (not (equal (caddr con1) (caddr con2))))))))

(defun remove-recursive-conds (flagname conds &optional (parity T))
  (if (consp conds)
      (if (member (car conds) '(AND OR))
	  (let ((bl (mapcar #'(lambda (x)
				(remove-recursive-conds flagname x parity))
			    (cdr conds))))
	    (cond ((and (eq (car conds) 'AND)
			(member NIL bl))
		   NIL)
		  ((and (eq (car conds) 'OR)
			(member T bl))
		   T)
		  (t 
		   (when (eq (car conds) 'AND)
		     (setq bl (remove-duplicates (remove T bl) :test #'equal)))
		   (when (eq (car conds) 'OR)
		     (setq bl (remove-duplicates (remove NIL bl) :test #'equal)))
		   (if (> (length bl) 1)
		       (cons (car conds) bl)
		     (if bl
			 (car bl)
		       (if (eq (car conds) 'OR)
			   nil
			 t))))))
	(if (eq (car conds) 'NOT)
	    (let ((b (remove-recursive-conds flagname (cadr conds) (not parity))))
	      (if (eq b T)
		  nil
		(if (eq b NIL)
		    t
		  (list 'NOT b))))
	  (if (condition-depends-on-flag flagname conds)
	      parity
	    conds)))
    (if (and (symbolp conds) (equal flagname (symbol-name conds)))
	parity
      conds)))

(defun condition-depends-on-flag (flagname trm)
  (if (consp trm)
      (if (eq (car trm) 'QUOTE)
	  nil
	(or (condition-depends-on-flag flagname (car trm))
	    (condition-depends-on-flag flagname (cdr trm))))
    (if (and (symbolp trm) (equal flagname (symbol-name trm)))
	T
      nil)))

(defun flags-in-easy-cond (conds all-flags)
  (if (consp conds)
      (if (member (car conds) '(AND OR))
	  (flags-in-easy-cond-list (cdr conds) all-flags)
	(if (eq (car conds) 'NOT)
	    (flags-in-easy-cond (cadr conds) all-flags)
	  (if (member (car conds) '(= < > <= >= eq eql equal neq))
	      (union (flags-in-easy-cond-term (cadr conds) all-flags)
		     (flags-in-easy-cond-term (caddr conds) all-flags))
	    nil)))
    (if (member conds all-flags)
	(list conds)
      nil)))

(defun flags-in-easy-cond-list (condsl all-flags)
  (if condsl
      (union (flags-in-easy-cond (car condsl) all-flags)
	     (flags-in-easy-cond-list (cdr condsl) all-flags))
    nil))

(defun flags-in-easy-cond-term (trm all-flags)
  (if (member trm all-flags)
      (list trm)
    nil))

(defun fill-in-flag-rel-hash (all-flags)
  (let ((always-relevant nil))
    (maphash #'(lambda (key value)
		 (when (equal value '(T))
		   (push (intern key) always-relevant)))
	     *flag-hash*)
    (setf (gethash "ALWAYS-RELEVANT" *flag-rel-hash*) always-relevant)
    (maphash #'(lambda (flag-name conds)
		 (let ((flag (intern flag-name)))
		   (unless (member flag always-relevant)
		     (let* ((conds2 (if (> (length conds) 2)
					(cons 'OR conds)
				      (car conds)))
			    (flags (flags-in-easy-cond conds2 all-flags)))
		       (when flags
			 (dolist (f flags)
				 (push (list flag conds2)
				       (gethash (symbol-name f) *flag-rel-hash*))))))))
	     *flag-hash*)))

; used by update-relevant if the hash tables are built
(defun relevant-flags-from-hash (flag)
  (let ((h (gethash (symbol-name flag) *flag-rel-hash*))
	(rel (gethash "ALWAYS-RELEVANT" *flag-rel-hash*)))
    (dolist (a h rel)
	    (when (eval (cadr a))
	      (setq rel (adjoin (car a) rel))))))

(defmenuitem show-relevance-paths
  (display-name "SHOW-RELEVANCE-PATHS")
  (placement 13.2)
  (command "SHOW-RELEVANCE-PATHS")
  (parent REVIEW)
  (remote-expert T)
  (etps t)
  (mhelp ""))

(defreview show-relevance-paths
  (argnames func-or-flag flag)
  (argtypes symbol tpsflag)
  (defaultfns (lambda (func-or-flag flag)
		(list (if (eq func-or-flag '$) 'AUTO::DIY func-or-flag) flag)))
  (arghelp "Start of Relevance Path: Function Name or Flag" "End of Relevance Path: Flag")
  (mhelp "Given a function F or flag A to start from and a flag B to end at,
show all paths which explain why the flag B should be relevant when F is called
or when the flag A has a certain value."))

(defun show-relevance-paths (func-or-flag flag)
  (choose-relevancy-data)
  (when (hash-table-p *flag-paths-hash*)
    (let* ((paths (gethash (symbol-name flag) *flag-paths-hash*))
	   (rpaths (mapcar #'reverse paths))
	   (ffname (symbol-name func-or-flag))
	   (is-flag (member func-or-flag global-flaglist))
	   (subpaths nil))
      (dolist (p rpaths)
	      (do ((subp p (cdr subp))
		   (condp nil (not condp))
		   (last-func nil)
		   (done nil))
		  ((or (null subp) done))
		  (when (and condp is-flag)
		    (when (condition-depends-on-flag ffname (car subp))
		      (push (append (cons last-func subp) (list flag)) subpaths)
		      (setq done t)))
		  (unless condp
		    (setq last-func (car subp))
		    (when (equal ffname (symbol-name (car subp)))
		      (push (append subp (list flag)) subpaths)
		      (setq done t)))))
      (dolist (subp0 subpaths)
	      (msgf "-------")
	      (do ((subp subp0 (cdr subp))
		   (condp nil (not condp))
		   (firstp t nil))
		  ((null subp))
		  (if condp
		      (pprint (car subp))
		    (if firstp
			(msgf (car subp) t)
		      (msgf " -> " (car subp) t)))))
      (msgf "-------"))))
      
