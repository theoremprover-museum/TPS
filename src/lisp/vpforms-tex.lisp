;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of VPFORMS)

(defflag texformat
  (flagtype symbol)
  (default vpp)
  (subjects jforms)
  (mhelp "HPD for a horizontal path diagram (p.d.) of the positive wff.
VPD for a vertical p.d. of the negated wff.
VPP (or anything else) for a vertical p.d. of the positive wff."))

(defflag vpform-tex-nest
  (flagtype integer+)
  (default 4)
  (subjects jforms)
  (mhelp "Maximal number of boxes to nest in path diagrams for TeX.
0 means not to break into boxes."))

(defflag vpform-tex-preamble
  (flagtype string)
  (default "")
  (subjects jforms)
  (mhelp "The string to be put at the beginning of a TeX file containing
vpforms."))

(defflag vpform-tex-magnification
  (flagtype integer+)
  (default 1000)
  (subjects jforms)
  (mhelp "The magnification factor to use for TeX files containing vpforms.
This has two possible settings: if it is lower than 10, then it is used
in the form \\magnification=\\magstepN
Roughly, 0 = 10pt, 1 = 12pt, 2 = 14pt, 3 = 17pt, 5 = 25pt.

Otherwise, it is used in the form \\magnificationN, in which case 
1000 corresponds to \"normal size\" (12pt), 800 is 80%, 1200 is 120%, and
so on."))

(deffile vpforms-tex
  (part-of vpforms)
  (extension clisp)
  (mhelp "Printing vertical path diagram commands to be processes by TeX. "))

(context jforms1)

(defun vertical-p (flavor)
  (or (and (eq texformat 'vpd) (eq flavor 'disjunction))
      (and (eq texformat 'vpp) (eq flavor 'disjunction))  
      (and (eq texformat 'hpd) (eq flavor 'conjunction))))


;;; next function is needed because TeX will frequently exeed its recursive
;;; stack size on vpforms.  It returns a sorted list of jforms which
;;; have to be made into boxes.

(defun vpform-tex-break (jform depth)
  (if (= depth vpform-tex-nest)
      (cons jform (vpform-tex-break jform 1))
      (case (jform-type jform)
	(literal nil)
	(disjunction 
	 (mapcan #'(lambda (jf) (vpform-tex-break jf (1+ depth)))
		 (disjunction-components jform)))
	(conjunction
	  (mapcan #'(lambda (jf) (vpform-tex-break jf (1+ depth)))
		  (conjunction-components jform)))
	((universal existential)
	 (vpform-tex-break (universal-scope jform) (1+ depth))))))

(defun vpform-tex-boxes (boxlist &optional(labels nil))
  (declare (special brief printlegend litlist))
  (do ((boxes (nreverse boxlist) (cdr boxes))
       (namelist nil (acons (car boxes) i namelist))
       (i 1 (1+ i)))
      ((null boxes) (format t "~%") namelist)
    ;;(format t "~&\\vpbox~D={~%" i)
    (format t "~&\\vpbox{~D}={~%" i)
    (vpform-tex (car boxes) namelist labels)
    (format t "~&}")))

(defun vpform-princ-lit (lit)
  (msg "{")
  (let* ((lit* (string lit))
	 (alpha-length 
          (1+ (position-if-not
               #'(lambda (x)
                   (member (string x) '("0" "1" "2" "3" "4"
                                        "5" "6" "7" "8" "9" ".")
                           :test #'string=))
               lit* :from-end t)))
          (counter (subseq lit* alpha-length (length lit*))))
;    (msg (subseq lit* 0 alpha-length) "_{" counter "}"))
    (msg "{" (subseq lit* 0 alpha-length) counter "}"))
  (msg "}"))


(defun vpform-tex (jform namelist &optional(labels nil))
  (declare (special brief printlegend litlist))
  (if (assoc jform namelist)
      (format t "\\ibox{~D} " (cdr (assoc jform namelist)))
      (case (jform-type jform)
	(literal
         (case vpd-brief
           ((nil lt)
            (princ "\\matrix{")
            (vpform-princ-lit (show-other-name (literal-name jform)))
            (princ "\\cr")
            (if (jform-pos jform) (prtwff (jform-represents jform))
                (prtwff (cons 'not (jform-represents jform))))
            (princ "\\cr}"))
           (a (if labels 
		  (msg "\\p[" (parse-boxname (show-other-name (literal-name jform))) "]") 
		(princ "\\p "))
              (if (jform-pos jform) (prtwff (jform-represents jform))
		(prtwff (cons 'not (jform-represents jform)))))
           (t (if labels
		  (msg "\\p[" (parse-boxname (show-other-name (literal-name jform))) "]" )
		(if (jform-pos jform)
		    (vpform-princ-lit (show-other-name (literal-name jform)))
		  (progn
		    (msg "\\p \\lnot ")
		    (vpform-princ-lit (show-other-name (literal-name jform))))))
	      (when printlegend (push jform litlist)))))
	(disjunction
	 (if (disjunction-components jform)
	     (progn (if (or labels (vertical-p 'disjunction)
			    allscopeflag)
			(princ "\\vpdor{") ;with []
		      (princ "\\vpdorx{")) ;without []
		    (do ((list (disjunction-components jform) (cdr list)))
			((null list))
		      (vpform-tex (car list) namelist labels)
		      (if (cdr list) (princ "\\cr \\v ") (princ "\\cr
}"))))
	   (princ "{\\ifmmode\\lbottom\\else$\\lbottom$\\fi}")))
	(conjunction
	 (if (conjunction-components jform)
	     (progn 
	       (if (or labels (vertical-p 'conjunction) 
		       allscopeflag)
		   (princ "\\vpdand{") ;with []
		 (princ "\\vpdandx{")) ;without []
	       (do ((list (conjunction-components jform) (cdr list)))
		   ((null list))
		 (vpform-tex (car list) namelist labels)
		 (if (cdr list) (princ "\\cr \\& ") (princ "\\cr }"))))
	   (princ "{\\ifmmode\\truth\\else$\\truth$\\fi}")))
	(t
	  (cond ((eq (jform-type jform) 'universal) (princ "\\vpdforall"))
		((eq (jform-type jform) 'existential) (princ "\\vpdexists"))
		(T (throwfail "ERROR: " jform " is not of type JFORM.")))
	  (if (or (and (not allscopeflag) (vertical-p (type-of (universal-scope jform))))
		  (and allscopeflag (not (equal 'literal (jform-type (universal-scope jform))))))
	      (princ "x{")
	      (princ "{"))
	  (dolist (qvar (universal-qvars jform))
	    (princ " \\q ")
	    (prtwff qvar))
	  (princ "}")
	  (princ "{")
	  (vpform-tex (universal-scope jform) namelist labels)
	  (princ "}")))
	))


(defun vpform-tex-printlegend (litlist &optional (labels nil))
  (setq litlist
	(sort (delete-duplicates litlist :test #'eq) #'(lambda (x y) (or (< (length x) (length y))
						(and (= (length x) (length y))
						     (string< x y))))
	      :key #'(lambda (x) (princ-to-string (show-other-name (literal-name x))))))
  (msg -2 "$\\matrix{{\\rm Literal} & {\\rm Represents}\\hfill \\cr")
  (dolist (lit litlist)
    (msg t)
    (if labels (msg "\\fb[" (parse-boxname (show-other-name (literal-name lit))) "]")
      (vpform-princ-lit (show-other-name (literal-name lit))))
    (msg "\\hfill &")
    (if labels
	(if (jform-pos lit) (prtwff (jform-represents lit)) (progn (msg "\\sim . ") (prtwff (jform-represents lit))))
      (prtwff (jform-represents lit)))
    (msg "\\hfill \\cr"))
  (msg "}$"))

(defun print-vpform-tex (jform vpd-file)
  (declare (special latex-preamble))
  (if (or (string= vpd-brief 'B) (string= vpd-brief 'BT))
      (print-vpform-tex-labels jform vpd-file)
  (reroute-output-append vpd-file (make-pathname% :name "vpd" :type "tex")
    (let ((style 'tex)
	  (in-tex-math-mode t)
	  (displaywff nil)
	  (leftmargin 0))
      (if existent-file
	  (if latex-emulation (msg "ake") (msg "fake"))			; to nullify the \end
	(if latex-emulation
	    (msgf latex-preamble)
	  (progn 
	    (if (< vpform-tex-magnification 10) 
		(format t "\\magnification=\\magstep~D" vpform-tex-magnification)
	      (format t "\\magnification~D" vpform-tex-magnification))
	    (msgf vpform-tex-preamble))))
      (terpri)
      (tex-preamble-fn)
      (terpri)
      (let ((brief (not (eq vpd-brief 'A)))
	    (printlegend (memq vpd-brief '(LT L))) 
            ;;(not (memq vpd-brief '(T A))) 
	    (litlist nil)
	    (printtypes vpd-ptypes))
	(declare (special brief printlegend litlist))
	(let* ((boxlist (vpform-tex-break jform 1))
	       (namelist (vpform-tex-boxes boxlist)))
	  (vpform-tex jform namelist))
	(terpri)
	(tex-postamble-fn)
	(when printlegend
	  (vpform-tex-printlegend litlist)))
      (if latex-emulation (msgf t t "\\eject\\endf") (msgf t t "\\eject\\end"))
      ;; It is important that no TERPRI follow here , because of `fake' above.
      ;; latex-preamble defines \endf to be \end{document}
      ))))

(defun print-vpform-tex-labels (jform vpd-file)
  (declare (special latex-preamble active-mating))
  (reroute-output-append vpd-file (make-pathname% :name "vpd" :type "tex")
    (let ((style 'tex)
	  (in-tex-math-mode t)
	  (vpd-brief (if (string= vpd-brief 'B) 'A 'L))
	  (displaywff nil)
	  (leftmargin 0))
      (if existent-file
	  (if latex-emulation (msg "ake") (msg "fake"))			; to nullify the \end
	(if latex-emulation
	    (msgf latex-preamble)
	  (progn 
	    (if (< vpform-tex-magnification 10) 
		(format t "\\magnification=\\magstep~D" vpform-tex-magnification)
	      (format t "\\magnification~D" vpform-tex-magnification))
	    (msgf vpform-tex-preamble))))
      (terpri)
      (let ((texformat (case texformat ('hpd 'hpdlc) ('vpd 'vpdlc) (t 'vpplc)))) (tex-preamble-fn))
      (terpri)
      (let ((brief (eq vpd-brief 'L))
	    (printlegend (eq vpd-brief 'L)) 
	    (litlist nil)
	    (printtypes vpd-ptypes))
	(declare (special brief printlegend litlist))
	(let* ((boxlist (vpform-tex-break jform 1))
	       (namelist (vpform-tex-boxes boxlist t)))
	  (vpform-tex jform namelist t))
	(terpri)
	(let ((texformat (case texformat ('hpd 'hpdlc) ('vpd 'vpdlc) (t 'vpplc)))) (tex-postamble-fn))
	(when printlegend
	  (vpform-tex-printlegend litlist t))
	(when active-mating
	      (msgf t "\\par\\vskip 36pt " t "Active mating: ")
	      (if (consp (car (mating-clist active-mating)))
		  (dolist (l (mating-clist active-mating))
			  (let ((l1 (if (literal-p (car l)) (literal-name (car l)) (car l)))
				(l2 (if (literal-p (cdr l)) (literal-name (cdr l)) (cdr l))))
			    (msgf "\\conn{" (parse-boxname (show-other-name l1)) "}{" 
				  (parse-boxname (show-other-name l2)) "} ")))
		(dolist (connection (mating-clist active-mating))
			(let* ((conn (car (gethash connection (connections-array))))
			       (l1 (if (literal-p (car conn)) (literal-name (car conn)) (car conn)))
			       (l2 (if (literal-p (cdr conn)) (literal-name (cdr conn)) (cdr conn))))
			  (msgf "\\conn{" (parse-boxname (show-other-name l1)) "}{"
				(parse-boxname (show-other-name l2)) "} "))))
	      (if (mating-completep active-mating)
		  (msg t "is complete."))
	      (msgf "\\vfill ")))
      (if latex-emulation (msgf t t "\\eject\\endf") (msgf t t "\\eject\\end"))
      ;; It is important that no TERPRI follow here , because of `fake' above.
      ;; latex-preamble defines \endf to be \end{document}
      )))

(defun tex-preamble-fn ()
  (format t "~[\\beginvpd~;\\beginhpd~;\\beginvpp~;\\beginvpplc~;\\beginhpdlc~;\\beginvpdlc~]"
	  (case texformat (vpd 0) (hpd 1) (vpp 2) (vpplc 3) (hpdlc 4) (vpdlc 5) (T 2))))

(defun tex-postamble-fn ()
  (format t "~[\\endvpd~;\\endhpd~;\\endvpp~;\\endvpplc~;\\endhpdlc~;\\endvpdlc~]"
	  (case texformat (vpd 0) (hpd 1) (vpp 2) (vpplc 3) (hpdlc 4) (vpdlc 5) (T 2))))


(defun vp-tex (jform filespec)
  (print-vpform-tex (ck-and-convert-gwff-to-jform jform) filespec))

(defwffop vp-tex
  (argtypes jform filespec)
  (argnames jform file)
  (arghelp "JForm" "The file to print into")
  (defaultfns (lambda (jform filespec)
		(list jform (if (eq filespec '$)
				(namestring (make-pathname% :name "vpd"
							   :type "tex"))
				filespec))))
  (resulttype ignore)
  (mhelp "Prints the path diagram, in a format understood by TeX, for a JForm 
or a GWFF. At present, it chops off whatever will not fit on one page.
The following flags affect the output:
    1. VPD-BRIEF controls whether labels or wffs are printed.
    2. VPD-PTYPES controls whether types are printed.
    3. TEXFORMAT controls whether the vertical or horizontal path diagram is
printed.
    4. ALLSCOPEFLAG controls where square brackets are printed."))

(defedop vpt
  (alias vp-tex)
  (result-> ignore)
  (edwff-argname jform))

(defun %vpform-tex (jform &rest garbage)
  (declare (ignore garbage))
  (let ((jform (ck-and-convert-gwff-to-jform jform)))
    (let ((style 'tex)
	  (in-tex-math-mode t)
	  (displaywff nil)
	  (leftmargin 0))
      (msg t t t) ;more blank space won't hurt..
      (terpri)
      (tex-preamble-fn)
      (terpri)
      (let ((brief (not (eq vpd-brief 'A)))
	    (printlegend (memq vpd-brief '(LT L))) 
            ;;(not (memq vpd-brief '(T A))) 
	    (litlist nil)
	    (printtypes vpd-ptypes))
	(declare (special brief printlegend litlist))
	(let* ((boxlist (vpform-tex-break jform 1))
	       (namelist (vpform-tex-boxes boxlist)))
	  (vpform-tex jform namelist))
	(terpri)
	(tex-postamble-fn)
	(when printlegend
	  (vpform-tex-printlegend litlist))
	(msg t t)))))
      ; don't print end because this is mostly for script files... MB 4/94(msg -2 "\\end" t))))

(defun parse-boxname (symb)
  (declare (special leaf-name))
  (string-left-trim (princ-to-string leaf-name) (princ-to-string symb)))
