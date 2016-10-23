;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto) 

(part-of vpforms) 

(deffile vpforms (part-of vpforms) (extension clisp)
 (mhelp "Printing vertical path diagram commands.")) 

(context jforms1) 

(defwffop vpform
 (argtypes jform filespec vpstyle boolean vpformat posinteger string)
 (argnames jform file style ptypes brief vpfpage comment)
 (arghelp "JForm" "The file to print into" "The style of the file"
  "T = print types" "" "Length of a line in the file" "")
 (defaultfns vpformdefaults) (resulttype ignore)
 (mhelp "Prints the vertical path diagram for a JForm or a GWFF.")) 

(defedop vpf (alias vpform) (result-> ignore) (edwff-argname jform)) 

(defwffop display-vp-diag (argtypes jform) (argnames jform)
 (arghelp "JFORM") (resulttype ignore)
 (mhelp
  "Use this operation for displaying vertical path diagram on the
terminal with default settings. For complete control over the defaults
use edop VPF.")) 

(defwffop display-vp-diag-ed (argtypes anything) (argnames jform)
 (arghelp "JFORM") (resulttype ignore)
 (mhelp
  "Prints a vertical path diagram. This is like VP in the MATE 
top level, but will use the current edwff to create a jform 
if none is currently available."))

(defun display-vp-diag-ed (jform)
  (if jform (display-vp-diag jform)
    (display-vp-diag (gwff-to-jform edwff))))

(defedop vp (alias display-vp-diag-ed) (result-> ignore)
 (edwff-argname jform)) 

(defwffop display-vpd (argtypes jform) (argnames jform)
 (arghelp "JFORM") (resulttype ignore)
 (mhelp
  "Use this operation for saving VP diagrams in a file. You may want
to change the values of the variables VPD-FILENAME, VPD-STYLE, VPD-PTYPES,
VPD-BRIEF, VPD-VPFPAGE.")) 

(defedop vpd (alias display-vpd) (result-> ignore)
 (edwff-argname jform)) 

(defvar forallindent 1) 

(defvar andheight 1) 

(defun display-vpd (gwff)
  (declare (special scribe-preamble))
 (let ((temp-file (make-pathname% :name (new-filename) :type "vpf"))
       (save-vpf-file
	(merge-pathnames vpd-filename (make-pathname% :type "vpf")))
       existent-file-flag)
   (handler-case ; cebrown 10/1/02
    (when (not (%vpform gwff temp-file vpd-style vpd-ptypes vpd-brief
			vpd-vpfpage "" nil))
      (setq existent-file-flag (probe-file save-vpf-file))
      (if (or existent-file-flag
	      (memq vpd-style '(scribe-slides scribe)))
	  (progn
	    (reroute-output-append save-vpf-file *default-pathname-defaults*
				   (with-open-file (old-file temp-file :direction :input)
					;was :element-type :default, but cmucl barfs on this. MB 4/94
						   (fresh-line)
						   (if (memq vpd-style '(scribe-slides scribe))
						       (if existent-file-flag 
							   (msg "@newpage" t "@begin(verbatim)" t)
							 (if (eq vpd-style 'scribe) (msg scribe-preamble t "@begin(verbatim)" t) 
							   (msg slides-preamble t "@begin(verbatim)" t)))
						     (write-char #\page))
						   (terpri) 
						   (stringdt)
						   (terpri)
						   (do ((char (read-char old-file nil 'eof)
							      (read-char old-file nil 'eof)))
						       ((eq char 'eof) (if (memq vpd-style '(scribe scribe-slides)) (msg t "@end(verbatim)" t))) 
						     (write-char char))))
	    (delete-file temp-file))
	(progn 
	  (rename-file temp-file save-vpf-file)
	  (if reroute-close-message
	      (ttymsg f "File " (save-vpf-file . filespec) " created.")))))
    (error (condition)
	   (throwfail "display-vpd failed." t condition)))))

(defun display-vp-diag (gwff)
  (progn
    (vpwin-update-one gwff)
    (when
     (apply #'vpform (vpformdefaults gwff '$ '$ '$ '$ '$ '$))
     (msg f "Trying again with BRIEF = L ...")
     (apply #'vpform (vpformdefaults gwff '$ '$ '$ 'l '$ '$)))
    (msg "Number of vpaths: " (number-of-vertical-paths gwff) t)))

;; Modified so that TRUTH and FALSEHOOD get printed as literals rather
;; than empty conjunction and disjunction ..  9/7/87 DAN

(defun describe-vpform (jform)
 (declare
  (special brief litlist flatforall flatexists flatand flator flatnot
   legend-flag barwidth))
 (case (jform-type jform) (literal (describe-vplit jform))
  (disjunction
   (if (disjunction-components jform)
    (let
     ((hght 0) (subparts (list 0))
      (newff
       (cons 'or
        (mapcar (function describe-vpform)
         (disjunction-components jform)))))
     (dolist (disj (cdr newff)) (setq hght (max hght (height disj)))
      (setq subparts
       (cons (+ (car subparts) (width disj) (cdr flator)) subparts)))
     (list newff hght (- (car subparts) (cdr flator))
      (nreverse (cdr subparts))))
    (describe-vplit jform)))
  (conjunction
   (if (conjunction-components jform)
    (let
     ((wdth 0) (subparts (list 0))
      (newff
       (cons 'and
        (mapcar (function describe-vpform)
         (conjunction-components jform)))))
     (dolist (conj (cdr newff)) (setq wdth (max wdth (width conj)))
      (setq subparts
       (cons (+ (car subparts) (height conj) andheight) subparts)))
     (list newff (- (car subparts) andheight)
      (+ barwidth wdth barwidth) (nreverse (cdr subparts))))
    (describe-vplit jform)))
  ((universal existential)
   (if (universal-qvars jform)
       (let
           ((newff
             (acons
              (if (eq (jform-type jform) 'universal) 'forall
                  'exists)
              (universal-qvars jform)
              (describe-vpform (universal-scope jform))))
            (flatbinder
             (case (jform-type jform) (universal flatforall)
               (existential flatexists)))
            (flatvars (mapcar (function flatwff) (universal-qvars jform)))
            (flatquant nil))
         (setq flatvars
               (cons (mapcan (function caar) flatvars)
                     (apply-fn-with-key (function +) flatvars (function cdr))))
         (setq flatquant
               (cons (append (car flatbinder) (car flatvars))
                     (+ (cdr flatbinder) (cdr flatvars))))
         (list newff (1+ (height (cdr newff)))
               (max
                (if (printbarsp (cdr newff))
                    (+ forallindent barwidth (width (cdr newff)) barwidth)
                    (+ forallindent (width (cdr newff))))
                (cdr flatquant))
               (cdr flatquant) (car flatquant)))
       (describe-vpform (universal-scope jform))))
  (t (throwfail jform " is not a jform.")))) 

;;; Changed so that neg-literals will be printed with their own
;;; litnames rather than the litname of their component
;;; 9/18/87 DAN

(defun describe-vplit (litname)
 (declare (special flatnot brief litlist legend-flag))
 (let*
  ((flatvalue nil)
   (litname1 (show-prop-name litname))
   (flatlit
    ;;Changed to allow for the possibility that an empty disjunction
    ;;or conjunction is being described -- DAN 3-25-88
    (cond ((eq brief 'a) nil)
     ((literal-p litname) (flatstring (string litname1)))
     (t (flatstring "")))))
  (if legend-flag (push litname litlist))
  (if (eq brief t)
   (if (and (stringp (caar flatlit)) (null (cdar flatlit)))
    (do ((i 1 (1+ i)) (str (caar flatlit)))
     ((digitnamp (char str i)) (setq flatvalue (list (subseq str i)))
      (setq flatlit (cons flatvalue (- (cdr flatlit) i)))))))
  (cond
   ((eq brief 'a)
    (setq flatvalue (flatwff (jform-to-gwff litname)))
    (list litname1 1 (cdr flatvalue) (list (cdr flatvalue))
     (list flatvalue)))
   (brief
    (list litname1 1 (cdr flatlit) (list (cdr flatlit))
     (list (car flatlit))))
   (t (setq flatvalue (flatwff (jform-to-gwff litname)))
    (list litname1 2 (max (cdr flatlit) (cdr flatvalue))
     (list (cdr flatlit) (cdr flatvalue))
     (list (car flatlit) flatvalue)))))) 

(defun digitnamp (c)
 (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))) 

(defun height (vpd) (cadr vpd)) 

(defun %%princ (char &optional (incr 1))
 (declare (special current-file poe vpffiles vpfpage))
 (incf poe incr)
 (if vpffiles
  (progn
   (multiple-value-bind (quo rem) (floor poe vpfpage)
    (if (and (zerop rem) (plusp quo))
     (terpri (nth (1- quo) vpffiles)))
    (setq current-file (nth quo vpffiles)))
   (if (consp char) (write-char (code-char (car char)) current-file)
    (princ char current-file)))
  (if (consp char) (tyo (car char)) (princ char)))) 

(defun print-vp-or-sign (line hght)
 (declare (special flator current-file poe vpffiles vpfpage))
 (if (= line (ceiling (1+ hght) 2)) (%printgencharlist (car flator))
  (%spaces (cdr flator)))) 

(defun print-vpform (vpd line)
 (declare
  (special flator barsign barwidth leftceiling leftfloor rightceiling
   rightfloor bigbar current-file poe vpffiles brief vpfpage))
 (if (atom (car vpd)) (print-vplit vpd line)
  (case (caar vpd)
   (or
    (do ((args (cdar vpd) (cdr args))) ((null args))
     (print-vpform (car args)
      (- line (floor (- (height vpd) (height (car args))) 2)))
     (if (cdr args) (print-vp-or-sign line (height vpd)))))
   (and
    (if (or (not (> line 0)) (> line (height vpd)))
     (%spaces (width vpd))
     (progn (printleftbar line (height vpd))
      (do
       ((args (cdar vpd) (cdr args))
        (wdth (- (width vpd) (+ barwidth barwidth))) (partline 0))
       ((null args)
        (throwfail "Illegal width information in VPFORM."))
       (cond
        ((> (+ partline (height (car args)) 1) line)
         (let ((partwidth (- wdth (width (car args)))))
          (%spaces (floor partwidth 2))
          (print-vpform (car args) (- line partline))
          (return (%spaces (- partwidth (floor partwidth 2))))))
        ((>
          (+
           (setq partline (+ partline (height (car args)) andheight))
           1)
          line)
         (return (%spaces wdth)))))
      (printrightbar line (height vpd)))))
   (t
    (if (memq (caaar vpd) '(forall exists))
     (cond
      ((= line 1) (%printgencharlist (car (cddddr vpd)))
       (%spaces (- (width vpd) (subparts vpd))))
      ((or (< line 1) (> line (height vpd)))
       (cond
        ((printbarsp (cdar vpd)) (%spaces forallindent)
         (%spaces barwidth) (print-vpform (cdar vpd) (1- line))
         (%spaces barwidth)
         (%spaces
          (- (width vpd) forallindent barwidth (width (cdar vpd))
           barwidth)))
        (t (%spaces forallindent) (print-vpform (cdar vpd) (1- line))
         (%spaces (- (width vpd) forallindent (width (cdar vpd)))))))
      ((printbarsp (cdar vpd)) (%spaces forallindent)
       (printleftbar (1- line) (1- (height vpd)))
       (print-vpform (cdar vpd) (1- line))
       (printrightbar (1- line) (1- (height vpd)))
       (%spaces
        (- (width vpd) forallindent barwidth (width (cdar vpd))
         barwidth)))
      (t (%spaces forallindent) (print-vpform (cdar vpd) (1- line))
       (%spaces (- (width vpd) forallindent (width (cdar vpd))))))
     (throwfail vpd " is not a VPFORM.")))))) 

(defun print-vplit (vpd line)
 (declare (special vpfpage brief current-file poe vpffiles))
 (prog (partwidth)
  (cond ((or (< line 1) (> line (height vpd))) (%spaces (width vpd)))
   ((eq brief 'a) (%sprintpplist (caar (cddddr vpd))))
   (brief (%printgencharlist (caar (cddddr vpd))))
   ((= line 1) (setq partwidth (- (width vpd) (car (subparts vpd))))
    (%spaces (floor partwidth 2))
    (%printgencharlist (caar (cddddr vpd)))
    (%spaces (- partwidth (floor partwidth 2))))
   ((= line 2) (setq partwidth (- (width vpd) (cadr (subparts vpd))))
    (%spaces (floor partwidth 2))
    (%sprintpplist (cadar (cddddr vpd)))
    (%spaces (- partwidth (floor partwidth 2))))))) 

(defun printbarsp (desc-vpf)
 (not (or (atom (car desc-vpf)) (eq (caar desc-vpf) 'and)))) 

(defun %printgencharlist (gcharlist)
 (declare (special current-file poe vpffiles vpfpage))
 (dolist (gchar gcharlist)
  (if (and (consp gchar) (consp (car gchar)))
   (dolist (char (car gchar))
    (if (consp char) (tyo (car char)) (princ char)))
   (%%princ gchar)))) 

(defun width (vpd) (caddr vpd)) 

(defun ck-and-convert-gwff-to-jform (jform)
 (if (and (fboundp 'etree-p) (etree-p jform))
  (etree-to-jform jform)
  (if (jform-p jform) jform
   (let ((lit-name vpd-lit-name)) (declare (special lit-name))
    (reset-name-counter lit-name) (gwff-to-jform jform))))) 

(defun %vpform  (jform vpd-file style printtypes brief vpfpage comment msg-flag)
  (if (eq style 'tex) (%vpform-tex jform vpd-file style printtypes brief vpfpage comment msg-flag)
    (%vpform-real jform vpd-file style printtypes brief vpfpage comment msg-flag)))

(defun %vpform-real (jform vpd-file style printtypes brief vpfpage comment msg-flag)
 (declare (special brief vpfpage core::kset))
 (let ((style (if (eq style 'scribe-slides) 'scribe style))
       (core::kset 0)) ;this is a hack to get concept-s fonts working again... MB 4/94
   (declare (special core::kset))
 (setq jform (ck-and-convert-gwff-to-jform jform))
 (let
  (;;added following so always get a vpform DAN 9NOV89
   (style
    (if (memq style '(tps-ascii concept concept-s generic xterm sail scribe))
	style 
      (if (eq style 'scribe-slides) 'scribe 'generic)))
   (scr-postamble nil)
   (current-file nil) (abort-flag nil) (litlist nil)
   (legend-flag (if (memq brief '(l lt)) t nil))
   (typesinlegend
    (if (eq brief 'lt) (progn (setq brief nil) t) printtypes))
   (flatforall (flatsym 'forall))
   (flatexists (flatsym 'exists))
   (flatand (flatsym 'and)) (flator (flatsym 'or))
   (flatnot (flatsym 'not)) (flatbar '(("|") . 1))
   (vpffile-objects nil) (vpffiles nil) vpd num-of-lines num-of-files
   wdth barwidth barsign leftceiling rightceiling leftfloor
   rightfloor bigbar)
  (declare
   (special litlist flatforall flatexists flatand flator flatnot
    legend-flag barwidth barsign leftceiling leftfloor rightceiling
    rightfloor bigbar current-file poe vpffiles))
  (if (not (terminalp vpd-file))
   (setq vpd-file
    (merge-pathnames vpd-file (make-pathname% :type "vpf"))))
  (setq barwidth (cdr flatbar)) (setq barsign (car flatbar))
  (if (or (eq style 'concept-s) (eq style 'xterm))
   (mapc (function (lambda (flatx x) (set flatx (car (flatsym x)))))
    '(leftceiling leftfloor rightceiling rightfloor bigbar)
    '(ceiling1 floor1 ceiling2 floor2 bigbar)))
  (setq vpd (describe-vpform jform)) (setq wdth (width vpd))
  (setq num-of-lines (height vpd))
  (multiple-value-bind (quo rem) (floor wdth vpfpage)
   (setq num-of-files (if (zerop rem) quo (1+ quo))))
  (cond
   ((or (not (terminalp vpd-file)) (> num-of-files 1))
    (msg f "The vertical path diagram is " num-of-files " page")
    (if (> num-of-files 1) (msg "s"))
    (msg " wide (" wdth " chars) and " num-of-lines " lines long."
     t)))
  (cond
   ((terminalp vpd-file)
    (when (> num-of-files 1)
     (msg f "The diagram can't fit on the screen."
      " Command aborted!")
     (setq abort-flag t)))
   ((= num-of-files 1)
    (setq vpffiles
     (list
      (open vpd-file :direction :output   ; removed because cmucl barfs MB 4/94 :element-type :default
       :if-does-not-exist :create)))
    (if (and scr-postamble msg-flag)
     (princ slides-preamble (car vpffiles)))
    (when (not (string= comment "")) (princ comment (car vpffiles))
     (terpri (car vpffiles))))
   (t
    (do
     ((counter 1 (1+ counter)) (page-no num-of-files (1- page-no)))
     ((= counter num-of-files)
      (push
       (open vpd-file :direction :output ; removed because cmucl barfs MB 4/94 :element-type :default
        :if-does-not-exist :create)
       vpffiles)
      (cond
       (scr-postamble
        (if msg-flag (princ slides-preamble (car vpffiles))))
       (t (princ "                          Page " (car vpffiles))
        (princ page-no (car vpffiles)) (terpri (car vpffiles)))))
     (push (make-pathname% :name (new-filename) :type "vpf")
      vpffile-objects)
     (push
      (open (car vpffile-objects) :direction :output ; removed for CMUCL :element-type :default 
       :if-does-not-exist :create)
      vpffiles)
     (when (not scr-postamble)
      (princ "                          Page " (car vpffiles))
      (princ page-no (car vpffiles)) (terpri (car vpffiles))))
    (when (not (string= comment "")) (setq poe -1)
     (do
      ((counter 0 (1+ counter)) (length (1- (length comment)))
       (comment comment) (new-line (* num-of-files vpfpage)))
      ((> counter length)
       (when (plusp (rem poe vpfpage)) (terpri current-file))
       (mapc (function terpri) (cdr (memq current-file vpffiles))))
      (%%princ (char comment counter))
      (when (= poe new-line) (setq poe -1))))))
  (if (and scr-postamble vpffiles)
   (dolist (filename vpffiles) (princ "@BEGIN(verbatim)" filename)
    (terpri filename)))
  (if (and msg-flag (not (terminalp vpd-file)))
   (msg f "Creating file " (vpd-file . filespec) "  ..."))
  (when (not abort-flag)
   (do ((line-num 0 (1+ line-num)) (num-of-lines num-of-lines))
    ((> line-num num-of-lines)
     (when (not (terminalp vpd-file))
      (mapc
       (function
        (lambda (filename)
          (terpri filename)
          (when scr-postamble (princ "@END(verbatim)" filename)
           (terpri filename))
          (close filename)))
       vpffiles)))
    (setq poe -1) (print-vpform vpd line-num)
    (if current-file
     (progn (if (> (rem poe vpfpage) 0) (terpri current-file))
      (if scr-postamble
       (mapc
        (function
         (lambda (filename) (princ " " filename) (terpri filename)))
        (cdr (memq current-file vpffiles)))
       (mapc (function terpri) (cdr (memq current-file vpffiles)))))
     (terpri)))
   (if legend-flag
    (if vpffiles
     (reroute-output-append vpd-file *default-pathname-defaults*
      (printlegend (nreverse litlist) vpfpage typesinlegend))
     (printlegend (nreverse litlist) vpfpage typesinlegend)))
   (cond ((terminalp vpd-file)) ((= num-of-files 1))
    (t
     (do
      ((vpffiles vpffile-objects (cdr vpffiles))
       (file-name
        (open vpd-file :direction :output :if-exists :append)))
;removed for CMUCL         :element-type :default)))
      ((null vpffiles) (close file-name)) (terpri file-name)
      (if scr-postamble (princ "@newpage" file-name)
       (write-char #\page file-name))
      (terpri file-name)
      (let
       ((old-file
         (open (car vpffiles) :direction :input )))
       (do ((char (read-char old-file nil control-d-char)
		  (read-char old-file nil control-d-char)))
	   ((eq char control-d-char)) 
	   (write-char char file-name))
       (close old-file) (delete-file (car vpffiles))))))
   (if (and msg-flag (not (terminalp vpd-file)))
    (msg t "File " (vpd-file . filespec) " successfully created.")))
  abort-flag)))

(defun printleftbar (line height)
 (declare (special vpfpage barsign leftceiling leftfloor bigbar))
 (if (or (eq style 'xterm) (eq style 'concept-s))
  (cond ((= line 1) (%printgencharlist leftceiling))
   ((= line height) (%printgencharlist leftfloor))
   (t (%printgencharlist bigbar)))
  (%printgencharlist barsign))) 


(defun printlegend (litlist linewidth printtypes)
 (declare (special printtypes) (ignore linewidth))
 (terpri)
 (setq litlist (remove-duplicates (remove-if-not #'literal-p litlist)
				  :test #'(lambda (x y) (eq (literal-name x) (literal-name y)))))
 (dolist (lit (sort litlist #'(lambda (x y) (or (< (length x) (length y))
						(and (= (length x) (length y))
						     (string< x y))))
		    :key #'(lambda (x) (princ-to-string (literal-name x)))))
  (msg (show-prop-name lit) 2 "=" 2)
  (let ((hatomalist nil) (core::pc '| |) (curpos 0))
   (declare (special hatomalist core::pc curpos))
   (printwff (jform-to-gwff lit) nil 1))
  (terpri))) 

(defun printrightbar (line height)
 (declare (special vpfpage barsign rightceiling rightfloor bigbar))
 (if (or (eq style 'xterm) (eq style 'concept-s))
  (cond ((= line 1) (%printgencharlist rightceiling))
   ((= line height) (%printgencharlist rightfloor))
   (t (%printgencharlist bigbar)))
  (%printgencharlist barsign))) 

(defun %spaces (n)
 (declare (special current-file poe vpffiles vpfpage))
 (dotimes (i n) (%%princ " "))) 

(defun %sprintaplicn (aplicn)
 (declare (special current-file poe vpffiles vpfpage))
 (cond
  ((genchar aplicn)
   (if (consp aplicn)
    (if (consp (car aplicn))
     (mapc (function sprintaplicn) (car aplicn)) (%%princ aplicn))
    (%%princ aplicn)))
  (t (cond ((caaar aplicn) (%sprintpplist (car aplicn))))
   (cond ((caadr aplicn) (%sprintpplist (cdr aplicn))))))) 

(defun %sprintpplist (pplist)
 (declare (special current-file poe vpffiles vpfpage))
 (case (cddar pplist) (core::brackets (%%princ "["))
  (core::dot (%%princ ".")) (core::space-brackets (%%princ " [" 2))
  (core::space-dot (%%princ " ." 2)))
 (mapc (function %sprintaplicn) (caar pplist))
 (if
  (memq (cddar pplist) '(core::brackets core::space-brackets))
  (%%princ "]"))) 

(defun subparts (vpd) (cadddr vpd)) 

(defun vpform (jform file style ptypes brief vpfpage comment)
 (%vpform jform file style ptypes brief vpfpage comment t)) 

(defun vpformdefaults
  (jform file pstyle ptypes brief vpfpage comment)
  (if (eq file '$) (setq file "TTY:"))
  (if (eq pstyle '$)
      (if (terminalp file) (setq pstyle style)
          (setq pstyle 'generic)))
  (if (eq vpfpage '$)
      (setq vpfpage
            (case pstyle
              ((tps-ascii concept concept-s) 78)
              ((generic xterm tex scribe) (- rightmargin leftmargin 1))
              (sail 120) (scribe-slides 52) (otherwise 78)))) ;used to be 79 for scribe...
  (if (eq brief '$)
      (setq brief (if vpform-labels 'a nil)))
  (if (eq ptypes '$)
      (setq ptypes
            (if
                (and (or (eq pstyle 'xterm) (eq pstyle 'concept-s))
                     (terminalp file))
                printtypes nil)))
  (if (eq comment '$) (setq comment ""))
  (list jform file pstyle ptypes brief vpfpage comment)) 

(defvar *vpwin-opened* nil)
(defvar *vpwin-process* nil)
(defvar *big-vpwin* nil)
(defvar *current-vpwname* nil)

(defflag vpw-width
  (flagtype posinteger)
  (default 120)
  (subjects jforms window-props)
  (mhelp "Contains the current width of the vpform window; should be updated by the 
user if the window is resized after being opened."))

(defflag vpw-height
  (flagtype posinteger)
  (default 25)
  (subjects jforms window-props)
  (mhelp "Contains the intial height of the vpform window; there is no need to update
this if the window is resized after being opened."))

(defmexpr open-matevpw
  (argtypes filespec)
  (argnames filename)
  (arghelp "File to send copy output to (\"\" to discard)")
  (defaultfns
    (lambda (filename) 
      (if (eq filename '$) (setq filename (if (string-equal (princ-to-string dproof) "*****") "vpwin.vpw" (concatenate 'string (string-downcase (princ-to-string dproof)) ".vpw"))))
      (list filename)))
  (mhelp "Open a window which will display the current vpform and substitution stack,
if any. The window can be closed with the command CLOSE-MATEVPW. The size 
of the text is determined by the flag CHARSIZE, and the current width of the window 
by the flag VPW-WIDTH. The initial height of the window is determined by VPW-HEIGHT
Use ..../tps/utilities/vpshow to view the file from the monitor level."))

(defmenuitem OPEN-MATEVPW
  (display-name "OPEN-MATEVPW")
  (placement 200)
  (command "OPEN-MATEVPW \\\"VPWIN\\\"")
  (parent TOP-LEVELS)
  (mhelp ""))

(defmenuitem CLOSE-MATEVPW
  (display-name "CLOSE-MATEVPW")
  (placement 201)
  (command "CLOSE-MATEVPW")
  (parent TOP-LEVELS)
  (mhelp ""))

(defun open-vpwin-auto ()
  (declare (special filename))
  (progn
    (if *using-interface*
	(setq filename "VPWIN")
      (prompt-read filename nil (msgf "File to send copy Vpform output to (\"\" to discard)") 'filespec 
		   (if (string-equal (princ-to-string dproof) "*****") "vpwin.vpw" (concatenate 'string (string-downcase (princ-to-string dproof)) ".vpw")) nil))
    (if (not *vpwin-opened*) (msg t "Use CLOSE-MATEVPW when you want to close the vpwindow." t))
    (open-matevpw filename)))

(defun open-matevpw (&optional (filename ""))
  (declare (special *using-interface*))
  (if *vpwin-opened*
      (if (and *current-vpwname* (not *using-interface*))
	  (msg t "Output is already going to an existing vpwindow and to the file " *current-vpwname* "." t)
	(msg t "A vpwindow is already open, and output is being directed to that. Use CLOSE-MATEVPW if you no longer need it." t))
    (progn
      (if (equal charsize 'MAX)
	  (setq *big-vpwin* T)
	(setq *big-vpwin* nil))
      (setq *vpwin-opened* (setup-vpwin filename))
      (if (string-equal filename "") (setq *current-vpwname* nil))
      (vpwin-update-headers))))

(defmexpr close-matevpw
  (argtypes)
  (argnames)
  (mhelp "Closes the window that displays the current vpform and
substitution stack. Use ..../tps/utilities/vpshow (from a shell, 
not from TPS) to view the output file again."))

(defun close-matevpw ()
  (declare (special *using-interface*))
  (if *vpwin-opened*
      (if *using-interface*
	  (progn
	    (close-window *vpwin-opened*)
	    (setq *vpwin-opened* nil)
	    (setq *current-vpwname* nil))
	(progn
	  (vpwin-update-footers)
	  (close *vpwin-opened*)
	  (core::kill-xterm-window *vpwin-process*)
	  (if *current-vpwname* 
	      (progn
		(msg "Closed vpwindow file : " *current-vpwname*)
		(let (discard)
		  (prompt-read discard nil (msgf "Shall I delete the output file " 
						 *current-vpwname* "? ") 'yesno nil nil)
		  (if discard (delete-file *current-vpwname*))
		  (setq *current-vpwname* nil)))
	    (when (probe-file (pathname *vpwin-opened*))
	      (delete-file (pathname *vpwin-opened*))))
	  (setq *vpwin-opened* nil)))))

(defun setup-vpwin (fname)
  (declare (special *using-interface*))
  (if *using-interface*
      (let* ((outfilename (if (string-equal fname "") "vpwin" fname))
	     (g (open-window-with-socket outfilename "VP Window"
					 vpw-width vpw-height
					 *big-vpwin*)))
	(setq *current-vpwname* outfilename)
	g)
    (let* ((outfilename (if (string-equal fname "")
			    (concatenate 'string "/tmp/foo" (princ-to-string (tps-get-internal-run-time)))
			  (namestring (merge-pathnames fname (make-pathname% :name "vpw" :type "vpw")))))
					;tried, and failed, to use :directory (namestring (pathname (user-homedir-pathname)))
					;it seems cmucl on the RT wants "pathname", and all the pmax lisps want "truename"
	   (outstream (open outfilename :direction :output 
			    :if-exists :supersede
			    :if-does-not-exist :create)))
      (setq *current-vpwname* outfilename)
      (if *big-vpwin*
	  (setq *vpwin-process* (core::setup-big-xterm-window "Vpform & Substitutions" outfilename 
							      (concatenate 'string (princ-to-string vpw-width)
									   "x" (princ-to-string vpw-height) "+0+0")))
	(setq *vpwin-process* (core::setup-xterm-window "Vpform & Substitutions" outfilename 
							(concatenate 'string (princ-to-string vpw-width)
								     "x" (princ-to-string vpw-height) "+0+0"))))
      outstream)))

(defun vpwin-update-headers ()
  (declare (special scribe-preamble latex-preamble tpstex vpdtex))
  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
	(let ((*standard-output* *vpwin-opened*)
	      (style (if use-window-style window-style style))
	      (rightmargin vpw-width))
	    (if (eq style 'scribe) (msg scribe-preamble t))
	    (when (memq style '(tex tex-1))
		  (if latex-emulation (msgf latex-preamble t)
		    (progn 
		      (msgf "\\magnification=\\magstep1")
		      (when (probe-file tpstex) (msgf "\\input " tpstex))
		      (when (probe-file vpdtex) (msgf "\\input " vpdtex)))))
	    (finish-output *standard-output*))))

(defun vpwin-update-footers ()
  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
	(let ((*standard-output* *vpwin-opened*)
	      (style (if use-window-style window-style style))
	      (rightmargin vpw-width))
	    (if (eq style 'tex) (if latex-emulation (msgf "\\end{document}" t) (msg t t "\\end" t)))
	  (finish-output *standard-output*))))

(defun vpwin-update-one (gwff)
  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
	(let ((*standard-output* *vpwin-opened*)
	      (style (if use-window-style window-style style))
	      (rightmargin vpw-width))
	  (if *using-interface*
	      (clear-window *vpwin-opened*)
	    (dotimes (i 7) (terpri)))
	  (when
	   (apply #'vpform (vpformdefaults gwff '$ '$ '$ '$ '$ '$))
	   (msg f "Trying again with BRIEF = L ...")
	   (apply #'vpform (vpformdefaults gwff '$ '$ '$ 'l '$ '$)))
	  (dotimes (i vpw-width) (msg "-"))
	  (finish-output *standard-output*))))

(defun vpwin-update-two (subst-stack)
  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
	(let ((*standard-output* *vpwin-opened*)
	      (style (if use-window-style window-style style))
	      (rightmargin vpw-width))
	  (terpri)
	  (terpri)
	  (msg T "Substitution Stack:" T)
	  (let ((h-vars nil))
	    (dolist (subst (reverse subst-stack))
		    (when (not (memq (car subst) h-vars))
			  (msg t ((car subst) . gwff) 3 "->" 3
			       ((lambda-reduce-subst (cdr subst) subst-stack) . gwff)))
		    (setq subst (cdr subst))
		    (when (subst-p subst)
			  (if (subst-new-h-vars subst)
			      (dolist (sub (subst-new-h-vars subst))
				      (push (cdr sub) h-vars))
			    (dolist (var (subst-h-vars subst))
				    (push var h-vars))))))
	  (finish-output *standard-output*))))

(defun vpwin-update-three (option-set)
  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
	(let ((*standard-output* *vpwin-opened*)
	      (style (if use-window-style window-style style))
	      (rightmargin vpw-width))
	  (terpri)
	  (terpri)
	  (let ((vars (remove-duplicates 
		       (flatten (mapcar #'option-nodes
					(option-set-options option-set)))))
		(subs-exist nil))
	    (msg "Considering option set " (option-set-label option-set) " for the " (option-set-times option-set) (ordinal-print (option-set-times option-set)) "time." t) 
	    (msg "Substitutions: " t)
	    (dolist (var vars)
		    (let ((exp-var (nth (position var (etree-components (etree-parent
									 var)))
					(expansion-terms (etree-parent var)))))
		      (if (not (eq (exp-var-var exp-var) (exp-var-subst exp-var)))
			  (progn
			    (msg ((exp-var-var exp-var) . gwff) "  -->  "
				 ((exp-var-subst exp-var) . gwff) t)
			    (setq subs-exist t)))))
	    (if (not subs-exist) (msg "None." t)))
	  (finish-output *standard-output*))))

(defun vpwin-update-four (tree)
  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
	(let ((*standard-output* *vpwin-opened*)
	      (style (if use-window-style window-style style))
	      (rightmargin vpw-width))
	  (terpri)
	  (terpri)
	  (let ((vars (remove-duplicates (flatten (option-tree-free-vars* tree))))
		(subs-exist nil))
	    (msg "Considering option tree " (option-tree-name tree) " for the " (timetonum (option-tree-tried tree)) (ordinal-print (timetonum (option-tree-tried tree))) "time." t) 
	    (msg "Substitutions: " t)
	    (dolist (var vars)
		    (let ((exp-var (nth (position var (etree-components (etree-parent
									 var)))
					(expansion-terms (etree-parent var)))))
		      (if (not (eq (exp-var-var exp-var) (exp-var-subst exp-var)))
			  (progn
			    (msg ((exp-var-var exp-var) . gwff) "  -->  "
				 ((exp-var-subst exp-var) . gwff) t)
			    (setq subs-exist t)))))
	    (if (not subs-exist) (msg "None." t)))
	  (finish-output *standard-output*))))


(defun vpwin-update-five (mating)
  (declare (ignore mating) (special active-mating master-eproof))
  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
	(let ((*standard-output* *vpwin-opened*)
	      (style (if use-window-style window-style style))
	      (rightmargin vpw-width))
	  (terpri)
	  (terpri)
	  (msg "Final Mating:" t)
	  (if (consp (car (mating-clist active-mating)))
	      (msg t (l (mating-clist active-mating)))
;	    (print-clist (mating-clist active-mating) (connections-array))  
; this is a macro, and it causes a bug. Replaced with the macro definition 7/1/93
; (the compiler assumes connection-array is a function, because it hasn't found
; it yet)
	    (print-clist (mating-clist active-mating) (eproof-connections-array master-eproof)))
	  (finish-output *standard-output*))))


(defun vpwin-update-six (mating)
  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
        (let ((*standard-output* *vpwin-opened*)
	      (style (if use-window-style window-style style))
	      (rightmargin vpw-width))
	  (terpri)
	  (terpri)
	  (msg "Final Mating:" t)
	  (do ((mating mating (cddr mating)))
	      ((null mating) (msg t))
	      (msg t (caar mating) 3 (cadr mating)))
	  (finish-output *standard-output*))))

(defun vpwin-update-seven (one)
  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
	(let ((*standard-output* *vpwin-opened*)
	      (style (if use-window-style window-style style))
	      (rightmargin vpw-width))
	  (terpri)
	  (terpri)
	  (msg t 3 "Primitive Substitution: " (one . gwff))
	  (finish-output *standard-output*))))

