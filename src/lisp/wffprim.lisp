;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFFS)

(deffile wffprim
  (part-of wffs)
  (extension clisp)
  (mhelp "Contains basic stuff for wffs."))

(context prim-obj)

(defun getnameroot (var)
  (if (label-q var)
      (apply-label var (getnameroot var))
      (let ((str (string var)))
        (intern (subseq str 0 (position #\< str :start 1)) *user*))))


(defun nameroot-type (var)
  (if (label-q var)
      (apply-label var (nameroot-type var))
      (let* ((str (string var))
             (n (position #\< str :start 1)))
        (if n (cons (subseq str 0 n) (subseq str n)) (cons str nil)))))

(defun nameroot (var)
  (if (label-q var)
      (apply-label var (nameroot var))
      (let* ((str (string var))
             (n (position #\< str :start 1)))
        (if n (subseq str 0 n) str))))

(defun name-eq (lsym1 lsym2)
  (equal (nameroot lsym1) (nameroot lsym2)))

(defflag ren-var-fn
  (flagtype symbol)
  (default ren-var-x1)
  (subjects wff-prims)
  (mhelp "The value of this flag is a function to be called when a variable
must be renamed automatically. It has three possible settings:
REN-VAR-X1 is the standard renaming function. It renames y to y^1,
 then to y^2, and so on. If there is another variable y, of a different 
 type, it makes no difference.
REN-VAR-X11 is much like REN-VAR-X1, except it will avoid creating
 two variables of the same name at different types (so it tends to 
 produce higher exponents than REN-VAR-X1).
REN-VAR-XA renames alphabetically, turning y into ya, then yba, 
 and so on."))

(definfo ren-var-x1
  (mhelp "A flag setting for REN-VAR-FN.
This is the standard renaming function. It renames y to y^1,
then to y^2, and so on. If there is another variable y, of a different 
type, it makes no difference."))

(definfo ren-var-x11
  (mhelp "A flag setting for REN-VAR-FN.
This is much like REN-VAR-X1, except it will avoid creating
two variables of the same name at different types (so it tends to 
produce higher exponents than REN-VAR-X1)."))

(definfo ren-var-xa
  (mhelp "A flag setting for REN-VAR-FN.
This renames variables alphabetically, turning y into ya, then yba, 
and so on."))

;;If TYPEP is non-nil, it must be a list like (O. (O. I)). 

(defun ren-var-xa (var &optional (new-type nil))
  (if (label-q var)
      (apply-label var (ren-var-xa var new-type))
      (let* ((nr-type (nameroot-type var))
             (typep (if new-type (type-to-string new-type) (cdr nr-type)))
             (nexp (car nr-type))
             (lchar-upcase-p (upper-case-p (char nexp (1- (length nexp)))))
             (maxchar (char-int (if lchar-upcase-p #\Z #\z))))
        (let ((trychar (char-int (if lchar-upcase-p #\A #\a))))
          (do ((trychar (1+ trychar) (1+ trychar))
               (tryname
                (intern (concatenate 'string nexp (string (code-char trychar))
                                     typep))
                (intern (concatenate 'string nexp (string (code-char trychar))
                                     typep))))
              ((not (lsymbol-p tryname))
               (putprop tryname (if new-type new-type (get var 'type)) 'type)
               (putprop tryname (if new-type new-type (get var 'unabbreviated-type))
                        'unabbreviated-type)
               tryname)
            (when (> trychar maxchar)
	      (setq nexp
		    (concatenate 'string nexp (string (code-char maxchar))))
	      (setq trychar (char-int (if lchar-upcase-p #\A #\a)))))))))

;;; want to find the last occurrence of an ^ with only an integer
;;; following it.  Go backwards through string until either not a
;;; digit, or find ^ or hit beginning.  If no ^ found return length of string.

(defun last-^-with-int (str)
  (let ((len (length str))
	(found-digit nil))
    (dotimes (i len len)
      (if (digit-char-p (char str (- len i 1)) 10)
	  (setq found-digit t)
	  (if (and found-digit (char= #\^ (char str (- len i 1))))
	      (return-from last-^-with-int (- len i 1))
	    (return-from last-^-with-int len))))))

;;; Modified the renaming functions so that we can handle superscripts
;;; in variables i.e, when variable to be renamed is of form x^123
;;; DAN 01NOV90

(defun ren-var-x1 (var &optional (new-type nil))
  (if (label-q var)
      (apply-label var (ren-var-x1 var new-type))
      (let* ((nameroot1 (nameroot-type var))
             (nexp (car nameroot1))
             (alpha-length 
	      (1- (last-^-with-int nexp)))
             (nexp* (subseq nexp 0 (1+ alpha-length)))
             (trynum 
	      (if (= (length nexp) (1+ alpha-length)) 
		  -1
		(or (parse-integer nexp :start (+ 2 alpha-length) 
				   :junk-allowed t)
                  -1)))
             (typep (if new-type (type-to-string new-type) (cdr nameroot1))))
        (do* ((trynum (1+ trynum) (1+ trynum))
              (tryname
               (intern-str (format nil "~A^~A~A" nexp* trynum typep))
               (intern-str (format nil "~A^~A~A" nexp* trynum typep))))
            ((not (lsymbol-p tryname))
             (putprop tryname (if new-type new-type (get var 'type)) 'type)
             (putprop tryname
                      (if new-type new-type (get var 'unabbreviated-type))
                      'unabbreviated-type)
             (set-superscript-face 
	      (intern-str (format nil "~A^~A" nexp* trynum)))
             tryname)))))


(defun ren-var-x11 (var &optional (new-type nil))
  (if (label-q var)
      (apply-label var (ren-var-x11 var new-type))
      (let* ((nameroot1 (nameroot-type var))
             (nexp (car nameroot1))
	     (len (length nexp))
             (alpha-length 
	      (1- (last-^-with-int nexp)))
             (typep (if new-type (type-to-string new-type) (cdr nameroot1)))
             (nameroot (intern-str nexp))
             (counter (getren-counter nameroot))) 
        (do ((counter (1+ counter) (1+ counter))
             (tryname (intern-str 
		       (format nil (if (= alpha-length (1- len))
				       "~A^~A~A"
				       "~A~A~A")
			       nexp counter typep))
                      (intern-str
		       (format nil (if (= alpha-length (1- len))
				       "~A^~A~A"
				       "~A~A~A")
			       nexp counter typep))))
            ((not (lsymbol-p tryname))
             (setren-counter nameroot counter)
             (putprop tryname (if new-type new-type (get var 'type)) 'type)
             (putprop tryname
                      (if new-type new-type (get var 'unabbreviated-type))
                      'unabbreviated-type)
	     (set-superscript-face 
	      (intern-str (format nil (if (= alpha-length (1- len))
				       "~A^~A"
				       "~A~A")
				  nexp (1- counter))))
             tryname)))))

(defun digits (counter)
  (multiple-value-bind (quo rem) (floor counter 10)
    (if (< quo 10) (if (plusp quo) (list rem quo) (list rem))
	(cons rem (digits quo)))))

(defun getren-counter (nameroot) (get nameroot 'ren-counter 1))

(defun setren-counter (nameroot value) (putprop nameroot value 'ren-counter))

(defun inherit-abbrev (stands-for insttype insttypelist)
  (let ((instname (create-propsym stands-for insttype)))
    (putprop instname stands-for 'stands-for)
    (putprop instname insttypelist 'polytypelist)
    (putprop instname (get stands-for 'printnotype) 'printnotype)
    (putprop instname (get stands-for 'infix) 'infix)
    (putprop instname (get stands-for 'prefix) 'prefix)
    instname))

(defflag rename-all-bd-vars
  (flagtype boolean)
  (default nil)
  (subjects wff-prims)
  (mhelp "When T, all bound variables inside a definition will be
renamed before instantiation."))

;;; Now checks to see whether bd-vars should be renamed DAN 2-22-88
(defun get-pmdefn (inst)
  (let*	((stands-for (get inst 'stands-for))
	 (wff (substitute-types
		(mapcar #'(lambda (tvar tconst) (cons tvar tconst))
			(get stands-for 'typelist)
			(get inst 'polytypelist))
		(get stands-for 'defn))))
    (if rename-all-bd-vars
	(ab-normalize wff)
      wff)))

(defun get-defn (abb) 
  (let ((defn (get abb 'defn)))
	(if rename-all-bd-vars
	    (ab-normalize defn)
	  defn)))

(defun get-defn-1 (abb) (or (get abb 'defn) (get-pmdefn abb)))

(defun fullname (nameroot tp)
  (intern (conc-strings (string nameroot) (type-to-string tp)) *user*))

(defun type-to-string (tp)
  (with-open-stream (string-stream (make-string-output-stream))
    (princ "<" string-stream)
    (type-to-stream tp string-stream)
    (princ ">" string-stream)
    (get-output-stream-string string-stream)))


;;; Changed prin1 to princ so that types will never have package
;;; qualifiers in them. DAN 15NOV90

(defun type-to-stream (tp stream)
  (if (symbolp tp) (princ tp stream)
      (progn (type-to-stream (car tp) stream)
	     (if (consp (cdr tp))
		 (progn (princ "<" stream)
			(type-to-stream (cdr tp) stream)
			(princ ">" stream))
		 (type-to-stream (cdr tp) stream)))))

;;;Same as previous function. Uses "PARENTHESES" instead of "ANGLE BRACKETS"


;;; See comment for type-to-stream.  

(defun type-to-lib-stream (tp stream)
  (if (symbolp tp) (princ tp stream)
      (progn (type-to-lib-stream (car tp) stream)
	     (if (consp (cdr tp))
		 (progn (princ "(" stream)
			(type-to-lib-stream (cdr tp) stream)
			(princ ")" stream))
		 (type-to-lib-stream (cdr tp) stream)))))

(defun create-propsym (var type)
  (let* ((unabbreviated-type (remove-type-abbrev type))
	 (fullname (fullname var unabbreviated-type)))
    (when (not (and retain-initial-type (get fullname 'type)))
	  (putprop fullname unabbreviated-type 'unabbreviated-type)
	  (putprop fullname type 'type))
    fullname))

(defun remove-type-abbrev (type)
  (if (symbolp type)
      (if (typeabbrev-p type) (remove-type-abbrev (get-type-def type)) type)
      (cons (remove-type-abbrev (car type)) (remove-type-abbrev (cdr type)))))

(defwffop intern-subst
  (argtypes gwff gvar)
  (argnames gwff var)
  (resulttype gwff)
  (applicable-p always-true)
  (mhelp "Converts term to desired form for substitution."))

(defun intern-subst (gwff var)
  (if (label-q gwff) (apply-label gwff (intern-subst gwff var)) gwff))

(defwffop rename-bd-var
  (argtypes gwff)
  (resulttype gwff)
  (argnames bdwff)
  (arghelp "bound wff")
  (applicable-p boundwff-p)
  (mhelp "Rename the top-level bound variable using the value of the
global parameter REN-VAR-FN."))

(defun rename-bd-var (bdwff)
  (cond ((label-q bdwff) (apply-label bdwff (rename-bd-var bdwff)))
	((boundwff-q bdwff)
	 (let ((newvar (funcall ren-var-fn (caar bdwff))))
	   (acons newvar (binder bdwff)
		  (substitute-term-var newvar (caar bdwff) (cdr bdwff)))))
	(t (throwfail "Cannot rename bound variable: "
		      (bdwff . gwff) " is not a bound wff."))))

(defwffop change-print-type
  (argtypes gvar typesym)
  (resulttype typesym)
  (argnames gvar typesym)
  (arghelp "Symbol" "Type to be used for printing")
  (mhelp "Use the type specified whenever this symbol is printed. Note that
this type may be overridden, if the flag retain-initial-type is NIL."))

(defun change-print-type (gvar typesym)
  (if (equal (get gvar 'unabbreviated-type) (remove-type-abbrev typesym))
      (putprop gvar typesym 'type)
      (throwfail T "Specified type " (typesym . typesym) 
		 " is not the same as the type of the variable "
		 (gvar . gvar) ".")))

(defwffop substitute-types
  (argtypes typealist gwff)
  (resulttype gwff)
  (argnames alist gwff)
  (arghelp "alist of types" "gwff")
  (mhelp "Substitute for types from list ((old . new) ...) in gwff."))

(defun substitute-types (alist gwff)
  (cond ((label-q gwff) (apply-label gwff (substitute-types alist gwff)))
	((lsymbol-q gwff)
	 (cond ((logconst-q gwff) ; cebrown 8/29/01 - could be polymorphic, eg = or IOTA
		(if (and (get gwff 'polytypelist) (get gwff 'stands-for))
		    (inherit-abbrev (get gwff 'stands-for)
				    (sublis alist (get gwff 'type))
				    (sublis alist (get gwff 'polytypelist)))
		  gwff))
	       ((propsym-q gwff)
		(create-propsym (getnameroot gwff) (sublis alist (type gwff))))
	       ((or (pmpropsym-q gwff) (pmabbrev-q gwff))
		(inherit-abbrev (get gwff 'stands-for)
				(sublis alist (get gwff 'type))
				(sublis alist (get gwff 'polytypelist))))
	       ((abbrev-q gwff) gwff)))
	((boundwff-q gwff)
	 (cons (cons (substitute-types alist (caar gwff)) (cdar gwff))
	       (substitute-types alist (cdr gwff))))
	(t (cons (substitute-types alist (car gwff))
		 (substitute-types alist (cdr gwff))))))

(defun subst-1-type (type1 type2 gwff)
  (substitute-types (list (cons type1 type2)) gwff))

(defwffop subst-1-type
  (argtypes typesym typesym gwff)
  (resulttype gwff)
  (argnames typevar typesym gwff)
  (arghelp "Type to be replaced" "Type to be substituted" "in what wff?")
  (mhelp "Substitute typevar with typesym."))

(defwffop type
  (argtypes gwff)
  (resulttype typesym)
  (argnames gwff)
  (arghelp "gwff")
  (mhelp "Return the type of a gwff."))

(defun typeabbrev-p (type)
  (and (symbolp type) (get type 'typeabbrev)))

(defun get-type-def (type) (get type 'type-defn))

(defun type-car (type)
  (if (atom type)
      (if (typeabbrev-p type) (type-car (get-type-def type))
	  (throwfail "No CAR for constant type."))
      (car type)))

(defun type-cdr (type)
  (if (atom type) (if (typeabbrev-p type) (type-cdr (get-type-def type))
		      (throwfail "No CDR for constant type."))
      (cdr type)))

(defun type-consp (type)
  (if (atom type) (if (typeabbrev-p type) (type-consp (get-type-def type)) nil)
      t))

(defun type (gwff)
  (cond ((label-q gwff) (apply-label gwff (type gwff)))
	((lsymbol-q gwff) (get gwff 'type))
	((boundwff-q gwff) (boundwfftype gwff))
	(t (type-car (type (car gwff))))))

(defun unabbreviated-type (gwff)
  (let ((type (type gwff)))
    (unabbreviate-type type)))

(defun unabbreviate-type (type)
  (if (atom type)
      (if (typeabbrev-p type)
	  (unabbreviate-type (get-type-def type))
	type)
    (cons (unabbreviate-type (car type))
	  (unabbreviate-type (cdr type)))))

(defun boundwfftype (gwff)
  (let* ((bd (cdar gwff))
	 (polytypevarlist
	  (mapcar #'(lambda (tvar) (cons tvar (next-temp-tvar)))
		  (get bd 'typelist))))
    (%catch% (matchtwo (type (caar gwff))
		       (sublis polytypevarlist (get bd 'var-type)))
	     (fail (throwfail "Incorrect type for variable bound by "
			      (bd . fsym) ".")))
    (%catch% (matchtwo (type (cdr gwff))
		       (sublis polytypevarlist (get bd 'scope-type)))
	     (fail (throwfail "Incorrect type for the scope of the binder "
			      (bd . fsym) ".")))
    (collecttype (sublis polytypevarlist (get bd 'wff-type)))))


(defwffop free-vars-of
  (argtypes gwff)
  (resulttype gvarlist)
  (argnames inwff)
  (applicable-q always-true)
  (mhelp "Creates a list of variables free in the wff."))

;;; Added remove-duplicates 10/2/87 DAN
;;;Changed to delete duplicates. SI 10/24/87

(defun free-vars-of (inwff) (delete-duplicates (free-vars inwff nil)))

(defwffrec free-vars 
  (argnames gwff bind-list)
  (mhelp "Finds free variables of a wff."))

(defun free-vars (gwff bind-list)
  (cond ((label-q gwff) (apply-label gwff (free-vars gwff bind-list)))
	((lsymbol-q gwff)
	 (cond ((get gwff 'abbrev) (free-vars (get gwff 'defn) bind-list))
               (t (if (and (not (logconst-q gwff)) (propsym-q gwff)
		           (not (member gwff bind-list)))
	              (list gwff)))))
	((boundwff-q gwff) (free-vars (cdr gwff) (cons (caar gwff) bind-list)))
	(t (nconc (free-vars (car gwff) bind-list) 
		  (free-vars (cdr gwff) bind-list)))))

(defwffop binding
  (argtypes gwff)
  (argnames gwff)
  (arghelp "bound wff")
  (resulttype binder)
  (applicable-q boundwff-q)
  (mhelp "Returns top-level binder of wff."))

(defun binding (gwff)
  (cond ((label-q gwff) (apply-label gwff (binding gwff)))
	((boundwff-q gwff) (cdar gwff))
	(t (throwfail (gwff . gwff) " is not a bound wff."))))

(defwffop bindvar
  (argtypes gwff)
  (argnames gwff)
  (arghelp "bound wff")
  (resulttype gwff)
  (mhelp "Returns variable bound at top-level."))

(defun bindvar (gwff)
  (cond ((label-q gwff) (apply-label gwff (bindvar gwff)))
	((boundwff-q gwff) (caar gwff))
	(t (throwfail (gwff . gwff) " is not a bound wff."))))

(defwffop bindhead
  (argtypes gwff)
  (argnames gwff)
  (arghelp "bound wff")
  (mhelp "Returns head of top-level binding."))

(defun bindhead (gwff)
  (cond ((label-q gwff) (apply-label gwff (bindhead gwff)))
	((boundwff-q gwff) (car gwff))
	(t (throwfail (gwff . gwff) " is not a bound wff."))))

(defun fresh-var (type bnam)
  (let ((try-1 (create-propsym bnam type)))
    (if (lsymbol-p try-1) (funcall ren-var-fn try-1) try-1)))

(defun fresh-var-1 (type) (fresh-var type '\x))

(defun bind-var-wff (binder var gwff)
  (if (label-q gwff) (apply-label gwff (bind-var-wff binder var gwff))
      (acons var binder gwff)))

(defun bind-var-wff-n (binder vlis gwff)
  (if vlis
      (bind-var-wff-n binder (cdr vlis) (bind-var-wff binder (car vlis) gwff))
      gwff))

(defun attach-head (head gwff)
  (if (label-q gwff) (apply-label gwff (attach-head head gwff))
      (cons head gwff)))

(defwffop apply-wff
  (argnames wff1 wff2)
  (argtypes gwff gwff)
  (resulttype gwff)
  (applicable-q function-of)
  (mhelp "Applies first wff to second."))

(defun type-of-arg-1 (gwff)
  (if (label-q gwff) (apply-label gwff (type-of-arg-1 gwff))
      (let ((typ (type gwff))) (if (consp typ) (cdr typ) nil))))

(defwffop type-of-arg-1
  (argnames gwff)
  (argtypes gwff)
  (resulttype typesym)
  (applicable-q (lambda (gwff) (consp (type gwff))))
  (mhelp "Finds type of first argument."))

(defun function-of (wff1 wff2)
  (equal-type-p (type-of-arg-1 wff1) (type wff2)))




(defun fntype-list (gwff)
  (if (label-q gwff) (apply-label gwff (fntype-list gwff))
    (do ((typin (type gwff) (car typin))
	 (typex nil (cons (cdr typin) typex)))
	((atom typin) (cons typin typex)))))

(defun cartsqproj-type (proj gwff)
  (let ((tlis (fntype-list proj)))
    (and
     (= 3 (length tlis)) (equal (cadr tlis) (caddr tlis))
     (equal (car tlis) (cadr tlis)) (equal (car tlis) (type gwff)))))

(defun inmost-car (list)
  (do ((list list (car list)))
      ((atom list) list)))


(defun apply-wff (wff1 wff2)
  (cond ((label-q wff1) (apply-label wff1 (apply-wff wff1 wff2)))
	((label-q wff2) (apply-label wff2 (apply-wff wff1 wff2)))
	(t (cons wff1 wff2))))

(defun apply-wff-n (gwff wflis)
  (if wflis (apply-wff (apply-wff-n gwff (cdr wflis)) (car wflis)) gwff))

(defun ab-change (gwff newvar)
  (if (label-q gwff) (apply-label gwff (ab-change gwff newvar))
      (acons newvar (binder gwff)
	     (substitute-term-var newvar (bdvar gwff) (cdr gwff)))))

;; The next two functions are from the DISTINGUISHED (now PRIMITIVE-SUBST)
;; package, but have
;; proved to be generally useful. The first takes a type-schema, a type
;; and the typevariables for the type-schema, and returns the cons
;; of the unmatched tvars with the list of matchings, if successful,
;; else the ncons of the type-variable list.

(defun ds-con-match (ctyp gtyp tvlis)
  (do ((tvs tvlis (cdr tvs))
       (unmtv nil)
       (mtv nil)
       (matchfail nil))
      ((or matchfail (endp tvs))
       (if matchfail (list tvlis) (cons unmtv mtv)))
    (let* ((tv (car tvs))
	   (temp (match-con-rec ctyp gtyp tv)))
      (if temp
	  (if (eq temp t)
	      (push tv unmtv)
	      (push (cons tv temp) mtv))
	  (setq matchfail t)))))

(defun match-con-rec (ctyp gtyp tv)
  (cond ((atom ctyp) (if (equal ctyp tv) gtyp (equal ctyp gtyp)))
	((atom gtyp) nil)
	(t (let ((first (match-con-rec (car ctyp) (car gtyp) tv)))
	     (and first
		  (let ((sec (match-con-rec (cdr ctyp) (cdr gtyp) tv)))
		    (cond ((eq first t) sec)
			  ((eq sec t) first)
			  (sec (and (equal first sec) sec))
			  (t nil))))))))


;;; A wffop rescued from oblivion.

(defun dupwff (gwff connective)
  (if (label-q gwff) (apply-label gwff (dupwff gwff connective))
      (apply-wff (apply-wff connective gwff) gwff)))

(defwffop dupwff
  (argtypes gwff0 gwff)
  (argnames gwff connective)
  (resulttype gwff)
  (applicable-p (lambda (gwff connective)
		  (and (lsymbol-q connective) 
		       (cartsqproj-type connective gwff))))
  (mhelp "duplicates wff across connective."))

;; cebrown 5/26/02
;; This used to be defined in etrees-wffops in the auto package.
;; Note that for exp-var's the action of this function
;; (as defined in ) looks in the subst slot.
;; I moved it to here because I wanted a function
;; independent of the auto package to use in the library code.
(defun get-all-defns (gwff)
  (do* ((l (get-all-defns-rec gwff) rec)
	(rec (remove-if #'null
			(reduce #'append
				(mapcar #'(lambda (x) (get-all-defns-rec x)) 
					(mapcar #'(lambda (x) (get x 'defn)) l))))
	     (remove-if #'null
			(reduce #'append
				(mapcar #'(lambda (x) (get-all-defns-rec x)) 
					(mapcar #'(lambda (x) (get x 'defn)) l)))))
	(full-l (append rec l) (append rec full-l)))
      ((null rec) (remove-if #'null (remove-duplicates full-l)))))

(defun get-all-defns-rec (gwff)
  (cond ((label-q gwff) (apply-label gwff (get-all-defns-rec gwff)))
	((lsymbol-q gwff)
	 (if (or (logconst-q gwff) (propsym-q gwff) (pmpropsym-q gwff))
	     nil (list (or (get gwff 'core::stands-for) gwff))))
	((boundwff-q gwff)
	 (append (if (get (binding gwff) 'defn) (list (binding gwff)) nil)
		 (get-all-defns-rec (gdr gwff))))
	(t (append (get-all-defns-rec (gar gwff)) (get-all-defns-rec (gdr gwff))))))

