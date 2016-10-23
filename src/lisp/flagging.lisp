;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of TPSDEF)

;;;
;;; File: Flagging
;;;
;;; defines DEFFLAG and other flag-related TPS-objects, but not
;;; REVIEW.  Belongs to BARE TPS.
;;;

(deffile flagging
  (part-of tpsdef)
  (extension clisp)
  (mhelp "Defines DEFFLAG and other flag-related TPS-objects."))

(context flags)

(defun subject-p (subject) (member subject global-subjectlist))

					; the following is an association list of flags and lists of flags.
					; if a flag is on the domain of the alist, then the value of the flag is currently irrelevant
					; because of the settings of the flags associated with it. - cebrown 8/25/00
(defvar *never-irrelevant* nil)
(defvar *relevance-graphs-built* nil)

;;; Following defvars flag right away, during compilation.  Also
;;; allows one to add subjects to a flag, by just 
;;; (defflag flag (subjects new-subject)) without otherwise modifying
;;; it. This is useful when new modules use old flags also.

(defcategory flag
  (define defflag%)
  (properties
   (flagtype single)
   (default single)
   (pre-change-fn singlefn) ; cebrown 10/29/00
   (change-fn singlefn)
   (subjects multiple)
   (relevancy-preconditions multiple)
   (irrelevancy-preconditions multiple)
   (relevant-kids multiple)
   (irrelevant-kids multiple)
   (mhelp single))
  (global-list global-flaglist)
  (mhelp-line "flag or parameter")
  (mhelp-fn describe-flag))

(defmacro defflag (name &rest forms)
  (let* ((current-subjects (get name 'subjects))
	 (subjects-form (cdr (assoc 'subjects forms)))
	 (all-subjects
	  (let ((res (copy-list current-subjects)))
	  (dolist (subject subjects-form res)
	    (pushnew subject res))))
	 (flagtype-form (or (cadr (assoc 'flagtype forms))
			    (get name 'flagtype)))
	 (default-form (assoc 'default forms))
	 (default-internal
	  (if (and flagtype-form default-form)
	      (%catch% (gettype flagtype-form (cadr default-form))
		       (fail (break) (complain "Default of flag " name
				       " incorrect.  NIL substituted." t 
				       expand-catch-throw)
			     nil))
	    (if (null flagtype-form)
		(progn
		  (complain "Flag " name " has no flagtype defined.  Not defining.")
		  (return-from defflag nil))
	      (get name 'default nil))))
	 (other-props (remove 'subjects
			      (remove 'flagtype

				      (remove 'default forms
					      :key #'car)
				      :key #'car)
			      :key #'car)))
    `(progn
       (defvar ,name ',default-internal)
       ,(if default-form
	   `(setf (get ',name 'default) ',default-internal))
       (setf (get ',name 'subjects) ',all-subjects)
       (setf (get ',name 'flagtype) ',flagtype-form)
       (defflag% ,name ,@other-props))))

;;;defsynonym is used to define synonym name for a flag. The usage is
;;;very simple. For example,
;;;(defsynonym SUBNAME
;;;            (synonym TRUENAME)
;;;            (replace-old T)
;;;            (mhelp "SUBNAME is a synonym for the flag TRUENAME".))
;;;defines SUBNAME as a synonym for the flag TRUENAME. Since replace-old
;;;is true, SUBNAME will be recorded instead of TRUENAME; Otherwise, SUBNAME
;;;would not be recorded(in this case SUBNANE behaves almost like an alias).
(defmacro defsynonym (name &rest forms)
   (let* ((synonym (cadr (assoc 'synonym forms)))
          (check-synonym (or (get synonym 'flag)
                             (throwfail "The synonym " synonym " given is not a flag.")))
          (replace-old (cadr (assoc 'synonym forms)))
	  (mhelp (cadr (assoc 'mhelp forms)))
          (truesynonym (get synonym 'synonym))
          (current-subjects (get name 'subjects))
          (subjects-form (get synonym 'subjects))
          (default-form (get synonym 'default)))
     (declare (ignore check-synonym))
     (msg mhelp)
;;;very clumsy way to update synonyms
       `(progn
          ,(if truesynonym
              `(setf (get ',name 'synonym) ',truesynonym)
               (if replace-old
                  `(setf (get ',synonym 'synonym) ',name)
                  `(setf (get ',name 'synonym) ',synonym)))
          (defvar ,name ',default-form)
          (update-synonymlist ',name ',synonym)
          (defflag% ,name
             (subjects
              ,@(let ((res (copy-list current-subjects)))
                    (dolist (subject subjects-form res)
                            (pushnew subject res))))
              (flagtype ,(get synonym 'flagtype))
              (default ,(get synonym 'default))
              (change-fn ,(get synonym 'change-fn))
	      (mhelp ,mhelp)))))

(defun update-synonymlist (name synonym)
  (let ((oldlist (get synonym 'synonymlist)))
   (setf (get name 'synonymlist) (cons synonym oldlist))
   (dolist (other (get name 'synonymlist) nil)
           (setf (get other 'synonymlist) 
                 (cons name (get other 'synonymlist))))))

(defun update-all-in-list (synonymlist value)
   (dolist (hxname synonymlist nil)
           (set hxname value)))

;;; MODES
;;; MODES are groups of flags all with a predetermined value.
;;; MODES are defined with the DEFMODE function and entered with
;;; the MODE command in REVIEW.
;;; NOTE:  in this first version we offer a DEFMODE function
;;;  which allows one to define a mode by listing flags and their
;;;  associated values (in external format) as well as a MHELP string.
;;;  The MODE command will the switch between modes.

(defcategory flag-mode
  (define defmode)
  (properties
   (flag-settings multiple)
   (mhelp single))
  (global-list global-modelist)
  (mhelp-line "mode")
  (mhelp-fn describe-mode))

(defun tps-mode-p (tps-mode) 
  (declare (special core::*lib-masterindex*))
  (or (member tps-mode global-modelist)
      (and (module-loaded-p 'library)
	   (let (again) 
	     (declare (special again *using-interface*))
	     (prompt-read again nil (msgf tps-mode " is not a tps-mode. Search for it in library?") 
			  'yesno 'yes nil))
	   (let ((y (car (remove-if-not #'(lambda (x) (memq (car x) '(mode mode1)))
					(gethash tps-mode core::*lib-masterindex*)))))
	     (if y
		 (progn (msgf "Found a " (car y) t)
			(core::retrieve-libobject tps-mode :type (car y) :multiple nil)
			t)
	       (progn (msgf "Can't find mode " tps-mode t) nil))))))

(defflag last-mode-name
  (flagtype string)
  (subjects auto:mating-search auto:transmit)
  (default "")
  (mhelp "LAST-MODE-NAME contains the name of the last MODE used. There
is no point in the user's altering its value, since TPS only ever 
writes to it, and never reads from it."))

(defvar *mode-altered* nil)
(defvar *lastmode* nil)

					; cebrown 11/18/00 - modified this to set other important flags not set by mode to default values
					; for example, many modes do not set MAX-SUBSTS-VAR, so MAX-SUBSTS-VAR should be NIL
					; (but only do this if mode is not "QUIET")
					; cebrown 11/30/00 - modified this to set flags to their default before setting flags in mode
					; because some flags set other flags (DEFAULT-MS sets DEFAULT-EXPAND and DEFAULT-MATE)
					; cebrown 11/5/03 - added argument to avoid changing certain flags, eg, test-theorems during tps-test
(defun mode (mode &optional exceptions)
  (let ((already-set nil)
	(suppress-irrelevance-warnings T))
    (declare (special suppress-irrelevance-warnings))
    (unless (eq mode (gettype 'tps-mode 'maint::quiet))
      (dolist (subject '(IMPORTANT MATING-SEARCH MS88 MS89 MS90-3 MS90-9 MS91-6 MS91-7 MS92-9 MS93-1 MS93-1 AUTO::MS98-MINOR MS98-1
			 UNIFICATION AUTO::PRIMSUBS AUTO::MTREE-TOP))
					; anyone who creates a new subject important for automatic search, it should be included on this list
	(dolist (flag (subject-flags subject))
	  (unless (or (member flag already-set)
		      (member flag exceptions)
		      (not (get flag 'flag)))
	    (push flag already-set)
	    (let ((current-value (eval flag)))
	      (set flag (get flag 'default)) ; using set here because set-flag tries to modify the value as if it were input by user
	      (when (get flag 'change-fn) ; need to call the change-fn (eg, for TRUTHVALUES-HACK), cebrown 3/3/01
		(funcall (get flag 'change-fn) flag (get flag 'default) current-value)))))))
    (dolist (pair (get mode 'flag-settings))
      (let ((flag (car pair)))
	(when (get flag 'flag) ;;;Sometimes a flag is defined depending on the version of LISP
	  (unless (member flag exceptions)
	    (%catch%
	     (set-flag flag (cadr pair))
	     (fail (complain f "Could not set " flag t expand-catch-throw))))))) ; note that some flags set other flags (eg: DEFAULT-MS sets DEFAULT-EXPAND & DEFAULT-MATE)
    (unless (and (eq mode (gettype 'tps-mode 'maint::quiet)) (neq last-mode-name ""))
      (setq last-mode-name 
	    (concatenate 'string
			 "Mode : "
			 (string-upcase (princ-to-string mode))))
      (setq *lastmode* mode)
      (setq *mode-altered* nil))))

(defun set-flag (flag flag-value)
  (%catch%
   (if (member flag global-flaglist) ; added this check to prevent being
					; thrown to the debugger when set-flag
					; is given a non-flag - cebrown 5/26/02
       (let ((internal-value (gettype (get flag 'flagtype) flag-value))
	     (current-value (eval flag)))
	 (when (get flag 'pre-change-fn)
	   (funcall (get flag 'pre-change-fn)
		    flag internal-value current-value))
	 (set flag internal-value)
	 (when (get flag 'change-fn)
	   (funcall (get flag 'change-fn) 
		    flag internal-value current-value))
	 (when (get flag 'synonymlist)
	   (update-all-in-list (get flag 'synonymlist) (eval flag)))
	 (unless (or (eq internal-value current-value) (eq flag 'last-mode-name))
	   (setq last-mode-name (concatenate 'string last-mode-name
					     (if *mode-altered* ", and " ", but with ")
					     (string-upcase (princ-to-string flag))
					     " set to " (princ-to-string flag-value)))
	   (setq *mode-altered* t)))
     (complain "WARNING: Could not set nonflag " flag))
   (fail (complain "Argument supplied is not of type "
		   (get flag 'flagtype) "." t expand-catch-throw t
		   "Flag not changed."))))

;;; IN-MODE is a very useful general macro for executing a piece of
;;; code with certain flag-settings forced.

;;; Changed error to throwfail and made more robust to
;;; allow one to say (in-mode 'mode ...)  8/9/87 DAN
(defmacro in-mode (mode &rest forms)
  (let ((mode (if (symbolp mode)
		  mode
		  (if (and (listp mode) (eq (car mode) 'quote)
			   (symbolp (cadr mode)))
		      (cadr mode)
		    mode))))
  (if (and (symbolp mode) (get mode 'flag-mode))
      `(let ,(mapcan #'(lambda (pair)
			 (let ((flagtype (get (car pair) 'flagtype)))
			   (if flagtype
			       (list (list (car pair)
					    (list 'gettype
						  (list 'quote flagtype)
						  (list 'quote (cadr pair)))))
			       nil)))
		     (get mode 'flag-settings))
	 ,@forms)
      (throwfail (format nil "Undefined Mode ~S used for IN-MODE" mode))
)))


(defutil in-mode
  (form-type macro)
  (keywords flags output)
  (mhelp "(IN-MODE mode form1 ... formn) is an extremely useful macro.
It will locally bind all flags and parameters affected by mode to the
value in the mode and the execute form1 ... formn.  Note that no initfn
is called when the flags are set.  Note also that the macro (of course!)
is expanded at compile-time and therefore changing the definition of
mode will have no effect on the execution of form1 ... formn until
the IN-MODE is recompiled or loaded in uncompiled form.  Examples
of uses are
 (IN-MODE SCRIBE-DOC (MSG (A . GWFF))) will print the gwff A in style
Scribe.
 (IN-MODE RE-READ (MSG (A . GWFF))) will print the gwff A in such a
way that it can be read back."))

					; cebrown 1/29/03
(defcategory modes-gwffs
  (define def-modes-gwffs)
  (properties
   (modes-gwffs-modes multiple)
   (modes-gwffs-gwffs multiple)
   (mhelp single))
  (global-list global-modes-gwffs)
  (mhelp-line "pair of list of modes and list of gwffs")
  (mhelp-fn describe-modes-gwffs))


