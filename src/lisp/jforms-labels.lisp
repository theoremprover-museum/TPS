;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of JFORMS)

(deffile jforms-labels
  (part-of jforms)
  (extension clisp)
  (mhelp "Defines flavors of jform labels and jform printing commands."))

(context jforms1)

(eval-when (compile load eval)
(defflavor jform
  (mhelp "Defines common properties of jforms.")
  (structured t)
  (instance-attributes
   represents
   (pos t) 
   (parent nil)
   type
   (num-paths-below 0)
   (progress 0)
   (position 0 :type integer) ;used in ms90-3 to recover the order in original jform
   )
  (printfn print-jform)
  ))

(eval-when (load compile eval)
(defflavor disjunction
  (mhelp "A disjunction label stands for a disjunction of wffs.")
  (inherit-properties jform)
  (include jform (type 'disjunction))
  (printfn print-jform)
  (instance-attributes
   components)
  (printwff (lambda (wff bracket depth)
	      (declare (ignore bracket))
	      (print-jform wff *standard-output* depth)))
  (prt-symbol-p (lambda (gwff)(declare (ignore gwff)) t))
  (prt-infix-op (lambda (gwff)(declare (ignore gwff)) nil))
  (prt-prefix-op (lambda (gwff) (declare (ignore gwff)) nil))
  (prt-associative-p (lambda (gwff) (declare (ignore gwff)) nil))
  (type (lambda (gwff) (declare (ignore gwff)) 'O))
  (gwff-p (lambda (gwff)(declare (ignore gwff)) T))
  (gwff-q (lambda (gwff)(declare (ignore gwff)) T))
  (legal-type-p1 (lambda (gwff) (declare (ignore gwff)) T))
  (copy-jform (lambda (jform) (copy-disjunction jform)))))

(eval-when (load compile eval)
(defflavor conjunction
  (mhelp "A conjunction label stands for a conjunction of wffs.")
  (include jform (type 'conjunction))
  (printfn print-jform)
  (inherit-properties disjunction)
  (instance-attributes
   components)
  (copy-jform (lambda (jform) (copy-conjunction jform)))))

(eval-when (load compile eval)
(defflavor literal
  (mhelp "A literal label stands for a wff which is not a conjunction,
	 disjunction, universally or existentially bound, or a negation.")
  (include jform (type 'literal))
  (printfn print-jform)
  (inherit-properties disjunction)
  (instance-attributes
   (name (create-namestring lit-name))
   prop-class)
  (not-p (lambda (label) (if (jform-pos label) nil t)))
  (gar (lambda (label) (if (jform-pos label) (gar (jform-represents label)) 'not)))
  (gdr (lambda (label) (if (jform-pos label) (gdr (jform-represents label))
			 (jform-represents label))))
  (glr (lambda (label) (if (jform-pos label) (glr (jform-represents label))
			 (throwfail "Label does not represent an infix formula."))))
  (grr (lambda (label) (if (jform-pos label) (grr (jform-represents label))
			 (throwfail "Label does not represent an infix formula."))))
  (copy-jform (lambda (jform) (copy-literal jform)))
  ))

(eval-when (compile load eval)
(defflavor universal
  (mhelp "A universal label stands for a wff which is universally bound.")
  (include jform (type 'universal))
  (printfn print-jform)
  (inherit-properties disjunction)
  (instance-attributes
   (qvars nil)
   (scope nil)
   (dup nil)
   (prnt nil)
   (selected nil)
   (substs nil)
   (dup-t nil)
   (dup-mark 0) ;;; used to fix the back-tracking bug incorrectlu handling 
   (adam nil)   ;;; duplication under disjunction; only duplicated jforms
                ;;; have "adam" property, which points to the original jforms.
   (out-qvars nil)) ;;;allow to duplicate inner quantifiers.
  (copy-jform (lambda (jform) (copy-universal jform)))))

;;;The following macro is used to check whether a jform or its duplications have
;;;been used in a mating. There was a bug bypassing a jform under a disjunction
;;;even if its duplications have got used. 
(defmacro jform-mark (jform)
  `(and (eq (jform-type ,jform) 'universal) 
        (> (universal-dup-mark ,jform) 0)))

(defflavor existential
  (mhelp "An existential label stands for a wff which is existentially bound.")
  (include universal (type 'existential))
  (printfn print-jform)
  (inherit-properties universal)
  (instance-attributes )
  (copy-jform (lambda (jform) (copy-existential jform))))

(defun print-jform (wff *standard-output* *print-depth*)
  (declare (ignore *print-depth*))
  (princ (print-jform-main wff)))

(defun print-jform-main (wff)
  (declare (special print-lit-name))
  (case (jform-type wff)
    (disjunction
      (cons 'or (mapcar #'print-jform-main (disjunction-components wff))))
    (conjunction
      (cons 'and (mapcar #'print-jform-main (conjunction-components wff))))
    (universal (list (cons 'forall (universal-qvars wff))
		     (print-jform-main (universal-scope wff))))
    (existential (list (cons 'exists (universal-qvars wff))
		       (print-jform-main (universal-scope wff))))
    (literal (if print-lit-name (literal-name wff)
		 (if (jform-pos wff) (jform-represents wff)
		     (cons 'not (jform-represents wff)))))
    (T (throwfail wff " is not a jform."))))

(defun copy-jform (jform)
  (apply-label jform (copy-jform jform)))
