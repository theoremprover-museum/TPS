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

(part-of jforms)

(deffile weak-mac-auto
  (part-of jforms)
  (extension clisp)
  (mhelp "Jform-Wff conversion commands."))

(context weak-labels)

(defflavor weak
  (gwff-to-jform-rec
    (lambda (pos gwff parent)
      (if print-weak
	  (list (make-literal :type 'literal :represents gwff :pos pos))
	  (gwff-to-jform-rec pos (get-weak gwff) parent))))
  (gwff-to-etree (lambda (gwff skolemize deepen)
		   (gwff-to-etree (get-weak gwff) skolemize deepen)))
  (simul-substitute-l-term-var-rec 
    (lambda (alist inwff)
      (simul-substitute-l-term-var-rec alist (get-weak inwff))))
  (simul-substitute-term-var-rec 
    (lambda (alist inwff)
      (simul-substitute-term-var-rec alist (get-weak inwff))))
  (simul-substitute-term-var-rec-etree
    (lambda (alist inwff)
      (simul-substitute-term-var-rec-etree alist (get-weak inwff))))
  ;;unification
  (fo-hnf
    (lambda (pos term args subst-stack)
      (fo-hnf pos (get-weak term) args subst-stack)))
  (hnf-rec
    (lambda (pos term args binder bdvars stack subst-stack)
      (hnf-rec pos (get-weak term) args binder bdvars stack subst-stack)))
  (free-in-var-term
    (lambda (var term bdvars stack subst-stack rigid-path free-vars)
      (free-in-var-term var (get-weak term) bdvars stack subst-stack
			rigid-path free-vars)))
  )
