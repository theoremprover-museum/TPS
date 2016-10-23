;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :MAINT)
(part-of maintain)

(deffile argtyp-maint
  (part-of maintain)
  (mhelp "Contains the definitions of types used in MAINT."))

(context coll-help)

(deftype% context 
  (getfn testfn)
  (testfn (get context 'context))
  (printfn princ)
  (mhelp "A context."))

(deflisttype contextlist context
  (other-keys ((eq contextlist 'all) 
	       (set-of contex global-contextlist
		       (and (symbolp contex) (accessible-p contex))))
	      ((and (consp contextlist) (eq (car contextlist) 'all-))
	       (set-of contex (setdiff global-contextlist (cdr contextlist))
		       (and (symbolp contex) (accessible-p contex)))))
  (mhelp ("A list of contexts or ALL or (ALL- ...)."
	  (e (category-mhelp-list 'context)))))

(deftype% tpscat 
  (getfn testfn)
  (testfn (member tpscat global-categorylist))
  (printfn princ)
  (mhelp "A category of TPS objects."))

(deflisttype tpscatlist tpscat
  (other-keys ((eq tpscatlist 'all) (set-of cat global-categorylist
				       (symbolp cat)))
	      ((and (consp tpscatlist)
		    (eq (car tpscatlist) 'all-))
	       (set-of cat (setdiff global-categorylist (cdr tpscatlist))
		 (symbolp cat))))
  (mhelp ("A list of categories or ALL or (ALL- ...).  Currently any of "
	  t global-categorylist)))

(deflisttype dev-stylelist dev-style
  (mhelp "A list of device styles."))

(context tps-maintenance)
(defconstype symbolpair symbol symbol (mhelp "The type of a dotted pair of symbols."))
(deflisttype symbolpairlist symbolpair (mhelp "The type of a list of dotted pairs of symbols"))

(context modules-in-tps)

(deftype% tps-module
  (testfn module-p)
  (getfn testfn)
  (printfn princ)
  (mhelp ("A module." (e (category-mhelp-list 'module)))))

(deflisttype modulelist tps-module
  (other-keys ((eq modulelist 'all) (remove-if #'consp global-modulelist))
	      ((eq modulelist 'loaded-modules) (loaded-modules))
	      ((eq modulelist 'unloaded-modules) (unloaded-modules))
	      ((and (consp modulelist)
		    (eq (car modulelist) 'all-))
	       (remove-if #'consp (setdiff global-modulelist
					   (cdr modulelist)))))
  (mhelp ("A list of modules."  (e (category-mhelp-list 'module)))))

(context rule-run)

(deftype% rule
  (testfn (get rule 'iruledef))
  (getfn testfn)
  (printfn princ)
  (mhelp ("A rule that has been defined through DEFIRULE."
	  (e (category-mhelp-list 'iruledef)))))

