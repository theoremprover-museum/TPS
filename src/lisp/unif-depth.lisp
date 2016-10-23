;;; -*- Mode: Lisp -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)

(defun unify-depth (next-node subst-hash-table depth)
  (if (and (memq unify-verbose '(med max)) max-utree-depth (> depth max-utree-depth)) 
      ;(msgf "Current utree depth is " depth "...which is over MAX-UTREE-DEPTH." t))
      (msg "u")) ; abbreviated because it was a pain. MB Thu May 18 12:48:23 1995
  (unless (and max-utree-depth (> depth max-utree-depth))
    (when (memq unify-verbose '(med max)) (msg 2 depth))
    (multiple-value-bind (terminal-flag subst-stack dpairs free-vars)
	  (simpl (node-dpairs next-node)
		 (node-subst-stack next-node) (node-free-vars next-node) 0)
      (if (eq terminal-flag 'more)
	  (multiple-value-bind (var substitutions flag)
	      (funcall apply-match next-node subst-hash-table depth nil)
	    ;;FLAG is T if RHEAD is NOT.
	    (dolist (substitution substitutions)
	      (let* ((son (create-successor
			    (acons var substitution subst-stack)
			    flag next-node))
		     (success (unify-depth son subst-hash-table (1+ depth))))
		(when success (return success)))))
	  (when (eq terminal-flag 'success)
	    (setf (node-subst-stack next-node)
		  (nconc (find-subs-flexible-pairs dpairs subst-stack)
			 (node-subst-stack next-node)))
	    (print-subst-stack (node-subst-stack next-node))
	    (setf (node-terminal-flag next-node) t)
	    next-node)))))


