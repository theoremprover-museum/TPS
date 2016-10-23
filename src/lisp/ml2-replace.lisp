;;; -*- Mode:LISP; Package:ML -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :ML)
(part-of MATH-LOGIC-2-RULES)

(deffile ml2-replace
  (part-of math-logic-2-rules)
  (extension lisp)
  (mhelp "Defines wffop used by ERP and IRP rules."))

(context abbrev-ops)

;;; Next section contains wffop which is used by the ERP and IRP rules.
;;; Note that it explicitly prompts for the symbol to be replaced
;;; because the rules package cannot handle arguments other than
;;; wffs.

(defwffop rpin
  (argtypes gwff)
  (resulttype gwff)
  (wffargtypes "O")
  (wffop-type "O")
  (argnames inwff)
  (arghelp "Wff to Replace Symbol in")
  (mhelp "Prompt for a replaceable symbol and the name of a replacement
and replace the first occurrence of the symbol."))

(defun rpin (inwff)
  (let (repsym repname)
    (prompt-read repsym nil (msgf "Symbol to Replace")
		 'repsym '$ ((? (mhelp 'repsym))))
    (loop (prompt-read repname nil (msgf "Name of Replacement")
		       'symbol '$ ((? (princ-rephelp repsym 'repsym))))
	  (if (assoc repname (get repsym 'equiv-to))
	      (return)
	      (complain "Not a legal replacement for " (repsym . repsym) ".")))
    (replace-equiv repsym repname inwff)))

;;; Next we define some replacable symbols

(defrepsymbol and
  (equiv-to
   (or "lambda p lambda q . ~ [~ p or ~ q]")
   (implies "lambda p lambda q . ~ [p implies ~ q]")
   (inverse "lambda p lambda q . q and p")))

(defrepsymbol or
  (equiv-to
   (and "lambda p lambda q . ~ [~ p and ~ q]")
   (implies "lambda p lambda q . ~ p implies q")
   (inverse "lambda p lambda q . q or p")))

(defrepsymbol implies
  (equiv-to
   (or "lambda p lambda q . ~ p or q")
   (and "lambda p lambda q . ~ [p and ~ q]")
   (inverse "lambda p lambda q . ~ q implies ~ p")))

(defrepsymbol subset
  (equiv-to
   (inverse "lambda p lambda q . [complement q] subset [complement p]")
   (implies "lambda p lambda q . forall x . p x implies q x")
   (intersect "lambda p lambda q . [p intersect q] = p")))
