;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)

(deffile wffmbed
  (part-of wff-ops1)
  (extension clisp)
  (mhelp "Contains editor operations to embed the current gwff
within the scope of a connective or quantifier."))

(defwffop mbed-and-left
  (argtypes gwff0 gwff0)
  (resulttype gwff0)
  (argnames lgwff rgwff)
  (arghelp "Left scope of AND" "Right scope of AND")
  (defaultfns (lambda (lgwff rgwff)
		(If (eq rgwff '$)   
		    (let ((global-type 'o))
		      (declare (special global-type))
		      (list lgwff (getwff-subtype 'gwff-p "x(O)")) )
		    (list lgwff rgwff))))
  (mhelp "Embed the current edwff in the left scope of AND. 
The right scope is provided by the user."))

(defun mbed-and-left (lgwff rgwff)
  (mbed-conn 'and lgwff rgwff))

(defun mbed-conn (connective lgwff rgwff)
  (acons connective lgwff rgwff))

(defwffop mbed-and-right
  (argtypes gwff0 gwff0)
  (resulttype gwff0)
  (argnames rgwff lgwff)
  (arghelp "Right scope of AND" "Left scope of AND")
  (defaultfns (lambda (rgwff lgwff)
		(If (eq lgwff '$)   
		    (let ((global-type 'o))
		      (declare (special global-type))
		      (list rgwff (getwff-subtype 'gwff-p "x(O)")))
		    (list lgwff rgwff))))
  (mhelp "Embed the current edwff in the right scope of AND. 
The left scope is provided by the user."))

(defun mbed-and-right (lgwff rgwff)
  (mbed-conn 'and lgwff rgwff))

(defwffop mbed-existential
  (argtypes gvar gwff0)
  (resulttype gwff0)
  (argnames vquant crwff)
  (arghelp "Variable of Quantification" "Wff to quantify")
  (defaultfns (lambda (vquant crwff)
		(If (eq vquant '$)   
		    (let ((global-type 'i))
		      (declare (special global-type))
		      (list (getwff-subtype 'gwff-p "x(i)") crwff))
		    (list vquant  crwff))))
  (mhelp "Embed the current edwff in the scope of a existential quantifier. 
The variable of quantification is provided by the user."))

(defun mbed-existential (vquant crwff)
  (mbed-quant 'exists vquant crwff))

(defwffop mbed-existential1
  (argtypes gvar gwff0)
  (resulttype gwff0)
  (argnames vquant crwff)
  (arghelp "Variable of Quantification" "Wff to quantify")
  (defaultfns (lambda (vquant crwff)
		(If (eq vquant '$)   
		    (let ((global-type 'i))
		      (declare (special global-type))
		      (list (getwff-subtype 'gwff-p "x(i)") crwff))
		    (list vquant  crwff))))
  (mhelp "Embed the current edwff in the scope of an exists1 quantifier. 
The variable of quantification is provided by the user."))

(defun mbed-existential1 (vquant crwff)
  (mbed-quant 'ml:exists1 vquant crwff))

(defwffop mbed-forall
  (argtypes gvar gwff0)
  (resulttype gwff0)
  (argnames vquant crwff)
  (arghelp "Variable of Quantification" "Wff to quantify")
  (defaultfns (lambda (vquant crwff)
		(If (eq vquant '$)
		    (let ((global-type 'i))
		      (declare (special global-type))
		      (list (getwff-subtype 'gwff-p "x(i)") crwff))
		    (list vquant crwff))))
  (mhelp "Embed the current edwff in the scope of a universal quantifier. 
The variable of quantification is provided by the user."))

(defun mbed-forall (vquant crwff)
  (mbed-quant 'forall vquant crwff))

(defun mbed-quant (quant vquant crwff)
  (declare (special edwff cmdstack wffstack))
  (if (eq quant 'lambda) 
      (if (wffeq crwff (edwin-find-top edwff cmdstack wffstack))
	  (list quant vquant crwff)
	  (msgf "WARNING: The top edwff after lambda embedding the current edwff 
will be ill-formed."))
      (list quant vquant crwff))
  (acons vquant quant crwff))
  
(defwffop mbed-implics-left
  (argtypes gwff0 gwff0)
  (resulttype gwff0)
  (argnames lgwff rgwff)
  (arghelp "Antecedent of implication" "Succedent of implication")
  (defaultfns (lambda (lgwff rgwff)
		(If (eq rgwff '$)   
		    (let ((global-type 'o))
		      (declare (special global-type))
		      (list lgwff (getwff-subtype 'gwff-p "x(O)")))
		    (list lgwff rgwff))))
  (mhelp "Embed the current edwff as the antecedent of a conditional. 
The consequent is provided by the user."))

(defun mbed-implics-left (lgwff rgwff)
  (mbed-implics 'implies lgwff rgwff))

(defwffop mbed-implics-right
  (argtypes gwff0 gwff0)
  (resulttype gwff0)
  (argnames lgwff rgwff)
  (arghelp "Antecedent of implication" "Succedent of implication")
  (defaultfns (lambda (lgwff rgwff)
		(If (eq lgwff '$)   
		    (let ((global-type 'o))
		      (declare (special global-type))
		      (list (getwff-subtype 'gwff-p "x(O)") rgwff))
		    (list lgwff rgwff))))
  (mhelp "Embed the current edwff as the consequent of a conditional. 
The antecedent is provided by the user."))

(defun mbed-implics-right (lgwff rgwff)
  (mbed-implics 'implies lgwff rgwff))

(defun mbed-implics (connective lgwff rgwff)
  (acons connective lgwff rgwff))

(defwffop mbed-or-left
  (argtypes gwff0 gwff0)
  (resulttype gwff0)
  (argnames lgwff rgwff)
  (arghelp "Left scope of OR" "Right scope of OR")
  (defaultfns (lambda (lgwff rgwff)
		(If (eq rgwff '$)   
		    (let ((global-type 'o))
		      (declare (special global-type))
		      (list lgwff (getwff-subtype 'gwff-p "x(O)")))
		    (list lgwff rgwff))))
  (mhelp "Embed the current edwff in the left scope of OR. 
The right scope is provided by the user."))

(defun mbed-or-left (lgwff rgwff)
  (mbed-conn 'or lgwff rgwff))

(defwffop mbed-or-right
  (argtypes gwff0 gwff0)
  (resulttype gwff0)
  (argnames rgwff lgwff)
  (arghelp "Right scope of OR" "Left scope of OR")
  (defaultfns (lambda (rgwff lgwff)
		(If (eq lgwff '$)   
		    (let ((global-type 'o))
		      (declare (special global-type))
		      (list rgwff (getwff-subtype 'gwff-p "x(O)")))
		    (list lgwff rgwff))))
  (mhelp "Embed the current edwff in the right scope of OR. 
The left scope is provided by the user."))

(defun mbed-or-right (lgwff rgwff)
  (mbed-conn 'or lgwff rgwff))

(defwffop mbed-lambda
  (argtypes gvar gwff)
  (resulttype gwff)
  (argnames vquant crwff)
  (arghelp "Variable of Quantification" "Current wff quantified over")
  (defaultfns (lambda (vquant crwff)
		(If (eq vquant '$)   
		    (let ((global-type 'i))
		      (declare (special global-type))
		      (list (getwff-subtype 'gwff-p "x(I)") crwff))
		    (list vquant crwff))))

 (mhelp "Embed the current edwff in the scope of lambda. 
The variable of quantification is provided by the user."))

(defun mbed-lambda (vquant crwff)
    (mbed-quant 'lambda vquant crwff))

(defwffop mbed-equiv-right
  (argtypes gwff0 gwff0)
  (resulttype gwff0)
  (argnames lgwff rgwff)
  (arghelp "Left side of equivalence" "Right side of equivalence")
  (defaultfns (lambda (lgwff rgwff)
		(If (eq lgwff '$)   
		    (let ((global-type 'o))
		      (declare (special global-type))
		      (list (getwff-subtype 'gwff-p "x(O)")  rgwff))
		    (list lgwff rgwff))))
(mhelp "Embed the current edwff on the right side of equivalence. 
The left side is provided by the user."))

(defun mbed-equiv-right (lgwff rgwff)
  (mbed-conn 'equiv lgwff rgwff))

  
(defwffop mbed-equiv-left
  (argtypes gwff0 gwff0)
  (resulttype gwff0)
  (argnames lgwff rgwff)
  (arghelp "Left side of equivalence" "Right side of equivalence")
  (defaultfns (lambda (lgwff rgwff)
		(If (eq rgwff '$)   
		    (let ((global-type 'o))
		      (declare (special global-type))
		      (list lgwff (getwff-subtype 'gwff-p "x(O)")))
		    (list lgwff rgwff))))
(mhelp "Embed the current edwff on the left side of equivalence. 
The right side is provided by the user."))

(defun mbed-equiv-left (lgwff rgwff)
  (mbed-conn 'equiv lgwff rgwff))

(defwffop mbed=left
  (argtypes gwff gwff)
  (resulttype gwff0)
  (argnames lgwff rgwff)
  (arghelp "Left side of =" "Right side of =")
  (defaultfns (lambda (lgwff rgwff)
		(If (eq rgwff '$)   
		    (let ((global-type (type lgwff)))
		      (declare (special global-type))
		      (list lgwff 
			    (create-propsym '\x (type lgwff))))
		    (list lgwff rgwff))))
(mhelp "Embed the current edwff on the left side of equality. 
The right side is provided by the user."))

(defun mbed=left (lgwff rgwff)
  (mbed=conn lgwff rgwff))

(defun mbed=conn (lgwff rgwff)
  (if (equal (type lgwff) (type rgwff))
      (acons (inherit-abbrev '= (acons 'O (type lgwff) (type rgwff))
			     (type lgwff))
	     lgwff rgwff)
    (throwfail t "Type mismatch. Cannot equate wffs of different types")))
  
(defwffop mbed=right
  (argtypes gwff gwff)
  (resulttype gwff0)
  (argnames rgwff lgwff)
  (arghelp "Left side of =" "Right side of =")
  (defaultfns (lambda (rgwff lgwff)
		(If (eq lgwff '$)   
		    (let ((global-type (type rgwff)))
		      (declare (special global-type))
		      (list rgwff (create-propsym '\x (type rgwff))))
		    (list rgwff lgwff))))
(mhelp "Embed the current edwff on the right side of equality. 
The left side is provided by the user."))

(defun mbed=right (rgwff lgwff)
  (mbed=conn  lgwff rgwff))

