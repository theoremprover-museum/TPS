;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)

(context embedding)

(deffile edmbed
  (part-of wff-editor)
  (extension clisp)
  (mhelp "Contains editor operations to embed the current gwff
within the scope of a connective or quantifier."))

(defedop mbed-al
  (alias mbed-and-left)
  (result-> edwff)
  (mhelp "Embed the current edwff in the left scope of AND. 
The right scope is provided by the user.")
  (edwff-argname lgwff))
  
(defedop mbed-ar
  (alias mbed-and-right)
  (result-> edwff)
  (mhelp "Embed the current edwff in the right scope of AND. 
The left scope is provided by the user.")
  (edwff-argname rgwff))
  
(defedop mbed-il
  (alias mbed-implics-left)
  (result-> edwff)
  (mhelp "Embed the current edwff as the antecedent of a conditional. 
The consequent is provided by the user.")
  (edwff-argname lgwff))

(defedop mbed-ir
  (alias mbed-implics-right)
  (result-> edwff)
  (mhelp "Embed the current edwff as the consequent of a conditional. 
The antecedent is provided by the user.")
  (edwff-argname rgwff))
  
(defedop mbed-ol
  (alias mbed-or-left)
  (result-> edwff)
  (mhelp "Embed the current edwff in the left scope of OR. 
The right scope is provided by the user.")
  (edwff-argname lgwff))
  
(defedop mbed-or
  (alias mbed-or-right)
  (result-> edwff)
  (mhelp "Embed the current edwff in the right scope of OR. 
The left scope is provided by the user.")
  (edwff-argname rgwff))
  
(defedop mbed-f
  (alias mbed-forall)
  (result-> edwff)
  (mhelp "Embed the current edwff in the scope of a universal quantifier. 
The variable of quantification is provided by the user.")
  (edwff-argname crwff))

(defedop mbed-e
  (alias mbed-existential)
  (result-> edwff)
  (mhelp "Embed the current edwff in the scope of an existential quantifier. 
The variable of quantification is provided by the user.")
  (edwff-argname crwff))

(defedop mbed-e1
  (alias mbed-existential1)
  (result-> edwff)
  (mhelp "Embed the current edwff in the scope of an exists1 quantifier. 
The variable of quantification is provided by the user.")
  (edwff-argname crwff))
  
(defedop mbed-l
  (alias mbed-lambda)
  (result-> edwff)
  (mhelp "Embed the current edwff in the scope of lambda. 
The variable of quantification is provided by the user.")
  (edwff-argname crwff))

(defedop mbed-ql
  (alias mbed-equiv-left)
  (result-> edwff)
  (mhelp "Embed the current edwff on the left side of equivalence. 
The right side is provided by the user.")
  (edwff-argname lgwff))

(defedop mbed-qr
  (alias mbed-equiv-right)
  (result-> edwff)
  (mhelp "Embed the current edwff on the right side of equivalence. 
The left side is provided by the user.")
  (edwff-argname rgwff))

(defedop mbed=r
  (alias mbed=right)
  (result-> edwff)
  (mhelp "Embed the current edwff on the right side of equality. 
The left side is provided by the user.")
  (edwff-argname rgwff))

(defedop mbed=l
  (alias mbed=left)
  (result-> edwff)
  (mhelp "Embed the current edwff on the left side of equality. 
The right side is provided by the user.")
  (edwff-argname lgwff))
