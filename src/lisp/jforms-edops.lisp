;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
;(part-of VPFORMS)

;(part-of vpforms)

(deffile jforms-edops
  (part-of jforms)
  (extension lsp)
  (mhelp "Jform-Wff conversion commands."))

(context jforms1)

(defedop djform
  (alias jform-to-gwff)
  (edwff-argname jform)
  (result-> edwff))

(defedop cjform
  (alias gwff-to-jform)
  (edwff-argname gwff)
  (result-> edwff))

(defwffop number-of-vertical-paths
  (argtypes gwff)
  (argnames gwff)
  (resulttype integer+)
  (mhelp "Counts the number of vertical paths through the given jform."))
	 
(defedop num-vpaths
  (alias number-of-vertical-paths)
  (edwff-argname gwff)
  (result-> princ))

(defun number-of-vertical-paths (gwff)
  (number-of-vertical-paths-main
   (ck-and-convert-gwff-to-jform gwff)))

(defun number-of-vertical-paths-main (jform)
  (case (type-of jform)
    (literal 1)
    ((universal existential)
     (number-of-vertical-paths-main (universal-scope jform)))
    (disjunction
     (if (disjunction-components jform)
         (apply-fn-with-key #'+ (disjunction-components jform)
                            #'number-of-vertical-paths-main)
         0))
    (conjunction
     (if (conjunction-components jform)
         (apply-fn-with-key #'* (conjunction-components jform)
                            #'number-of-vertical-paths-main)
         1))))
    
(defwffop number-of-horizontal-paths
  (argtypes gwff)
  (argnames gwff)
  (resulttype integer+)
  (mhelp "Counts the number of horizontal paths through the given jform."))
	 
(defedop num-hpaths
  (alias number-of-horizontal-paths)
  (edwff-argname gwff)
  (result-> princ))

(defun number-of-horizontal-paths (gwff)
  (number-of-horizontal-paths-main 
   (ck-and-convert-gwff-to-jform gwff)))

(defun number-of-horizontal-paths-main (jform)
  (case (type-of jform)
    (literal 1)
    ((universal existential)
     (number-of-horizontal-paths-main (universal-scope jform)))
    (disjunction
     (if (disjunction-components jform)
         (apply-fn-with-key #'* (disjunction-components jform)
                            #'number-of-horizontal-paths-main)
         1))
    (conjunction
     (if (conjunction-components jform)
         (apply-fn-with-key #'+ (conjunction-components jform)
                            #'number-of-horizontal-paths-main)
         0))))

(defwffop print-jlist
  (argtypes gwff)
  (argnames gwff)
  (print-op T)
  (resulttype ignore)
  (mhelp "Prints the given gwff, using lists for jforms."))

(defun print-jlist (gwff)
  (prtwff gwff (ppwfflag nil)))

(defedop pj
  (alias print-jlist)
  (edwff-argname gwff)
  (result-> ignore))

(defedop prop-cjform
  (alias gwff-to-jform)
  (edwff-argname gwff)
  (result-> edwff))

