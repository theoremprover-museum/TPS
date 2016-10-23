;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
;(part-of mating-search)
;(part-of ms88)

;(context mating-search)
(context ms88)

(deffile mating-macros
; (part-of mating-search)
  (part-of mating)
 (extension lisp)
 (mhelp "Contains macros needed for mating search."))



(defmacro max-cgraph-counter ()
  `(eproof-max-cgraph-counter master-eproof))
 
(defmacro bktrack-limit ()
  `(eproof-bktrack-limit current-eproof))

(defmacro connections-array ()
  `(eproof-connections-array master-eproof))

(defmacro incomp-clists-wrt-etree ()
  `(eproof-incomp-clists-wrt-etree current-eproof))

(defmacro mating-list ()
  `(eproof-mating-list current-eproof))

(defmacro incomp-clists ()
  `(eproof-incomp-clists master-eproof))

(defmacro cgraph ()
  `(eproof-cgraph master-eproof))

(defmacro max-incomp-clists-wrt-etree ()
  `(eproof-max-incomp-clists-wrt-etree current-eproof))

