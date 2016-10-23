;;; -*- Mode:LISP; Package:MAINT -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :MAINT)
(part-of REPORT)

(deffile report-init
  (part-of report)
  (extension lisp)
  (mhelp "Modes for running REPORT on different records."))

;;; This file contains modes to run REPORT on different records.

(defmode etps2
  (flag-settings
   (advice-file "tpsrec:etps2.advice")
   (command-file "tpsrec:etps2.command")
   (error-file "tpsrec:etps2.error")
   (input-error-file "tpsrec:etps2.ierror")
   (proof-file "tpsrec:etps2.proof")
   (remarks-file "tpsrec:etps2.remarks")
   (rule-error-file "tpsrec:etps2.rerror")
   (score-file "tpsrec:etps2.scores"))
  (mhelp
"Mode to take events from the records from current record files for ETPS."))

(defmode stps2
  (flag-settings
   (advice-file "tpsrec:stps2.advice")
   (command-file "tpsrec:stps2.command")
   (error-file "tpsrec:stps2.error")
   (input-error-file "tpsrec:stps2.ierror")
   (proof-file "tpsrec:stps2.proof")
   (remarks-file "tpsrec:stps2.remarks")
   (rule-error-file "tpsrec:stps2.rerror")
   (score-file "tpsrec:stps2.scores"))
  (mhelp
"Mode to take events from the records from current record files for STPS."))


(defmode etps2-s86-c
  (flag-settings
   (advice-file "tpsrec:etps2.advice")
   (command-file "tpsrec:etps2.command")
   (error-file "tpsrec:etps2.error")
   (input-error-file "tpsrec:etps2.ierror")
   (proof-file "tpsrec:etps2.proof")
   (remarks-file "tpsrec:etps2.remarks")
   (rule-error-file "tpsrec:etps2.rerror")
   (score-file "tpsrec:etps2.scores"))
  (mhelp
"Mode to take events from the records from current record files for ETPS
on CMUC (Spring 86)."))


(defmode etps2-s86-tf
  (flag-settings
   (advice-file "tpsrec:etps2-s86-tf.advice")
   (command-file "tpsrec:etps2-s86-tf.command")
   (error-file "tpsrec:etps2-s86-tf.error")
   (input-error-file "tpsrec:etps2-s86-tf.ierror")
   (proof-file "tpsrec:etps2-s86-tf.proof")
   (remarks-file "tpsrec:etps2-s86-tf.remarks")
   (rule-error-file "tpsrec:etps2-s86-tf.rerror")
   (score-file "tpsrec:etps2-s86-tf.scores"))
  (mhelp "Mode to take events from the records from the Logic I class/
in the spring of 1986."))

(defmode etps2-f85-tf
  (flag-settings
   (advice-file "tpsrec:etps2-f85-tf.advice")
   (command-file "tpsrec:etps2-f85-tf.command")
   (error-file "tpsrec:etps2-f85-tf.error")
   (input-error-file "tpsrec:etps2-f85-tf.ierror")
   (proof-file "tpsrec:etps2-f85-tf.proof")
   (remarks-file "tpsrec:etps2-f85-tf.remarks")
   (rule-error-file "tpsrec:etps2-f85-tf.rerror")
   (score-file "tpsrec:etps2-f85-tf.scores"))
  (mhelp "Mode to take events from the records from the Logic I class/
in the fall of 1985."))


(defmode etps2-f85-c
  (flag-settings
   (advice-file "tpsrec:etps2-f85-c.advice")
   (command-file "tpsrec:etps2-f85-c.command")
   (error-file "tpsrec:etps2-f85-c.error")
   (input-error-file "tpsrec:etps2-f85-c.ierror")
   (proof-file "tpsrec:etps2-f85-c.proof")
   (remarks-file "tpsrec:etps2-f85-c.remarks")
   (rule-error-file "tpsrec:etps2-f85-c.rerror")
   (score-file "tpsrec:etps2-f85-c.scores"))
  (mhelp "Mode to take events from the records from ETPS on CMUC
during the Logic I class in the fall of 1985."))


(defmode stps2-f85-td
  (flag-settings
   (advice-file "tpsrec:stps2-f85-td.advice")
   (command-file "tpsrec:stps2-f85-td.command")
   (error-file "tpsrec:stps2-f85-td.error")
   (input-error-file "tpsrec:stps2-f85-td.ierror")
   (proof-file "tpsrec:stps2-f85-td.proof")
   (remarks-file "tpsrec:stps2-f85-td.remarks")
   (rule-error-file "tpsrec:stps2-f85-td.rerror")
   (score-file "tpsrec:stps2-f85-td.scores"))
  (mhelp "Mode to take events from the records from the Logic class/
of the philosphy department in the fall of 1985."))


(defmode stps2-f85-c
  (flag-settings
   (advice-file "tpsrec:stps2-f85-c.advice")
   (command-file "tpsrec:stps2-f85-c.command")
   (error-file "tpsrec:stps2-f85-c.error")
   (input-error-file "tpsrec:stps2-f85-c.ierror")
   (proof-file "tpsrec:stps2-f85-c.proof")
   (remarks-file "tpsrec:stps2-f85-c.remarks")
   (rule-error-file "tpsrec:stps2-f85-c.rerror")
   (score-file "tpsrec:stps2-f85-c.scores"))
  (mhelp "Mode to take events from the records from STPS on CMUC
during the Logic class of the philosphy department in the fall of 1985."))

