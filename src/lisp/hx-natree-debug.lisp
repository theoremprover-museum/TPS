;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
(part-of etr-nat)

(context etr-nat)

(defflag natree-debug
  (flagtype boolean)
  (default nil)
  (subjects mating-search ms90-3 ms90-9 ms91-7 ms91-6 ms89 ms88 ms92-9 ms93-1)
  (mhelp "To invoke the debugging facilities mentioned in the Programmers
Guide associated with NAT-ETREE. If NATREE-VERSION is set to CEB and
NATREE-DEBUG is set to T, then the code doublechecks that a mating exists,
giving the user lots of information.  This should eventually evolve into
a flag with more choices."))

(defun xattach-dup-info-to-natree (natree dup-info)
  (let ((natree-debug T))
    (display-dup-info dup-info))
  (attach-dup-info-to-natree natree dup-info))


(defun xupward-update-natree (natree dup-info)
  (let ((natree-debug T))
    (display-dup-info dup-info))
  (upward-update-natree natree dup-info))

(defun xupdate-dup-info (dup-info)
  (xdisplay-dup-info dup-info)
  (let ((new-dup-info (update-dup-info dup-info)))
    (xdisplay-dup-info new-dup-info)
    new-dup-info))

(defun attention ()
  (msgf "*****************************************"))

(defun xdisplay-dup-info (dup-info)
  (let ((*ignore-statuses* T))
    (msgf "*************** BEGIN ******* BEGIN *****************"))
    (display-vp-diag (etree-to-jform dup-info))
    (display-etree-all dup-info)
    (msgf "**************** END ********* END ******************"))