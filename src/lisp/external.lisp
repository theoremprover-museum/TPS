;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of external-services)
(context mating-search)

; File created July 27, 2001 - cebrown
; This file and its contents were created to better support
; the integration of TPS and Omega.  It is intended to support
; providing the ability for TPS to process requests from
; any external program.  These requests should be very simple,
; in order to hide internal details about the operation of TPS
; (as these details are subject to change).
(deffile external
  (part-of external-services)
  (extension clisp)
  (mhelp "Defines functions for providing services for external programs
that wish to call tps.  In particular, this is used to communicate
with Omega.  This file is only supported for Allegro at the moment,
because it makes use of Allegro multiprocessing."))

; m is a string - message out
(defun external-output (message)
  (socket~write message :inout))

(defun external-send-proof (mainprocname)
  (external-output
   (let ((str1 (make-string-output-stream))
	 (str2 (make-string-output-stream)))
     (let ((*standard-output* str1))
       (core::write-saved-proof dproof))
     (let ((*standard-output* str2)
	   (STYLE 'GENERIC))
       (pall))
     (prog1
	 (format nil "(~S ~S ~S)~%"
		 mainprocname
		 (get-output-stream-string str1)
		 (get-output-stream-string str2))
       (close str1)
       (close str2)))))


; for now this does nothing, but should perform a Mathweb request
; such as fetching an abbreviation.
(defun process-mathweb-request (r)
  (if (consp r)
      (cond ((eq (car r) 'MBASE-FETCH)
             (get-mbase-definitions-for (cadr r) (caddr r)))
            (t nil))
                                        ; e.g., (serv=operation r) ?
    nil))

; (def-abbrev PU-LT
;       (type "OAA(AA)(AA)(AAA)A")
;       (type (type LAMBDA Z(A) LAMBDA P(AAA) LAMBDA L(AA) LAMBDA R(AA) LAMBDA x(A) LAMBDA y(A) EXISTS X(OA).X(OA) [P(AAA) x(A) y(A)] AND FORALL t(A) FORALL u(A).X(OA) [P(AAA) t(A) u(A)] IMPLIES [u(A) =(OAA) Z(A) IMPLIES t(A) =(OAA) Z(A)] AND X(OA) [P(AAA) [L(AA) t(A)].L(AA) u(A)] AND X(OA).P(AAA) [R(AA) t(A)].R(AA) u(A)))
;       (typelist ("A"))
;       (fo-single-symbol t)
;       (defn "LAMBDA Z(A) LAMBDA P(AAA) LAMBDA L(AA) LAMBDA R(AA) LAMBDA x(A) LAMBDA y(A) EXISTS X(OA).X(OA) [P(AAA) x(A) y(A)] AND FORALL t(A) FORALL u(A).X(OA) [P(AAA) t(A) u(A)] IMPLIES [u(A) =(OAA) Z(A) IMPLIES t(A) =(OAA) Z(A)] AND X(OA) [P(AAA) [L(AA) t(A)].L(AA) u(A)] AND X(OA).P(AAA) [R(AA) t(A)].R(AA) u(A)")
                                        ;       )
;; (auto::get-mbase-definitions-for 'tps 'union)
;; (auto::get-mbase-definitions-for 'tps 'transitive)
(defun get-mbase-definitions-for (theory symbol)
  (serv~enter "MBASE")
  (let* ((query (format nil
                        "getDefinitions(theory: \"~A\" name: \"~A\" matchType: exact return: formula)"
                        theory
                        symbol))                       
         (definition-strings (serv~apply "MBASE" query))
         (formula-strs (read-from-string definition-strings))
         )
    (format t "query: ~S" query)
    (when formula-strs
      (format t "Definition from MBase: ~S" formula-strs)
      (format t "Strings : ~S" definition-strings)
      (format t "Type: ~A" (type (getrwff (first formula-strs))))
      ;;      (first formula-strs)
      ;;   (def-abbr-quick <name:symbol> <parsed formula> <help:string>)
      (def-abbr-quick symbol (getrwff (first formula-strs)))
      )))

        

; we could make this a mexpr, but it seems more reasonable to make it
; a command line option, since an image providing services should
; not be doing anything else.  (Since, eg, local changes in flag values could
; affect the work on an external problem.)
;(defmexpr provide-external-services
;  (argtypes string filespec filespec) ; change to use Mathweb
;  (argnames name tps-in tps-out)
;  (arghelp "Name for the server image"
;           "Filename for finding requests" ; change to use Mathweb
;	   "Filename for outputs for requests") ; change to use Mathweb
;  (mhelp "This starts a process to look for requests from
;an external program.  The external program can spawn processes
;to try to prove a theorem in a certain amount of time,
;give more time to a process (or take time away),
;and kill a process.
;
;We can only process one request at a time since a request
;may change global flags (like default-ms) for every process running.
;
;It uses Allegro's multiprocessing facilities to handle starting
;the process and killing the process separately."))
;
; services provided:
;
; 1.  <TPS-NAME> 
;     BASIC-DIY <PROCNAME (string)>
;                <Mathweb requests - eg, to fetch abbrevs - always NIL for now> 
;                <defsavedproof (incomplete proof)>
;                <MODE> <MS> <TIMEOUT-IN-SECS>
; Mainly starts a process to prove the planned wff of the incomplete proof:
; Sets USE-RULEP to NIL, REMOVE-LEIBNIZ to NIL,
; MODE to <MODE>, DEFAULT-MS to <MS> (unless "").
; Then calls DIY.  If successful, we double-check
; that no subst='s and RuleP's occur (eliminating them if they do),
; and sending the proof object out to the external program.
; It also starts a process to kill of the process trying to
; prove the wff if it runs out of time.
; BASIC-DIY is used by Omega.
;
; 2. <TPS-NAME> 
;     DIY <PROCNAME (string)>
;                <Mathweb requests - eg, to fetch abbrevs - always NIL for now> 
;                <defsavedproof (incomplete proof)>
;                <MODE> <MS> <TIMEOUT-IN-SECS>
; Like BASIC-DIY, but without setting USE-RULEP NIL and REMOVE-LEIBNIZ NIL,
; and at the end we allow the proof to use RuleP and equality reasoning.
;
; 2. <TPS-NAME> ADJUSTTIME <PROCNAME (string)> <SECS>
; gives the process more (or less) time according to the value of <secs>
;
; 3. <TPS-NAME> KILL <PROCNAME (string)>
; kills the process
;
; 4. PING <PROCNAME (string)> - any TPS server can respond to this
; with a PONG <PROCNAME> <TPS-NAME>, so that the external program
; can send a request to that particular TPS server.

(defvar *external-request-queue-process* nil)
(defvar *external-request-queue* nil)
(defvar *external-main-process* nil)
(defvar *timeout-process* nil)
(defvar *killing-time* nil)
(defvar *last-pong-time* nil)

; #-:(and allegro-version>= (version>= 5 0))
; (defun enqueue-external-request (info)
;   (throwfail "Sorry.  External Services are not Provided by the version of TPS
; under this LISP."))

#-:(and allegro-version>= (version>= 5 0))
(defun process-request-queue ()
  (throwfail "Sorry.  External Services are not Provided by the version of TPS
under this LISP."))

#-:(and allegro-version>= (version>= 5 0))
(defun process-external-request (info)
  (throwfail "Sorry.  External Services are not Provided by the version of TPS
under this LISP."))

; either processes the next request immediately or
; puts it on the queue
#+:(and allegro-version>= (version>= 5 0))
(defun enqueue-external-request (info)
  (declare (special *external-main-process*))
  (cond ((atom info)
	 (msgf "Did not understand atom:" info))
	((string-equal (symbol-name (car info)) "PING")
	   (msgf "*** RECEIVED A PING FROM " (cadr info) " ***")
	   (unless (or *external-request-queue*
		       (and *external-main-process*
			    (or
			     (mp:process-active-p
			      *external-main-process*)
			     (mp:process-arrest-reasons
			      *external-main-process*)))
		       (and *last-pong-time*
			    (< (- (get-universal-time) *last-pong-time*)
			       10)))
	     (msgf "*** REPLYING WITH A PONG ***")
	     (setq *last-pong-time* (get-universal-time))
	     (external-output
	      (format nil "(~S PONG ~S)~%"
		      (cadr info)
		      *tps-server-name*))))
	  ((string-equal (car info) *tps-server-name*) ; req for this server
	   (let ((newprocname (format nil "~d" (caddr info))))
	     (mp:process-add-arrest-reason *external-request-queue-process*
					   'processing-new)
	     (if (and *external-main-process*
		      (string= (mp:process-name
			      *external-main-process*) ; this request will be processed now 
			     newprocname))    ; because it involves the
					; running request
	       (process-external-request info)
	     (setq *external-request-queue* (nconc *external-request-queue*
						   (list 
						    info))))
	   (mp:process-revoke-arrest-reason *external-request-queue-process*
					    'processing-new)))
	(t nil))) ; do nothing, request was not for this tps
 

#+:(and allegro-version>= (version>= 5 0))
(defun process-request-queue ()
  (declare (special *external-request-queue* *external-main-proc*))
  (loop while t do
	(mp:process-wait "waiting to process"
			 #'(lambda ()
			     (and *external-request-queue*
				  (not (and *external-main-process*
					    (or (mp:process-active-p
						 *external-main-process*)
						(mp:process-arrest-reasons
						 *external-main-process*)))))))
	(process-external-request (pop *external-request-queue*))))
      
#+:(and allegro-version>= (version>= 5 0))
(defun process-external-request (info)
  (declare (special *external-main-process* *timeout-process* *killing-time*))
  (unwind-protect
      (let* ((req (cdr info))
	     (mainprocname (format nil "~d" (cadr req)))
	     (timeoutprocname (format nil "~d.timeout" mainprocname)))
	(cond ((member (car req) '("BASIC-DIY" "DIY") :test #'string-equal)
	       (msgf "*** WORKING ON " mainprocname " ***")
	       (let ((resource-in-sec (nth 6 req)))
		 (when *external-main-process*
		   (mp:process-kill *external-main-process*)
		   (setq *external-main-process* nil))
		 (when *timeout-process*
		   (mp:process-kill *timeout-process*)
		   (setq *timeout-process* nil))
		 (setq *killing-time* (+ resource-in-sec (get-universal-time)))
		 (setq *external-main-process*
		       (mp:process-run-function
			mainprocname
			#'(lambda ()
			    (let ((dsp (nth 3 req)) 
				  (m (nth 4 req))
				  (ms (nth 5 req)))
			      (dolist (mathweb-request (nth 2 req))
				(process-mathweb-request mathweb-request))
			      (when (and (consp dsp)
					 (eq (car dsp) 'defsavedproof))
				(setf (proof-lines (cadr dsp)) nil) ; just overwrite an existing
					; proof if nec
				(set-flag 'QUERY-USER NIL)
				(eval dsp)
				(when (or (core::retrieve-libobject m :type 'mode1 :multiple nil
								    :fail-gently t)
					  (core::retrieve-libobject m :type 'mode
								    :multiple nil
								    :fail-gently t))
				  (mode m)
				  (set-flag 'QUERY-USER NIL)
				  (when (and ms (not (string-equal ms "")))
				    (set-flag 'default-ms ms))
				  (set-flag 'ALLSCOPEFLAG T)
				  (set-flag 'PRINTTYPES T)
				  (set-flag 'PRINTTYPES-ALL T)
				  (set-flag 'INFIX-NOTATION NIL)
				  (when (string-equal (car req) "BASIC-DIY")
				    (set-flag 'USE-RULEP 'NIL)
				    (set-flag 'REMOVE-LEIBNIZ 'NIL))
				  (diy (caar (proof-plans dproof)) nil nil t)
				  (unless (proof-plans dproof)
				    (when (string-equal (car req) "BASIC-DIY")
				      (expand-to-basic-pf))
				    (cleanup)
				    (remove-unnecessary-gaps)))
				(mp:process-kill *timeout-process*) ; so we don't "fail
								    ; due to time out" and
								    ; send whatever dproof
								    ; is at that time
				(msgf "*** " mainprocname " SUCCEEDED, SENDING PROOF ***")
				(external-send-proof mainprocname)
				)))))
		 (setq *timeout-process*
		       (mp:process-run-function
			timeoutprocname
			#'(lambda ()
			    (mp:process-sleep resource-in-sec)
			    (mp:process-kill *external-main-process*)
			    (msgf "*** " mainprocname " FAILED DUE TO TIMEOUT ***")
			    (external-send-proof mainprocname) ; make sure to send it the partial
						  ; proof, in case it's really waiting
			    )))))
	      ((string-equal (car req) "KILL")
	       (msgf "*** KILLING " mainprocname " AS REQUESTED ***")
	       (when *external-main-process*
		 (mp:process-kill *external-main-process*)
		 (setq *external-main-process* nil))
	       (when *timeout-process*
		 (mp:process-kill *timeout-process*)
		 (setq *timeout-process* nil)))
	      ((string-equal (car req) "ADJUSTTIME")
	       (when *timeout-process*
		 (msgf "*** ADJUSTING TIME FOR " mainprocname " AS REQUESTED ***")
		 (mp:process-kill *timeout-process*)
		 (setq *killing-time* (+ (caddr req) *killing-time*))
		 (let ((new-rem-time (- *killing-time* 
					(get-universal-time))))
		   (msgf "Starting new timeout with " new-rem-time " secs left")
		   (setq *timeout-process*
			 (mp:process-run-function
			  timeoutprocname
			  #'(lambda ()
			      (mp:process-sleep new-rem-time)
			      (mp:process-kill *external-main-process*)))))))


					; the rest are requests that can be
					; run in parallel,
					; so we don't need to queue them
	      ((string-equal (car req) "MP-PROVE-SEARCH-STATE")
	       (let ((cs (read-search-state-comm (cadr req))))
		 (mp:process-run-function
		  (format nil "MP~A" (search-state-name cs))
		  #'phase-search
		  cs)))
	      ((string-equal (car req) "MP-PRUNE-SEARCH-STATE")
	       (let ((cs (read-search-state-comm (cadr req))))
		 (mp:process-run-function
		  (format nil "MP~A-prune" (search-state-name cs))
		  #'prune-search-state
		  cs
		  (caddr req))))
	      ((string-equal (car req) "SET-MP-VAL")
	       (set-mp-val (cadr req) (caddr req)))
	      ((string-equal (car req) "KILL-ALL-MPS")
	       (dolist (p sys:*all-processes*)
		       (let ((pn (mp:process-name p)))
			 (when (string= pn "MP" :start1 0 
					:end1 (max 2 (length pn)))
			   (mp:process-kill p)))))
	      ((string-equal (car req) "NEW-GENSYM")
	       (setf (get (cadr req) 'gensym) (caddr req)))
	      (t (msgf "Did not understand request: " req))))
    nil))
  
(defun expand-to-basic-pf ()
  (unless (proof-plans dproof)
					; first expand ='s as Leibniz:
    (let ((subst=-lines (remove-if-not #'(lambda (x) (member (car (line-justification x))
							     '("Subst=" "Sub=" "Assert SYM=" "Sym=")
							     :test #'string=))
				       (proof-lines dproof))))
      (auto::remove-subst=-lines subst=-lines))
					; then get rid of rulep's
    (eliminate-all-rulep-apps)
    (auto::expand-cases)))


