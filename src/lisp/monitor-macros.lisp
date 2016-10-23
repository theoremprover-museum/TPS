;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of MATING)

;;;
;;; File : Monitor
;;; 

(deffile monitor-macros
  (part-of mating)
  (extension clisp)
  (mhelp "Defines the defmonitor command and all related functions."))


;; Explanation:
;; The user/programmer will define "monitor functions" as outlined below. These monitor functions
;; are in essence mexprs; they take a list of arguments from the user and then set the 
;; *current-monitorfn* to the name of a function and the *current-monitorfn-params* to some 
;; data structure corresponding to the user's inputs. 
;; The flag MONITORFLAG will be set to T if monitoring is switched on. 
;; At various strategic points in the code the user may add breakpoints like this:
;; (if monitorflag (funcall (symbol-function *current-monitorfn*) 'here alist))
;; where 'here is the "name" of the current point in the code --- for example 'added-conn or
;; 'complete-mating or 'unifying or whatever, and alist is an association list of potentially 
;; relevant objects in the form ((name1 . object1) (name2 . object2) ...)
;;
;; the function *current-monitorfn* is envisioned as checking to see whether 'here is the 
;; relevant point in the code, returning control immediately if it is not, and performing
;; some actions using the user's input (stored in *current-monitorfn-params*) and the given
;; alist. For example, suppose we want to run quietly until we reach a certain mating. Then the
;; monitorfn might check to see whether 'here is 'complete-mating, then if it is
;; look for the pair (current-mating . xxxxx) in the alist, and compare xxxxx to some mating 
;; previously supplied by the user, taking action if the comparison succeeds.
;;
;;
;; HOW TO DEFINE A MONITOR FUNCTION:
;; 
;; 1. The main function should be defined with a defmonitor command; this is just like a defmexpr,
;;    and should take all necessary arguments from the user and/or from the current configuration
;;    of the system (e.g. it might read the current values of some flags, or the current mating).
;;    By convention, use the "mainfn" property and give the function part of the defmonitor command
;;    a *different* name to the command itself; then we can use the name of the command as the name 
;;    of the actual function that does the work. (Look at the examples in monitor.lisp if this isn't clear)
;;
;; 2. This function should set *current-monitorfn* to a symbol corresponding to the name of the 
;;    function to be called at each breakpoint in the code.
;;    It should set *current-monitorfn-params* to whatever it likes; this is just for storing the 
;;    user's input until it is needed.
;;    It should set *monitorfn-params-print* to a symbol corresponding to a function that can 
;;    print out *current-monitorfn-params* in a coherent manner, or to nil (this is used by MONITOR
;;    and NOMONITOR).
;;
;; 3. The actual function which does the work, which by convention has the same name as the defmonitor
;;    command, should accept two arguments - a symbol denoting the location of the breakpoint
;;    that did the calling, and an association list containing pairs of (name . value) for any
;;    local variables that would otherwise be unaccessible to the function (no need to pass global variables,
;;    since the function has access to those anyway. If this function makes any permanent changes, it's 
;;    up to the programmer to make sure these aren't harmful.
;;
;; 4. Insert breakpoints in the code, of the form 
;;    (if monitorflag (funcall (symbol-function *current-monitorfn*) 'whats-happening local-var-alist))
;;    These should be left in the code permanently from then onwards.


(context mating-search)

(defvar *current-monitorfn* nil)
(defvar *current-monitorfn-params* nil)
(defvar *monitorfn-params-print* nil)
;(defvar global-monitorlist nil)

(defflag monitorflag
  (flagtype boolean)
  (default nil)
  (subjects mating-search)
  (mhelp "The monitor is switched on if this flag is T and off if it is NIL.
This flag is set by the command MONITOR, and unset by the command NOMONITOR
(and may of course also be set manually)."))

(defmexpr monitor
  (mhelp "Turns the monitor on, and prints out the current monitor 
function and parameters. See NOMONITOR. See also QUERY-USER for an
alternative way to monitor the progress of the matingsearch.
For a list of monitor functions, type MONITORLIST. To change the current
monitor function, enter the name of the desired new monitor function from
the main top level or the mate top level."))

(defun monitor ()
  (setq monitorflag t)
  (msg "The monitor is now ON." t) 
  (if *current-monitorfn* (msg "The current monitor function is " *current-monitorfn* t)
    (msg "No monitor function is currently in use."))
  (if (and *current-monitorfn* *monitorfn-params-print*)
      (funcall (symbol-function *monitorfn-params-print*) *current-monitorfn-params*) t))


(defmexpr nomonitor
  (mhelp "Turns the monitor off, and prints out the current monitor 
function and parameters. See MONITOR.
For a list of monitor functions, type MONITORLIST. To change the current 
monitor function, enter the name of the desired new monitor function from
the main top level or the mate top level."))

(defun nomonitor ()
  (setq monitorflag nil)
  (msg "The monitor is now OFF." t) 
  (if *current-monitorfn* (msg "The current monitor function is " *current-monitorfn* t)
    (msg "No monitor function is currently in use."))
  (if (and *current-monitorfn* *monitorfn-params-print*)
      (funcall (symbol-function *monitorfn-params-print*) *current-monitorfn-params*) t))

(defmexpr monitorlist
  (mhelp "List all monitor functions."))

(defun monitorlist ()
  (declare (special global-monitorlist))
  (msgf)
  (dolist (mon global-monitorlist (msgf))
    (if (symbolp mon) (msg mon " ")))
  (msg t "To change the current monitor function to one of these functions, " t 
"just type the name of the required monitor function as a command from either" t 
"the main top level or the mate top level."))

(defcategory monitorfn
  (define defmonitor)
  (properties
   (argtypes multiple)
   (argnames multiple)
   (arghelp multiple)
   (defaultfns multiplefns)
   (mainfns multiplefns)
   (print-command single)
   (dont-restore single)
   (mhelp single))
  (global-list global-monitorlist)
  (shadow t)
  (mhelp-line "monitor function")
  (mhelp-fn (command-mhelp monitorfn "<n>" category))
  (scribe-one-fn
   (lambda (item)
     (maint::scribe-doc-command
      (format nil "@IndexOther(~A)" (symbol-name item))
      (get item 'mon-argnames)
      (cdr (assoc 'monitorfn (get item 'mhelp)))))))
