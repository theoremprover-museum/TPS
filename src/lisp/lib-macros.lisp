;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)

;;;
;;; File: LIB-macros
;;; Package: library
;;;

(deffile lib-macros
  (part-of library)
  (extension lisp)
  (mhelp "Defines LIBRARY operations."))

(context library)

(defcategory libobject
  (define deflibobject)
  (properties
    (lib-promptfn singlefn)
    (lib-descr-readfn singlefn)
    (lib-attr-readfn singlefn)
    (lib-tpsobject single)
    (lib-printfn singlefn)
    (mhelp single))
  (global-list global-libobject-list)
  (mhelp-line "library object")
  (mhelp-fn princ-mhelp))

(defstruct libitem
  name
  type
  context
  keywords
  provability
  proof-date
  description
  mhelp
  other-attributes
  other-remarks
  (needed-objects nil)
  file)

(defstruct (libclass
	    (:print-function (lambda (d s k)
			       (declare (ignore k))
			       (format s "~d" (libclass-name d)))))
  libitems
  name
  parents
  kids)

; most deftype%'s are in argtyp*.lisp, but I put this here since
; it could cause problems defining the type without the struct
; (e.g., ETPS doesn't include the library, but does load argtyp.lisp).
; - cebrown 6/30/03
(deftype% libclass
  (getfn (lambda (x)
	   (if (libclass-p x)
	       x
	     (if (and (symbolp x) (libclass-p (get x 'libclass)))
		 (get x 'libclass)
	       (build-class-scheme x)))))
  (testfn (lambda (x) (libclass-p x)))
  (printfn princ)
  (mhelp "A libclass is a directed acyclic graph (DAG) classifying objects
in the library.  A classification scheme for the library is a libclass
along with a direction (up or down)."))

(defvar *current-class* nil)

(putprop 'lib-argtype 'global-libobject-list 'global-list)

(defflag default-libfile-type
  (flagtype string)
  (default "lib")
  (subjects library)
  (mhelp "The default value for the extension of library files."))

(defflag default-libindex-type
  (flagtype string)
  (default "rec")
  (change-fn (lambda (flag value old-value)
	       (declare (ignore flag old-value)
			(special lib-masterindex-file default-lib-dir))
	       (setq lib-masterindex-file 
		     (merge-pathnames (make-pathname% :type value)
				      lib-masterindex-file))))
  (subjects library)
  (mhelp "The default value for the extension of library index files."))

(defflag default-lib-dir
  (flagtype dirspeclist)
  (default nil)
  (subjects library)
  (pre-change-fn (lambda (flag value old-value) ; cebrown 3/30/02
		   (declare (ignore flag))
		   (unless (equal value old-value)
		     (when (and *running-remotely* (not *expert-running-remotely*))
		       (throwfail "Sorry.  You are not allowed to change this flag when running remotely.")))))
  (change-fn (lambda (flag value old-value)
	       (declare (ignore flag))
	       (unless (equal value old-value)
		       (restore-lib-hashtable))))
  (mhelp "The list of writeable directories containing 
library files. All of the directories in this list ought to be 
library directories to which the user has write access.
See also BACKUP-LIB-DIR and SHOW-ALL-LIBOBJECTS."))

(defflag backup-lib-dir
  (flagtype dirspeclist)
  (default nil)
  (subjects library)
  (pre-change-fn (lambda (flag value old-value) ; cebrown 3/30/02
		   (declare (ignore flag))
		   (unless (equal value old-value)
		     (when (and *running-remotely* (not *expert-running-remotely*))
		       (throwfail "Sorry.  You are not allowed to change this flag when running remotely.")))))
  (change-fn (lambda (flag value old-value)
	       (declare (ignore flag))
	       (unless (equal value old-value)
		       (restore-lib-hashtable))))
  (mhelp "The list of all backup directories of library files.
These should be directories to which the user has read access.
No attempt will be made to write to a directory on this list.
See also DEFAULT-LIB-DIR and SHOW-ALL-LIBOBJECTS."))

(defflag show-all-libobjects
  (flagtype boolean)
  (default t)
  (subjects library)
  (mhelp "When loading an object, if there are multiple objects of that name 
and type, when NIL then accept the first object found (searching 
DEFAULT-LIB-DIR and then BACKUP-LIB-DIR in order). When T, show a list
of all the objects and ask the user to choose."))

(defflag add-subdirectories
  (flagtype boolean)
  (default t)
  (subjects library)
  (mhelp "When restoring the library index, search the directories in 
DEFAULT-LIB-DIR and BACKUP-LIB-DIR for subdirectories which also contain 
library files, and add these to the flags. This flag only works for Allegro,
CMU, Kyoto and Lucid Common Lisps."))

(defflag recordflags
  (flagtype tpsflaglist)
  (default nil)
  (subjects library auto::mating-search)
  (mhelp "List of flags to be saved when using the mateop DATEREC."))
(context library)

(defflag lib-masterindex-file
  (flagtype filespec)
  (default "libindex.rec")
  (subjects library)
  (mhelp "Name of the file containing index of entries in the library."))

(defflag lib-keyword-file
  (flagtype filespec)
  (default "keywords.rec")
  (subjects library)
  (mhelp "Name of the file containing acceptable keywords for the library."))

(defflag lib-bestmode-file
  (flagtype filespec)
  (default "bestmodes.rec")
  (subjects library)
  (mhelp "Name of the file containing best modes for the theorems in the library."))

(defflag remove-trailing-dir
  (flagtype boolean)
  (default T)
  (subjects library)
  (change-fn (lambda (flag value old-value)
	       (declare (ignore flag value old-value))
	       (initialise-assumed)))
  (mhelp "If T, the parts of the directory specification that are the same 
for all library files will be removed before printing. If NIL, the full
directory will be printed."))

(defvar assumed-initial-dir "")
(defvar lib-keyword-list 
  '((SK-FIRST-ORDER . "\"After skolemizing, this is a first-order gwff/abbrev.\"")
    (SK-HIGHER-ORDER . "\"After skolemizing, this is a higher-order gwff/abbrev.\"")
    (FIRST-ORDER . "\"This is a first-order gwff/abbrev.\"")
    (HIGHER-ORDER . "\"This is a higher order gwff/abbrev.\"")
    (AUTO-PROOF . "\"This gwff has been proven automatically or semi-automatically.\"")
    (NO-AUTO-PROOF . "\"This gwff has not been proven automatically.\"")
    (WITH-DEFN . "\"This gwff contains a definition.\"")
    (WITHOUT-DEFN . "\"This gwff does not contain a definition.\"")
    (WITH-EQUALITY . "\"This gwff contains an equality.\"")
    (WITHOUT-EQUALITY . "\"This gwff does not contain an equality.\"")
    (PROVEN . "\"This gwff has been proven.\"")
    (UNPROVEN . "\"This gwff has not been proven.\"")))

(defvar provability-list '("Successful mode for automatic proof found automatically."
			   "Automatic expansion proof and translation to natural deduction"
			   "Automatic expansion proof"
			   "Semi-automatic expansion proof in MATE top level"
			   "Interactive expansion proof in MATE top level" 
			   "Semi-automatic natural deduction proof"
			   "Interactive natural deduction proof"
			   "Not proven" 
			   "Not provable" 
			   NIL))

(defvar lib-bestmodes (make-hash-table :test #'eq))

(defcategory class-scheme
  (define def-class-scheme)
  (properties
   (class-direction updown)
   (libclass libclass))
  (global-list global-class-scheme-list)
  (mhelp-line "Classification Scheme for the library.")
  (mhelp-fn princ-mhelp))
