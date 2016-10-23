;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)

(deffile test-macros
  (part-of mating)
  (extension lisp)
  (mhelp "Defines structures and flags for test-top."))

(context test-searchlists)

(defvar *suggest-list*)
(defvar *test-gwff* nil)
(defvar *test-gwff-name* nil)
(defvar *test-mode-name* nil)
(defvar *times-around* 0) ; the number of times we've been around the entire loop
(defvar *current-bfs-depth* 0)
(defvar *press-down-fixed* nil)
(defvar *press-down-changed* nil)
(defvar *press-down-last-succeeded* 0)
(defvar *testwin-opened* nil)
(defvar *current-testname* nil)
(defvar *testwin-process* nil)
(defvar *big-testwin* nil)

(defstruct (search-item (:print-function print-search-item))
  (flag nil)     ; the name of the flag
  (current nil)  ; the current value
  (default nil)  ; the default value
  (range nil)    ; a list of the values to use
  (fixed nil)    ; will be set to T if the current search has fixed a value for this
  (internal nil) ; reserved for internal use by individual searches
)

(defun print-search-item (obj stream depth)
  (declare (ignore depth))
  (format stream "~S = ~S, default is ~S, range is [~S]" (search-item-flag obj) 
	  (search-item-current obj) (search-item-default obj) (search-item-range obj))
  (if (search-item-fixed obj) (format stream "... is now fixed.")))

(defstruct (searchlist (:print-function print-searchlist))
  (list nil)
  (function nil)
  (name nil)
)

(defun print-searchlist (obj stream depth)
  (declare (ignore depth stream))
  (princ (searchlist-name obj)) (princ ":") (terpri)
  (dolist (item (searchlist-list obj)) (princ item) (terpri)))

(defvar *global-searchlist* nil)
(defvar current-sl nil)
(defvar current-sl-name nil) ; will usually be eq to (searchlist-name current-sl)
(defvar *stupid-flag-list*   ; a (not necessarily exhaustive) list of flags we don't want to vary
  (list 'VPW-HEIGHT 'EDWIN-VPFORM-HEIGHT 'EDWIN-CURRENT-HEIGHT 'EDWIN-TOP-HEIGHT
	'EDWIN-VPFORM-WIDTH 'EDWIN-CURRENT-WIDTH 'EDWIN-TOP-WIDTH 'PROOFW-ALL-WIDTH 
	'PROOFW-ACTIVE-WIDTH 'PROOFW-ALL-HEIGHT 'PROOFW-ACTIVE-HEIGHT 'PROOFW-ACTIVE 'PROOFW-ALL
	'STYLE 'SHOW-ALL-PACKAGES 'HISTORY-SIZE 'ALLSCOPEFLAG 'ATOMVALFLAG 'BLANK-LINES-INSERTED 
	'CHARSIZE 'DISPLAYWFF 'FILLINEFLAG 'FIRST-ORDER-PRINT-MODE 'FLUSHLEFTFLAG 'LEFTMARGIN 
	'LOCALLEFTFLAG 'PPWFFLAG 'PRINTDEPTH 'PRINTTYPES 'RETAIN-INITIAL-TYPE 'RIGHTMARGIN
	'SCOPE 'USE-DOT 'IN-TEX-MATH-MODE 'PAGELENGTH 'TEX-MIMIC-SCRIBE 'PRINT-WEAK
	'SAVE-INTERVAL 'SAVE-WORK-ON-START-UP 'SAVE-WORK-P 'PRINTEDTFILE 'PRINTEDTFLAG 
	'PRINTEDTFLAG-SLIDES 'PRINTEDTOPS 'SCRIBE-LINE-WIDTH 'SLIDES-TURNSTYLE-INDENT
	'TURNSTYLE-INDENT 'ECONJ-NAME 'EDISJ-NAME 'EPROOF-NAME 'EXPANSION-NAME 'FALSE-NAME 'IMP-NAME 
	'LEAF-NAME 'MATING-NAME 'NEG-NAME 'PRINT-DEEP 'PRINT-NODENAMES 'REWRITE-NAME 'SELECTION-NAME
	'SHOW-SKOLEM 'SKOLEM-SELECTION-NAME 'TRUE-NAME 'REC-MS-FILE 'REC-MS-FILENAME 
	'GO-INSTRUCTIONS 'QUIETLY-USE-DEFAULTS 'RESOLVE-CONFLICT 'LIT-NAME 'PRINT-LIT-NAME 
	'PRINTVPDFLAG 'TEXFORMAT 'VPD-BRIEF 'VPD-FILENAME 'VPD-LIT-NAME 'VPD-PTYPES
	'VPD-STYLE 'VPD-VPFPAGE 'VPFORM-LABELS 'VPFORM-TEX-MAGNIFICATION 'VPFORM-TEX-NEST
	'VPFORM-TEX-PREAMBLE 'VPW-WIDTH 'EDPPWFFLAG 'EDPRINTDEPTH 'EDWIN-CURRENT 'EDWIN-TOP
	'EDWIN-VPFORM 'META-BDVAR-NAME 'META-VAR-NAME 'PRIM-PREFIX 'ADVICE-ASKED-ENABLED 'ADVICE-FILE 
	'COMMAND-ENABLED 'COMMAND-FILE 'DONE-EXC-ENABLED 'ERROR-ENABLED 'ERROR-FILE 'EVENT-CYCLE 
	'EVENTS-ENABLED 'INPUT-ERROR-ENABLED 'INPUT-ERROR-FILE 'PROOF-ACTION-ENABLED 'PROOF-FILE
	'QUIET-EVENTS 'RULE-ERROR-ENABLED 'RULE-ERROR-FILE 'SCORE-FILE 'COMPILED-EXTENSION 'EXPERTFLAG 
	'INIT-DIALOGUE 'INIT-DIALOGUE-FN 'LOAD-WARN-P 'NEWS-DIR 'READ-LLOAD-SOURCES-P 'SAVE-FILE 
	'SOURCE-EXTENSION 'SOURCE-PATH 'BACKUP-LIB-DIR 'CAL-PERCENTAGE 'COURSE-NAME 'DEFAULT-LIB-DIR 
	'DEFAULT-LIBFILE-TYPE 'DEFAULT-LIBINDEX-TYPE 'DEFAULT-PENALTY-FN 'DROP-MIN 'DUE-DATE-FLAG 
	'ETPS-FILE 'GRADE-FILE 'HLINE-JUSTIFICATION 'LETTER-GRADE-FILE 'LETTER-GRADE-FLAG
	'LISP-IMPLEMENTATION-TYPE 'MACHINE-TYPE 'META-LABEL-NAME 'NEW-ITEM 'OLD-GRADE-FILE 
	'OLD-TOTALS-GRADE-FILE 'PATCH-FILE 'PRINT-DOTS 'PRINT-META 'PRINT-N-DIGITS 'PRINTLINEFLAG
	'RECORDFLAGS 'SHORT-HELP 'SHORT-SITE-NAME 'STATISTICAL-OPTIONS 'TOTALS-GRADE-FILE
	'LIB-MASTERINDEX-FILE 'TURNSTYLE-INDENT-AUTO 'TURNSTILE-INDENT-AUTO 'PROOFW-ACTIVE+NOS
	'PROOFW-ACTIVE+NOS-HEIGHT 'PROOFW-ACTIVE+NOS-WIDTH 'SLIDES-TURNSTILE-INDENT 
	'TURNSTILE-INDENT 'PRINTMATEFILE 'PRINTMATEFLAG 'PRINTMATEFLAG-SLIDES
	'PRINTMATEOPS 'TEX-LINE-WIDTH 'TESTWIN-HEIGHT 'TESTWIN-WIDTH 'TEST-THEOREMS
	'TPSTEX 'TEX-PREAMBLE 'TEX-POSTAMBLE 'TEX-1-PREAMBLE 'TEX-1-POSTAMBLE 'SCRIBE-PREAMBLE
	'SCRIBE-POSTAMBLE 'SLIDES-PREAMBLE 'LAST-MODE-NAME))
(defvar *dangerous-flag-list* ; a list of flags not to vary because they control the test-search.
  (list 'test-next-search-fn 'test-verbose 'test-initial-time-limit 'TEST-REDUCE-TIME
	'test-increase-time 'test-faster-if-t 'test-faster-if-nil 'test-faster-if-high
	'test-faster-if-low 'test-easier-if-t 'test-easier-if-nil 'test-easier-if-high
	'test-easier-if-low 'test-fix-unif-depths 'test-max-search-values))
(defvar *finished-flag* nil)

(defflag test-max-search-values
  (flagtype posinteger)
  (subjects test-top)
  (default 10)
  (mhelp "The maximum number of values that will be put in the
range of any flag in an automatically-generated searchlist.
(In a manually-generated list, you can have as large a range
as you like.)"))

(defflag test-easier-if-nil
  (flagtype tpsflaglist)
  (subjects test-top)
  (default '())
  (mhelp "The list of flags that, if set to NIL, make 
mating-search easier. Used by SCALE-UP. \"Easier\" in this context
means \"more likely to succeed eventually, although possibly
taking longer about it\". Compare TEST-FASTER-IF-NIL; the list is 
somewhat debatable, which is why you're allowed to change it."))

(defflag test-faster-if-nil
  (flagtype tpsflaglist)
  (subjects test-top)
  (default '())
  (mhelp "The list of flags that, if set to NIL, make 
mating-search run faster. Used by SCALE-DOWN. \"Faster\" in this
context means \"if it succeeds at all, it does so more quickly\".
Compare TEST-EASIER-IF-NIL; the list is somewhat debatable, which is 
why you're allowed to change it."))

(defflag test-easier-if-t
  (flagtype tpsflaglist)
  (subjects test-top)
  (default '(eta-rule min-quantifier-scope ms-split))
  (mhelp "The list of flags that, if set to T, make 
mating-search easier.  Used by SCALE-UP. \"Easier\" in this context
means \"more likely to succeed eventually, although possibly
taking longer about it\". Compare TEST-FASTER-IF-T; the list is 
somewhat debatable, which is why you're allowed to change it."))

(defflag test-faster-if-t
  (flagtype tpsflaglist)
  (subjects test-top)
  (default '(min-quantifier-scope ms-split))
  (mhelp "The list of flags that, if set to T, make 
mating-search faster.  Used by SCALE-DOWN. \"Faster\" in this
context means \"if it succeeds at all, it does so more quickly\".
Compare TEST-EASIER-IF-T; the list is somewhat debatable, which is 
why you're allowed to change it."))

(defflag test-easier-if-high
  (flagtype tpsflaglist)
  (subjects test-top)
  (default '(max-search-depth search-time-limit num-of-dups max-utree-depth max-mates max-search-limit))
  (mhelp "The list of flags that, if set to high numbers, make 
mating-search easier. Used by SCALE-UP. \"Easier\" in this context
means \"more likely to succeed eventually, although possibly
taking longer about it\". Compare TEST-FASTER-IF-HIGH; the list is 
somewhat debatable, which is why you're allowed to change it."))

(defflag test-faster-if-high
  (flagtype tpsflaglist)
  (subjects test-top)
  (default '(min-quick-depth))
  (mhelp "The list of flags that, if set to high numbers, make 
mating-search faster.  Used by SCALE-DOWN. \"Faster\" in this
context means \"if it succeeds at all, it does so more quickly\".
Compare TEST-EASIER-IF-HIGH; the list is somewhat debatable, which is 
why you're allowed to change it."))

(defflag test-easier-if-low
  (flagtype tpsflaglist)
  (subjects test-top)
  (default '(min-quick-depth))
  (mhelp "The list of flags that, if set to low numbers, make 
mating-search easier. Used by SCALE-UP. \"Easier\" in this context
means \"more likely to succeed eventually, although possibly
taking longer about it\". Compare TEST-FASTER-IF-LOW; the list is 
somewhat debatable, which is why you're allowed to change it."))

(defflag test-faster-if-low
  (flagtype tpsflaglist)
  (subjects test-top)
  (default '(max-search-depth search-time-limit num-of-dups max-utree-depth max-mates max-search-limit))
  (mhelp "The list of flags that, if set to low numbers, make 
mating-search faster. Used by SCALE-DOWN. \"Faster\" in this
context means \"if it succeeds at all, it does so more quickly\".
Compare TEST-EASIER-IF-LOW; the list is somewhat debatable, which is 
why you're allowed to change it."))

(defflag test-next-search-fn
  (flagtype symbol)
  (subjects test-top)
  (default exhaustive-search)
  (mhelp "The name of a function which should take a searchlist and the time taken for
the previous attempt as arguments, and should set the flags in the list
appropriately for the next search. This function should also return T in
*finished-flag* if all settings have been tried.
The only values defined so far are:
EXHAUSTIVE-SEARCH, which tries all combinations of flags in a searchlist, 
 varying one flag through its entire range before trying the next flag.
BREADTH-FIRST-SEARCH, which also tries all combinations of flags, but 
 varies each flag a little at a time.
PRESS-DOWN, which is used by the PRESS-DOWN command.
PRESS-DOWN-2, which behaves like breadth-first search except that if varying
 a flag makes the search faster, that flag is then prevented
 from returning above its original value (the range of each flag is 
 assumed to be ordered; if the range is (A B C D), and setting it to
 C results in a faster search, it will never again be set to A or B).
PUSH-UP, which is used by the PUSH-UP command.
PUSH-UP-2, which is like breadth-first search but terminates once a 
 successful mode is discovered; it is used for relaxing an unsuccessful
 mode until it is successful."))

(definfo exhaustive-search
  (mhelp "A setting for TEST-NEXT-SEARCH-FN.
Tries all combinations of flags in a searchlist, 
varying one flag through its entire range before trying the next flag."))

(definfo breadth-first-search
  (mhelp "A setting for TEST-NEXT-SEARCH-FN.
Tries all combinations of flags, but varies each flag a little at a time
rather than varying one flag through its entire range before trying the next."))

(definfo press-down
  (mhelp "A setting for TEST-NEXT-SEARCH-FN.
This setting is used internally by the PRESS-DOWN command."))

(definfo press-down-2
  (mhelp "A setting for TEST-NEXT-SEARCH-FN.
This behaves like breadth-first search except that if varying
a flag makes the search faster, that flag is then prevented
from returning above its original value (the range of each flag is 
assumed to be ordered; if the range is (A B C D), and setting it to
C results in a faster search, it will never again be set to A or B)."))

(definfo push-up
  (mhelp "A setting for TEST-NEXT-SEARCH-FN.
This setting is used internally by the PUSH-UP command."))

(definfo push-up-2
  (mhelp "A setting for TEST-NEXT-SEARCH-FN.
This setting is like breadth-first search but terminates once a 
successful mode is discovered; it is used for relaxing an unsuccessful
mode until it is successful."))

(defflag test-verbose
  (flagtype boolean)
  (subjects test-top)
  (default t)
  (mhelp "If NIL, suppresses a lot of the output of the test top level."))

(defflag test-initial-time-limit
  (flagtype posinteger)
  (subjects test-top)
  (default 30)
  (mhelp "The time limit to be used for each individual search. This
limit will be increased if it is found to be insufficient. See 
also the flags TEST-INCREASE-TIME and TEST-REDUCE-TIME. 
The time referred to will be internal time without counting 
garbage collection, if possible (see the flag EXCLUDING-GC-TIME)."))

(defflag test-reduce-time
  (flagtype boolean)
  (subjects test-top)
  (default t)
  (mhelp "If T, then TEST-INITIAL-TIME-LIMIT will be reduced every time a faster 
combination of flags is found. If NIL, then it won't be."))

(defflag test-increase-time
  (flagtype integer+)
  (subjects test-top)
  (default 0)
  (mhelp "After each unsuccessful search in the test top level,
the value of TEST-INITIAL-TIME-LIMIT will be increased by this 
proportion. (So, e.g., setting this flag to 10 will result in a 10% 
increase on each attempt; setting it to 100 will double 
TEST-INITIAL-TIME-LIMIT every time around.)
NOTE: After the first successful search, this flag will be set to
zero. The change will be permanent, in order to allow CONTINUE to
work properly."))

(defflag test-fix-unif-depths
  (flagtype boolean)
  (subjects test-top)
  (default t)
  (mhelp "If T, then LEAST-SEARCH-DEPTH will be used to fix the unification depths
MAX-UTREE-DEPTH and MAX-SEARCH-DEPTH as soon as a search in the TEST top 
level is successful, and these will not be varied again. Destructively 
alters the search list, by changing the range of these two flags to a 
single element."))

(defflag testwin-width
  (flagtype posinteger)
  (default 80)
  (subjects test-top window-props)
  (mhelp "Contains the initial width of the testwindow."))

(defflag testwin-height
  (flagtype posinteger)
  (default 24)
  (subjects test-top window-props)
  (mhelp "Contains the initial height of the testwindow."))

