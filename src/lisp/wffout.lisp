;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-PRINT)

;;;
;;; File: WFFOUT
;;; Package: wff-print
;;;
;;; defines flags, variables, and macros for wffs.
;;;

(part-of wff-print)

(deffile wffout
  (part-of wff-print)
  (extension lsp)
  (mhelp "Contains flags and macros for printing wffs."))

(context wff-printing)

(defflag allscopeflag
  (flagtype boolean)
  (default nil)
  (subjects printing)
  (mhelp "If T, all brackets will be printed; no implicit scoping is assumed."))

(defflag atomvalflag
  (flagtype boolean)
  (default nil)
  (subjects printing)
  (mhelp "If T, the name of every atom will be printed below its value."))

(defflag blank-lines-inserted
  (flagtype posinteger)
  (subjects editor printing window-props)
  (default 24)
  (mhelp "Number of blank lines printed in the proofwindows between different
stages of each proof."))

(defflag charsize
  (flagtype symbol)
  (default med)
  (subjects editor printing)
  (mhelp "Should be one of MIN, MED or MAX. 
Determines the size of characters used by Proofwindows and Editor Windows.
Currently, MIN and MED are the same size."))

(defflag displaywff
  (flagtype boolean)
  (default nil)
  (subjects printing printing-tex)
  (mhelp "If T, formulas are printed on separate lines."))

(defflag fillineflag
  (flagtype boolean)
  (default nil)
  (subjects printing)
  (mhelp "If NIL, every argument of an associative infix operator will have a
separate line."))

(defflag first-order-print-mode
  (flagtype boolean)
  (default nil)
  (subjects printing)
  (mhelp "If T, formulas are printed so they can be parsed when 
FIRST-ORDER-MODE-PARSE is set to T."))

(defflag flushleftflag
  (flagtype boolean)
  (default nil)
  (subjects printing)
;  (mhelp "If T, no line of a pretty-printed formula will be indented.")
  (mhelp "Currently this flag does nothing."))

(defflag localleftflag
  (flagtype boolean)
  (default nil)
  (subjects printing)
  (mhelp "If T, arguments of infix operators start in the same column as
the operator."))


(defflag ppwfflag
  (flagtype boolean)
  (default t)
  (subjects printing printing-tex)
  (mhelp "If T, formulas will generally be pretty-printed
(except for the editor).  For pretty-printing to work properly,
the flag INFIX-NOTATION must be set to T."))

(defflag printdepth
  (flagtype integer+)
  (default 0)
  (subjects printing)
  (mhelp "If 0, all printing will be done to arbitrary recursive depth,
if n > 0 subformulas of depth n will be replaced by '&'."))

(defflag printtypes
  (flagtype boolean)
  (default t)
  (subjects printing)
  (mhelp "If NIL, type symbols will never be printed."))

(defflag printtypes-all
  (flagtype boolean)
  (default nil)
  (subjects printing)
  (mhelp "This flag only applies when the flag PRINTTYPES is T.
If PRINTTYPES-ALL is NIL, type symbols will be printed only on the first 
occurrence of a variable name. If it is T, type symbols will be printed on
every occurrence of a variable name."))

(defflag leftmargin
  (flagtype integer+)
  (default 0)
  (subjects printing)
  (mhelp "The global left margin of the terminal in characters."))

(defflag rightmargin
  (flagtype integer+)
  (default 79)
  (subjects printing)
  (mhelp "The global right margin of the terminal in characters.

See Also:  PAGEWIDTH"))

(defflag use-internal-print-mode
  (flagtype boolean)
  (default NIL) ; changed default to NIL - cebrown 3/7/03
  (subjects printing printing-tex)
  (mhelp "If T, the internally-defined modes SCRIBE-OTL,
TEX-OTL and TEX-1-OTL will be used for printing Scribe and
TeX output. (See the help message for TEX-MIMIC-SCRIBE for 
help on the difference between the last two.)
These are usually good enough, but if you want to use a 
custom-defined flag setting, then set this flag to NIL to
override the internal modes.  This may cause problems,
in which case set this flag back to T."))

(defflag scope
  (flagtype boolean)
  (default nil)
  (subjects printing)
  (mhelp "If T, all wffs will be enclosed in square brackets."))


(defflag use-dot
  (flagtype boolean)
  (default t)
  (subjects printing)
  (mhelp "If T, formulas are printed using Church's dot notation.
If NIL, only brackets will be used."))

(defvar ppvirtflag nil)

(defvar pc nil)

(defvar ppwfflist nil)

(defvar ppwfflength 0)

(defflag slides-preamble
  (flagtype string)
  (default "")
  (subjects printing)
  (mhelp "The preamble that is printed into the first lines of all 
the Scribe slides files produced by TPS. See also SCRIBE-PREAMBLE."))

(defvar slides-width 49)

