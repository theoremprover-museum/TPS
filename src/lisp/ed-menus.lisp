;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-EDITOR)

;;;
;;; File: EDTOP
;;; Package: WFF-EDITOR
;;;
;;; defines menus for the editor top-level
;;; cebrown 5/6/02

(part-of wff-editor)

(deffile ed-menus
  (part-of wff-editor)
  (extension lsp)
  (mhelp "Define menus for editor top-level."))

(context subtoplevels)

(defmenuitem EDITOR0
  (display-name "ED")
  (placement 5)
  (command "ED")
  (parent TOP-LEVELS)
  (hotkey #\e)
  (etps t)
  (mhelp ""))

(defmenu EDITOR
  (display-name "Editor")
  (placement 2)
  (parent ED-TOP)
  (mhelp ""))

(defmenu PRINT
  (display-name "Print")
  (placement 2)
  (parent EDITOR)
  (mhelp ""))

(defmenuitem P
  (display-name "P")
  (placement 2)
  (command "P")
  (parent PRINT)
  (mhelp ""))

(defmenuitem PP
  (display-name "PP")
  (placement 3)
  (command "PP")
  (parent PRINT)
  (mhelp ""))

(defmenuitem PS
  (display-name "PS")
  (placement 4)
  (command "PS")
  (parent PRINT)
  (mhelp ""))

(defmenuitem PT
  (display-name "PT")
  (placement 5)
  (command "PT")
  (parent PRINT)
  (mhelp ""))

(defmenu WEAK-LABELS
  (display-name "Weak Labels")
  (placement 2)
  (parent EDITOR)
  (mhelp ""))

(defmenuitem CW
  (display-name "CW")
  (placement 2)
  (command "CW")
  (parent WEAK-LABELS)
  (mhelp ""))

(defmenuitem DELWEAK
  (display-name "DELWEAK")
  (placement 3)
  (command "DELWEAK")
  (parent WEAK-LABELS)
  (etps t)
  (mhelp ""))

(defmenuitem DW
  (display-name "DW")
  (placement 4)
  (command "DW")
  (parent WEAK-LABELS)
  (mhelp ""))

(defmenuitem DW*
  (display-name "DW*")
  (placement 5)
  (command "DW*")
  (parent WEAK-LABELS)
  (mhelp ""))

(defmenuitem NAME
  (display-name "NAME")
  (placement 6)
  (command "NAME")
  (parent WEAK-LABELS)
  (etps t)
  (mhelp ""))

(defmenuitem RW
  (display-name "RW")
  (placement 7)
  (command "RW")
  (parent WEAK-LABELS)
  (mhelp ""))

(defmenuitem SAVE
  (display-name "SAVE")
  (placement 2)
  (command "SAVE")
  (parent EDITOR)
  (mhelp ""))

(defmenu ED-SCRIBE-RECORD
  (display-name "Ed Scribe Record")
  (placement 3)
  (parent EDITOR)
  (mhelp ""))

(defmenuitem O
  (display-name "O")
  (placement 2)
  (command "O")
  (parent ED-SCRIBE-RECORD)
  (etps t)
  (mhelp ""))

(defmenuitem REM
  (display-name "REM")
  (placement 3)
  (command "REM")
  (parent ED-SCRIBE-RECORD)
  (mhelp ""))

(defmenu ED-JFORMS
  (display-name "Ed JForms")
  (placement 3)
  (parent EDITOR)
  (mhelp ""))

(defmenuitem CJFORM
  (display-name "CJFORM")
  (placement 2)
  (command "CJFORM")
  (parent ED-JFORMS)
  (mhelp ""))

(defmenuitem DJFORM
  (display-name "DJFORM")
  (placement 3)
  (command "DJFORM")
  (parent ED-JFORMS)
  (mhelp ""))

(defmenuitem NUM-HPATHS
  (display-name "NUM-HPATHS")
  (placement 4)
  (command "NUM-HPATHS")
  (parent ED-JFORMS)
  (mhelp ""))

(defmenuitem NUM-VPATHS
  (display-name "NUM-VPATHS")
  (placement 5)
  (command "NUM-VPATHS")
  (parent ED-JFORMS)
  (mhelp ""))

(defmenuitem PJ
  (display-name "PJ")
  (placement 6)
  (command "PJ")
  (parent ED-JFORMS)
  (mhelp ""))

(defmenuitem PROP-CJFORM
  (display-name "PROP-CJFORM")
  (placement 7)
  (command "PROP-CJFORM")
  (parent ED-JFORMS)
  (mhelp ""))

(defmenuitem VP
  (display-name "VP")
  (placement 8)
  (command "VP")
  (parent ED-JFORMS)
  (mhelp ""))

(defmenuitem VPD
  (display-name "VPD")
  (placement 9)
  (command "VPD")
  (parent ED-JFORMS)
  (mhelp ""))

(defmenuitem VPF
  (display-name "VPF")
  (placement 10)
  (command "VPF")
  (parent ED-JFORMS)
  (mhelp ""))

(defmenuitem VPT
  (display-name "VPT")
  (placement 11)
  (command "VPT")
  (parent ED-JFORMS)
  (mhelp ""))

(defmenuitem LEAVE
  (display-name "LEAVE")
  (placement 300)
  (command "LEAVE")
  (parent EDITOR)
  (etps t)
  (mhelp ""))

(defmenuitem NOOP
  (display-name "NOOP")
  (placement 101)
  (command "NOOP")
  (parent EDITOR)
  (etps t)
  (mhelp ""))

(defmenuitem OK
  (display-name "OK")
  (placement 202)
  (command "OK")
  (parent EDITOR)
  (etps t)
  (mhelp ""))

(defmenu ED-MOVING
  (display-name "Ed Moving")
  (placement 2)
  (parent ED-TOP)
  (mhelp ""))

(defmenuitem UP-ONE-LEVEL
  (display-name "|0| (Up One Level)")
  (placement 2)
  (command "|0|")
  (parent ED-MOVING)
  (etps t)
  (mhelp ""))

(defmenuitem A
  (display-name "A")
  (placement 3)
  (command "A")
  (parent ED-MOVING)
  (etps t)
  (mhelp ""))

(defmenuitem D
  (display-name "D")
  (placement 4)
  (command "D")
  (parent ED-MOVING)
  (etps t)
  (mhelp ""))

(defmenuitem FIRST-BINDER
  (display-name "FB (First Binder)")
  (placement 5)
  (command "FB")
  (parent ED-MOVING)
  (etps t)
  (mhelp ""))

(defmenuitem FIRST-INFIX
  (display-name "FI (First Infix)")
  (placement 6)
  (command "FI")
  (parent ED-MOVING)
  (etps t)
  (mhelp ""))

(defmenuitem LEFT
  (display-name "L (Left)")
  (placement 7)
  (command "L")
  (parent ED-MOVING)
  (etps t)
  (mhelp ""))

(defmenuitem RIGHT
  (display-name "R (Right)")
  (placement 8)
  (command "R")
  (parent ED-MOVING)
  (etps t)
  (mhelp ""))

(defmenuitem UNDO
  (display-name "UNDO")
  (placement 9)
  (command "UNDO")
  (parent ED-MOVING)
  (etps t)
  (mhelp ""))

(defmenuitem XTR
  (display-name "XTR")
  (placement 10)
  (command "XTR")
  (parent ED-MOVING)
  (etps t)
  (mhelp ""))

(defmenuitem GOTO-TOP
  (display-name "^ (Goto Top)")
  (placement 11)
  (command "^")
  (parent ED-MOVING)
  (etps t)
  (mhelp ""))

(defmenu CHANGING
  (display-name "Changing")
  (placement 2)
  (parent ED-TOP)
  (mhelp ""))

(defmenuitem ASRB
  (display-name "ASRB")
  (placement 2)
  (command "ASRB")
  (parent CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem ASSL
  (display-name "ASSL")
  (placement 3)
  (command "ASSL")
  (parent CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem ASSR
  (display-name "ASSR")
  (placement 4)
  (command "ASSR")
  (parent CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem CMRG
  (display-name "CMRG")
  (placement 5)
  (command "CMRG")
  (parent CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem CMUT
  (display-name "CMUT")
  (placement 6)
  (command "CMUT")
  (parent CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem CNTOP
  (display-name "CNTOP")
  (placement 7)
  (command "CNTOP")
  (parent CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem DIST-CTR
  (display-name "DIST-CTR")
  (placement 8)
  (command "DIST-CTR")
  (parent CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem DIST-EXP
  (display-name "DIST-EXP")
  (placement 9)
  (command "DIST-EXP")
  (parent CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem DL
  (display-name "DL")
  (placement 10)
  (command "DL")
  (parent CHANGING)
  (mhelp ""))

(defmenuitem DNEG
  (display-name "DNEG")
  (placement 11)
  (command "DNEG")
  (parent CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem DR
  (display-name "DR")
  (placement 12)
  (command "DR")
  (parent CHANGING)
  (mhelp ""))

(defmenuitem MRG
  (display-name "MRG")
  (placement 13)
  (command "MRG")
  (parent CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem PMUT
  (display-name "PMUT")
  (placement 14)
  (command "PMUT")
  (parent CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem SUBEQ
  (display-name "SUBEQ")
  (placement 15)
  (command "SUBEQ")
  (parent CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem SUBIM
  (display-name "SUBIM")
  (placement 16)
  (command "SUBIM")
  (parent CHANGING)
  (etps t)
  (mhelp ""))

(defmenu REC-CHANGING
  (display-name "Rec Changing")
  (placement 17)
  (parent CHANGING)
  (mhelp ""))

(defmenuitem ASRB*
  (display-name "ASRB*")
  (placement 2)
  (command "ASRB*")
  (parent REC-CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem ASSL*
  (display-name "ASSL*")
  (placement 3)
  (command "ASSL*")
  (parent REC-CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem ASSR*
  (display-name "ASSR*")
  (placement 4)
  (command "ASSR*")
  (parent REC-CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem CMRG*
  (display-name "CMRG*")
  (placement 5)
  (command "CMRG*")
  (parent REC-CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem CMUT*
  (display-name "CMUT*")
  (placement 6)
  (command "CMUT*")
  (parent REC-CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem DIST-CTR*
  (display-name "DIST-CTR*")
  (placement 7)
  (command "DIST-CTR*")
  (parent REC-CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem DIST-EXP*
  (display-name "DIST-EXP*")
  (placement 8)
  (command "DIST-EXP*")
  (parent REC-CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem DNEG*
  (display-name "DNEG*")
  (placement 9)
  (command "DNEG*")
  (parent REC-CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem MRG*
  (display-name "MRG*")
  (placement 10)
  (command "MRG*")
  (parent REC-CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem PMUT*
  (display-name "PMUT*")
  (placement 11)
  (command "PMUT*")
  (parent REC-CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem SUBEQ*
  (display-name "SUBEQ*")
  (placement 12)
  (command "SUBEQ*")
  (parent REC-CHANGING)
  (etps t)
  (mhelp ""))

(defmenuitem SUBIM*
  (display-name "SUBIM*")
  (placement 13)
  (command "SUBIM*")
  (parent REC-CHANGING)
  (etps t)
  (mhelp ""))

(defmenu EMBEDDING
  (display-name "Embedding")
  (placement 17)
  (parent CHANGING)
  (mhelp ""))

(defmenuitem MBED-AL
  (display-name "MBED-AL")
  (placement 2)
  (command "MBED-AL")
  (parent EMBEDDING)
  (etps t)
  (mhelp ""))

(defmenuitem MBED-AR
  (display-name "MBED-AR")
  (placement 3)
  (command "MBED-AR")
  (parent EMBEDDING)
  (etps t)
  (mhelp ""))

(defmenuitem MBED-E
  (display-name "MBED-E")
  (placement 4)
  (command "MBED-E")
  (parent EMBEDDING)
  (etps t)
  (mhelp ""))

(defmenuitem MBED-E1
  (display-name "MBED-E1")
  (placement 5)
  (command "MBED-E1")
  (parent EMBEDDING)
  (etps t)
  (mhelp ""))

(defmenuitem MBED-F
  (display-name "MBED-F")
  (placement 6)
  (command "MBED-F")
  (parent EMBEDDING)
  (etps t)
  (mhelp ""))

(defmenuitem MBED-IL
  (display-name "MBED-IL")
  (placement 7)
  (command "MBED-IL")
  (parent EMBEDDING)
  (etps t)
  (mhelp ""))

(defmenuitem MBED-IR
  (display-name "MBED-IR")
  (placement 8)
  (command "MBED-IR")
  (parent EMBEDDING)
  (etps t)
  (mhelp ""))

(defmenuitem MBED-L
  (display-name "MBED-L")
  (placement 9)
  (command "MBED-L")
  (parent EMBEDDING)
  (etps t)
  (mhelp ""))

(defmenuitem MBED-OL
  (display-name "MBED-OL")
  (placement 10)
  (command "MBED-OL")
  (parent EMBEDDING)
  (etps t)
  (mhelp ""))

(defmenuitem MBED-OR
  (display-name "MBED-OR")
  (placement 11)
  (command "MBED-OR")
  (parent EMBEDDING)
  (etps t)
  (mhelp ""))

(defmenuitem MBED-QL
  (display-name "MBED-QL")
  (placement 12)
  (command "MBED-QL")
  (parent EMBEDDING)
  (etps t)
  (mhelp ""))

(defmenuitem MBED-QR
  (display-name "MBED-QR")
  (placement 13)
  (command "MBED-QR")
  (parent EMBEDDING)
  (etps t)
  (mhelp ""))

(defmenuitem MBED=L
  (display-name "MBED=L")
  (placement 14)
  (command "MBED=L")
  (parent EMBEDDING)
  (etps t)
  (mhelp ""))

(defmenuitem MBED=R
  (display-name "MBED=R")
  (placement 15)
  (command "MBED=R")
  (parent EMBEDDING)
  (etps t)
  (mhelp ""))

(defmenu REWRITING
  (display-name "Rewriting")
  (placement 16)
  (parent EMBEDDING)
  (mhelp ""))

(defmenuitem ARR
  (display-name "ARR")
  (placement 2)
  (command "ARR")
  (parent REWRITING)
  (mhelp ""))

(defmenuitem ARR*
  (display-name "ARR*")
  (placement 3)
  (command "ARR*")
  (parent REWRITING)
  (mhelp ""))

(defmenuitem ARR1
  (display-name "ARR1")
  (placement 4)
  (command "ARR1")
  (parent REWRITING)
  (mhelp ""))

(defmenuitem ARR1*
  (display-name "ARR1*")
  (placement 5)
  (command "ARR1*")
  (parent REWRITING)
  (mhelp ""))

(defmenuitem MAKE-RRULE
  (display-name "MAKE-RRULE")
  (placement 6)
  (command "MAKE-RRULE")
  (parent REWRITING)
  (mhelp ""))

(defmenuitem UNARR
  (display-name "UNARR")
  (placement 7)
  (command "UNARR")
  (parent REWRITING)
  (mhelp ""))

(defmenuitem UNARR*
  (display-name "UNARR*")
  (placement 8)
  (command "UNARR*")
  (parent REWRITING)
  (mhelp ""))

(defmenuitem UNARR1
  (display-name "UNARR1")
  (placement 9)
  (command "UNARR1")
  (parent REWRITING)
  (mhelp ""))

(defmenuitem UNARR1*
  (display-name "UNARR1*")
  (placement 10)
  (command "UNARR1*")
  (parent REWRITING)
  (mhelp ""))

(defmenu SUBSTITUTION
  (display-name "Substitution")
  (placement 16)
  (parent EMBEDDING)
  (mhelp ""))

(defmenuitem AB
  (display-name "AB")
  (placement 2)
  (command "AB")
  (parent SUBSTITUTION)
  (mhelp ""))

(defmenuitem IB
  (display-name "IB")
  (placement 3)
  (command "IB")
  (parent SUBSTITUTION)
  (mhelp ""))

(defmenuitem PRIM-SUBST
  (display-name "PRIM-SUBST")
  (placement 4)
  (command "PRIM-SUBST")
  (parent SUBSTITUTION)
  (etps t)
  (mhelp ""))

(defmenuitem REW-EQUIV
  (display-name "REW-EQUIV")
  (placement 5)
  (command "REW-EQUIV")
  (parent SUBSTITUTION)
  (mhelp ""))

(defmenuitem RP
  (display-name "RP")
  (placement 6)
  (command "RP")
  (parent SUBSTITUTION)
  (mhelp ""))

(defmenuitem RPALL
  (display-name "RPALL")
  (placement 7)
  (command "RPALL")
  (parent SUBSTITUTION)
  (mhelp ""))

(defmenuitem SUB
  (display-name "SUB")
  (placement 8)
  (command "SUB")
  (parent SUBSTITUTION)
  (etps t)
  (mhelp ""))

(defmenuitem SUBST
  (display-name "SUBST")
  (placement 9)
  (command "SUBST")
  (parent SUBSTITUTION)
  (mhelp ""))

(defmenuitem SUBSTYP
  (display-name "SUBSTYP")
  (placement 10)
  (command "SUBSTYP")
  (parent SUBSTITUTION)
  (etps t)
  (mhelp ""))

(defmenu ABBREV-OPS
  (display-name "Abbrev Ops")
  (placement 16)
  (parent EMBEDDING)
  (mhelp ""))

(defmenuitem ABBR
  (display-name "ABBR")
  (placement 2)
  (command "ABBR")
  (parent ABBREV-OPS)
  (mhelp ""))

(defmenuitem CONSTANTS
  (display-name "CONSTANTS")
  (placement 3)
  (command "CONSTANTS")
  (parent ABBREV-OPS)
  (mhelp ""))

(defmenuitem EXPAND=
  (display-name "EXPAND=")
  (placement 4)
  (command "EXPAND=")
  (parent ABBREV-OPS)
  (mhelp ""))

(defmenuitem EXPAND=*
  (display-name "EXPAND=*")
  (placement 5)
  (command "EXPAND=*")
  (parent ABBREV-OPS)
  (mhelp ""))

(defmenuitem INST
  (display-name "INST")
  (placement 6)
  (command "INST")
  (parent ABBREV-OPS)
  (mhelp ""))

(defmenuitem INST1
  (display-name "INST1")
  (placement 7)
  (command "INST1")
  (parent ABBREV-OPS)
  (mhelp ""))

(defmenuitem INSTALL
  (display-name "INSTALL")
  (placement 8)
  (command "INSTALL")
  (parent ABBREV-OPS)
  (mhelp ""))

(defmenuitem NEW-DEFS
  (display-name "NEW-DEFS")
  (placement 10)
  (command "NEW-DEFS")
  (parent ABBREV-OPS)
  (etps t)
  (mhelp ""))

(defmenu LAMBDA-OPS
  (display-name "Lambda Ops")
  (placement 16)
  (parent EMBEDDING)
  (mhelp ""))

(defmenuitem ABNORM
  (display-name "ABNORM")
  (placement 2)
  (command "ABNORM")
  (parent LAMBDA-OPS)
  (mhelp ""))

(defmenuitem ETAB
  (display-name "ETAB")
  (placement 3)
  (command "ETAB")
  (parent LAMBDA-OPS)
  (mhelp ""))

(defmenuitem ETAC
  (display-name "ETAC")
  (placement 4)
  (command "ETAC")
  (parent LAMBDA-OPS)
  (mhelp ""))

(defmenuitem ETAN
  (display-name "ETAN")
  (placement 5)
  (command "ETAN")
  (parent LAMBDA-OPS)
  (mhelp ""))

(defmenuitem ETAX
  (display-name "ETAX")
  (placement 6)
  (command "ETAX")
  (parent LAMBDA-OPS)
  (mhelp ""))

(defmenuitem LETA
  (display-name "LETA")
  (placement 7)
  (command "LETA")
  (parent LAMBDA-OPS)
  (mhelp ""))

(defmenuitem LEXP
  (display-name "LEXP")
  (placement 8)
  (command "LEXP")
  (parent LAMBDA-OPS)
  (mhelp ""))

(defmenuitem LNORM
  (display-name "LNORM")
  (placement 9)
  (command "LNORM")
  (parent LAMBDA-OPS)
  (etps t)
  (mhelp ""))

(defmenuitem LNORM-BETA
  (display-name "LNORM-BETA")
  (placement 10)
  (command "LNORM-BETA")
  (parent LAMBDA-OPS)
  (etps t)
  (mhelp ""))

(defmenuitem LNORM-ETA
  (display-name "LNORM-ETA")
  (placement 11)
  (command "LNORM-ETA")
  (parent LAMBDA-OPS)
  (etps t)
  (mhelp ""))

(defmenuitem RED
  (display-name "RED")
  (placement 12)
  (command "RED")
  (parent LAMBDA-OPS)
  (mhelp ""))

(defmenuitem ULNORM
  (display-name "ULNORM")
  (placement 13)
  (command "ULNORM")
  (parent LAMBDA-OPS)
  (mhelp ""))

(defmenu NEGATION-OPS
  (display-name "Negation Ops")
  (placement 16)
  (parent EMBEDDING)
  (mhelp ""))

(defmenuitem NEG
  (display-name "NEG")
  (placement 2)
  (command "NEG")
  (parent NEGATION-OPS)
  (mhelp ""))

(defmenuitem NNF2
  (display-name "NNF")
  (placement 3)
  (command "NNF")
  (parent NEGATION-OPS)
  (etps t)
  (mhelp ""))

(defmenuitem PULL-NEG
  (display-name "PULL-NEG")
  (placement 4)
  (command "PULL-NEG")
  (parent NEGATION-OPS)
  (mhelp ""))

(defmenuitem PUSH-NEG
  (display-name "PUSH-NEG")
  (placement 5)
  (command "PUSH-NEG")
  (parent NEGATION-OPS)
  (mhelp ""))

(defmenu PRIMSUB-OPS
  (display-name "Primsub Ops")
  (placement 16)
  (parent EMBEDDING)
  (mhelp ""))

(defmenuitem NAME-PRIM
  (display-name "NAME-PRIM")
  (placement 2)
  (command "NAME-PRIM")
  (parent PRIMSUB-OPS)
  (mhelp ""))

(defmenuitem PRT-PRIM
  (display-name "PRT-PRIM")
  (placement 3)
  (command "PRT-PRIM")
  (parent PRIMSUB-OPS)
  (mhelp ""))

(defmenu MISC-OPS
  (display-name "Misc Ops")
  (placement 16)
  (parent EMBEDDING)
  (mhelp ""))

(defmenuitem CLAUSE-FORM
  (display-name "CLAUSE-FORM")
  (placement 2)
  (command "CLAUSE-FORM")
  (parent MISC-OPS)
  (mhelp ""))

(defmenuitem CNF
  (display-name "CNF")
  (placement 3)
  (command "CNF")
  (parent MISC-OPS)
  (etps t)
  (mhelp ""))

(defmenuitem HEAD
  (display-name "HEAD")
  (placement 4)
  (command "HEAD")
  (parent MISC-OPS)
  (etps t)
  (mhelp ""))

(defmenuitem HVARS
  (display-name "HVARS")
  (placement 5)
  (command "HVARS")
  (parent MISC-OPS)
  (etps t)
  (mhelp ""))

(defmenuitem MIN-SCOPE
  (display-name "MIN-SCOPE")
  (placement 6)
  (command "MIN-SCOPE")
  (parent MISC-OPS)
  (mhelp ""))

(defmenuitem SUBFORMULAS
  (display-name "SUBFORMULAS")
  (placement 7)
  (command "SUBFORMULAS")
  (parent MISC-OPS)
  (mhelp ""))

(defmenu SKOLEMIZE
  (display-name "Skolemize")
  (placement 16)
  (parent EMBEDDING)
  (mhelp ""))

(defmenuitem SK1
  (display-name "SK1")
  (placement 2)
  (command "SK1")
  (parent SKOLEMIZE)
  (mhelp ""))

(defmenuitem SK3
  (display-name "SK3")
  (placement 3)
  (command "SK3")
  (parent SKOLEMIZE)
  (mhelp ""))

(defmenu INNER-QUANT-OPS
  (display-name "Inner Quant Ops")
  (placement 16)
  (parent EMBEDDING)
  (mhelp ""))

(defmenuitem DB
  (display-name "DB")
  (placement 2)
  (command "DB")
  (parent INNER-QUANT-OPS)
  (mhelp ""))

(defmenuitem EP
  (display-name "EP")
  (placement 3)
  (command "EP")
  (parent INNER-QUANT-OPS)
  (mhelp ""))

(defmenuitem OP
  (display-name "OP")
  (placement 4)
  (command "OP")
  (parent INNER-QUANT-OPS)
  (mhelp ""))

(defmenu ILL-FORMED-WFF-OPS
  (display-name "Ill-Formed Wff Ops")
  (placement 16)
  (parent EMBEDDING)
  (mhelp ""))

(defmenuitem DUPW
  (display-name "DUPW")
  (placement 2)
  (command "DUPW")
  (parent ILL-FORMED-WFF-OPS)
  (mhelp ""))

(defmenuitem EDILL
  (display-name "EDILL")
  (placement 3)
  (command "EDILL")
  (parent ILL-FORMED-WFF-OPS)
  (etps t)
  (mhelp ""))

(defmenuitem ILL
  (display-name "ILL")
  (placement 4)
  (command "ILL")
  (parent ILL-FORMED-WFF-OPS)
  (mhelp ""))

(defmenuitem TP
  (display-name "TP")
  (placement 5)
  (command "TP")
  (parent ILL-FORMED-WFF-OPS)
  (mhelp ""))

(defmenuitem WFFP
  (display-name "WFFP")
  (placement 6)
  (command "WFFP")
  (parent ILL-FORMED-WFF-OPS)
  (mhelp ""))

