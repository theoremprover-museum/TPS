;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of SCRIBE-WFF)

(part-of scribe-wff)

(deffile dfont
  (part-of scribe-wff)
  (extension lsp)
  (mhelp "Defines SCRIBE style characters."))

;*;(defcontext greek-letters-lowercase
;*;  (short-id "Lowercase Greek")
;*;  (order 37)
;*;  (mhelp "Lowercase Greek letters."))

(context greek-letters-lowercase)

(defscribefont alpha
  (dfont "g{a}"))

(defscribefont beta
  (dfont "g{b}"))

(defscribefont gamma
  (dfont "g{g}"))

(defscribefont delta
  (dfont "g{d}"))

(defscribefont epsilon
  (dfont "g{e}"))

(defscribefont zeta
  (dfont "g{z}"))

(defscribefont eta
  (dfont "g{h}"))

(defscribefont theta
  ;(dfont "g{q}")
  (dfont "theta1"))

(defscribefont iota
  (dfont "g{i}"))

(defscribefont kappa
  (dfont "g{k}"))

(defscribefont lambda
  (dfont "g{l}"))

(defscribefont mu
  (dfont "g{m}"))

(defscribefont nu
  (dfont "g{n}"))

(defscribefont xi
  (dfont "g{x}"))

(defscribefont omicron
  (dfont "g{o}"))

(defscribefont pi
  (dfont "g{p}"))

(defscribefont rho
  (dfont "g{r}"))

(defscribefont sigma
  (dfont "g{s}"))

(defscribefont tau
  (dfont "g{t}"))

(defscribefont upsilon
  (dfont "g{u}"))

(defscribefont phi
  (dfont "g{f}"))

(defscribefont chi
  (dfont "g{c}"))

(defscribefont psi
  (dfont "g{y}"))

(defscribefont omega
  ;(dfont "g{w}")
  (dfont "omega1"))

;*;(defcontext greek-letters-uppercase
;*;  (short-id "Uppercase Greek")
;*;  (order 38)
;*;  (mhelp "Uppercase Greek letters."))

(context greek-letters-uppercase)

(defscribefont capalpha
  (dfont "g{A}"))

(defscribefont capbeta
  (dfont "g{B}"))

(defscribefont capgamma
  (dfont "g{G}"))

(defscribefont capdelta
  (dfont "g{D}"))

(defscribefont capepsilon
  (dfont "g{E}"))

(defscribefont capzeta
  (dfont "g{Z}"))

(defscribefont capeta
  (dfont "g{H}"))

(defscribefont captheta
  (dfont "g{Q}"))

(defscribefont capiota
  (dfont "g{I}"))

(defscribefont capkappa
  (dfont "g{K}"))

(defscribefont caplambda
  (dfont "g{L}"))

(defscribefont capmu
  (dfont "g{M}"))

(defscribefont capnu
  (dfont "g{N}"))

(defscribefont capxi
  (dfont "g{X}"))

(defscribefont capomicron
  (dfont "g{O}"))

(defscribefont cappi
  (dfont "g{P}"))

(defscribefont caprho
  (dfont "g{R}"))

(defscribefont capsigma
  (dfont "g{S}"))

(defscribefont captau
  (dfont "g{T}"))

(defscribefont capupsilon
  (dfont "g{U}"))

(defscribefont capphi
  (dfont "g{F}"))

(defscribefont capchi
  (dfont "g{C}"))

(defscribefont cappsi
  (dfont "g{Y}"))

(defscribefont capomega
  (dfont "g{W}"))

;*;(defcontext bold-letters
;*;  (short-id "Bold Letters")
;*;  (order 40)
;*;  (mhelp "Upper case very bold letters."))

(context bold-letters)

(defscribefont bolda
  (dfont "bolda"))

(defscribefont boldb
  (dfont "boldb"))

(defscribefont boldc
  (dfont "boldc"))

(defscribefont boldd
  (dfont "boldd"))

(defscribefont bolde
  (dfont "bolde"))

(defscribefont boldf
  (dfont "boldf"))

(defscribefont boldg
  (dfont "boldg"))

(defscribefont boldh
  (dfont "boldh"))

(defscribefont boldi
  (dfont "boldi"))

(defscribefont boldj
  (dfont "boldj"))

(defscribefont boldk
  (dfont "boldk"))

(defscribefont boldl
  (dfont "boldl"))

(defscribefont boldm
  (dfont "boldm"))

(defscribefont boldn
  (dfont "boldn"))

(defscribefont boldo
  (dfont "boldo"))

(defscribefont boldp
  (dfont "boldp"))

(defscribefont boldq
  (dfont "boldq"))

(defscribefont boldr
  (dfont "boldr"))

(defscribefont bolds
  (dfont "bolds"))

(defscribefont boldt
  (dfont "boldt"))

(defscribefont boldu
  (dfont "boldu"))

(defscribefont boldv
  (dfont "boldv"))

(defscribefont boldw
  (dfont "boldw"))

(defscribefont boldx
  (dfont "boldx"))

(defscribefont boldy
  (dfont "boldy"))

(defscribefont boldz
  (dfont "boldz"))

;*;(defcontext script-letters
;*;  (short-id "Script Letters")
;*;  (order 34)
;*;  (mhelp "Uppercase script letters."))

(context script-letters)

(defscribefont scripta
  (dfont "scripta"))

(defscribefont scriptb
  (dfont "scriptb"))

(defscribefont scriptc
  (dfont "scriptc"))

(defscribefont scriptd
  (dfont "scriptd"))

(defscribefont scripte
  (dfont "scripte"))

(defscribefont scriptf
  (dfont "scriptf"))

(defscribefont scriptg
  (dfont "scriptg"))

(defscribefont scripth
  (dfont "scripth"))

(defscribefont scripti
  (dfont "scripti"))

(defscribefont scriptj
  (dfont "scriptj"))

(defscribefont scriptk
  (dfont "scriptk"))

(defscribefont scriptl
  (dfont "scriptl"))

(defscribefont scriptm
  (dfont "scriptm"))

(defscribefont scriptn
  (dfont "scriptn"))

(defscribefont scripto
  (dfont "scripto"))

(defscribefont scriptp
  (dfont "scriptp"))

(defscribefont scriptq
  (dfont "scriptq"))

(defscribefont scriptr
  (dfont "scriptr"))

(defscribefont scripts
  (dfont "scripts"))

(defscribefont scriptt
  (dfont "scriptt"))

(defscribefont scriptu
  (dfont "scriptu"))

(defscribefont scriptv
  (dfont "scriptv"))

(defscribefont scriptw
  (dfont "scriptw"))

(defscribefont scriptx
  (dfont "scriptx"))

(defscribefont scripty
  (dfont "scripty"))

(defscribefont scriptz
  (dfont "scriptz"))

;*;(defcontext subscripts
;*;  (short-id "Subscripts")
;*;  (order 35)
;*;  (mhelp "Non-greek subscript symbols."))

(context subscripts)

(defscribefont sub0
  (dfont "-{0}"))

(defscribefont sub1
  (dfont "-{1}"))

(defscribefont sub2
  (dfont "-{2}"))

(defscribefont sub3
  (dfont "-{3}"))

(defscribefont sub4
  (dfont "-{4}"))

(defscribefont sub5
  (dfont "-{5}"))

(defscribefont sub6
  (dfont "-{6}"))

(defscribefont sub7
  (dfont "-{7}"))

(defscribefont sub8
  (dfont "-{8}"))

(defscribefont sub9
  (dfont "-{9}"))

(defscribefont sublparen
  (dfont "-{(}"))

(defscribefont submember
  (dfont "submember"))

(defscribefont subnullset
  (dfont "subnullset"))

(defscribefont subrparen
  (dfont "-{)}"))

;*;(defcontext greek-subscripts
;*;  (short-id "Greek Subscripts")
;*;  (order 39)
;*;  (mhelp "Greek Subscripts as used for type symbols."))

(context greek-subscripts)

(defscribefont subalpha
  (dfont "-{@g{a}}"))

(defscribefont subbeta
  (dfont "-{@g{b}}"))

(defscribefont subgamma
  (dfont "-{@g{g}}"))

(defscribefont subdelta
  (dfont "-{@g{d}}"))

(defscribefont subepsilon
  (dfont "-{@g{e}}"))

(defscribefont subzeta
  (dfont "-{@g{z}}"))

(defscribefont subeta
  (dfont "-{@g{h}}"))

(defscribefont subtheta
  (dfont "-{@g{q}}"))

(defscribefont subiota
  (dfont "-{@g{i}}"))

(defscribefont subkappa
  (dfont "-{@g{k}}"))

(defscribefont sublambda
  (dfont "-{@g{l}}"))

(defscribefont submu
  (dfont "-{@g{m}}"))

(defscribefont subnu
  (dfont "-{@g{n}}"))

(defscribefont subxi
  (dfont "-{@g{x}}"))

(defscribefont subomicron
  (dfont "-{@g{o}}"))

(defscribefont subpi
  (dfont "-{@g{p}}"))

(defscribefont subrho
  (dfont "-{@g{r}}"))

(defscribefont subsigma
  (dfont "-{@g{s}}"))

(defscribefont subtau
  (dfont "-{@g{t}}"))

(defscribefont subupsilon
  (dfont "-{@g{u}}"))

(defscribefont subphi
  (dfont "-{@g{f}}"))

(defscribefont subchi
  (dfont "-{@g{c}}"))

(defscribefont subpsi
  (dfont "-{@g{y}}"))

(defscribefont subomega
  (dfont "-{@g{w}}"))

;*;(defcontext superscripts
;*;  (short-id "Superscripts")
;*;  (order 36)
;*;  (mhelp "Symbols which print as superscripts."))

(context superscripts)

(defscribefont sup0
  (dfont "+{0}"))

(defscribefont sup1
  (dfont "+{1}"))

(defscribefont sup2
  (dfont "+{2}"))

(defscribefont sup3
  (dfont "+{3}"))

(defscribefont sup4
  (dfont "+{4}"))

(defscribefont sup5
  (dfont "+{5}"))

(defscribefont sup6
  (dfont "+{6}"))

(defscribefont sup7
  (dfont "+{7}"))

(defscribefont sup8
  (dfont "+{8}"))

(defscribefont sup9
  (dfont "+{9}"))

(defscribefont supa
  (dfont "+{a}"))

(defscribefont supb
  (dfont "+{b}"))

(defscribefont supc
  (dfont "+{c}"))

(defscribefont supd
  (dfont "+{d}"))

(defscribefont supe
  (dfont "+{e}"))

(defscribefont supf
  (dfont "+{f}"))

(defscribefont supg
  (dfont "+{g}"))

(defscribefont suph
  (dfont "+{h}"))

(defscribefont supi
  (dfont "+{i}"))

(defscribefont supj
  (dfont "+{j}"))

(defscribefont supk
  (dfont "+{k}"))

(defscribefont supl
  (dfont "+{l}"))

(defscribefont suplparen
  (dfont "+{(}"))

(defscribefont supm
  (dfont "+{m}"))

(defscribefont supminus
  (dfont "+{-}"))

(defscribefont supn
  (dfont "+{n}"))

(defscribefont supo
  (dfont "+{o}"))

(defscribefont supp
  (dfont "+{p}"))

(defscribefont supplus
  (dfont "+{+}"))

(defscribefont supq
  (dfont "+{q}"))

(defscribefont supr
  (dfont "+{r}"))

(defscribefont suprparen
  (dfont "+{)}"))

(defscribefont sups
  (dfont "+{s}"))

(defscribefont supt
  (dfont "+{t}"))

(defscribefont supu
  (dfont "+{u}"))

(defscribefont supv
  (dfont "+{v}"))

(defscribefont supw
  (dfont "+{w}"))

(defscribefont supx
  (dfont "+{x}"))

(defscribefont supy
  (dfont "+{y}"))

(defscribefont supz
  (dfont "+{z}"))

(context misc-symbols)

(defscribefont aleph
  (dfont "aleph"))

(defscribefont approx
  (dfont "approx"))

(defscribefont assertedby
  (dfont "assertedby"))

(defscribefont asterisk
  (dfont "ast"))

(defscribefont ceiling1
  (dfont "ceiling1"))

(defscribefont ceiling2
  (dfont "ceiling2"))

(defscribefont circledot
  (dfont "circledot"))

(defscribefont circleminus
  (dfont "ominus"))

(defscribefont compose
  (dfont "compose"))

(defscribefont del
  (dfont "partial"))

(defscribefont diamond
  (dfont "diamond"))

(defscribefont directsum
  (dfont "directsum"))

(defscribefont divide
  (dfont "divide"))

(defscribefont doubtilde
  (dfont "approx"))

;;; Added 8/29/87 DAN

(defscribefont eqp
  (dfont "eqp"))

(defscribefont falsehood
  (dfont "falsehood"))

(defscribefont floor1
  (dfont "floor1"))

(defscribefont floor2
  (dfont "floor2"))

(defscribefont gradient
  (dfont "nabla"))

(defscribefont greateq
  (dfont "greateq"))

(defscribefont iff1
  (dfont "iff1"))

(defscribefont iff2
  (dfont "iff2"))

(defscribefont imp1
  (dfont "imp1"))

(defscribefont imp2
  (dfont "imp2"))

(defscribefont imp3
  (dfont "imp3"))

(defscribefont implied1
  (dfont "implied1"))

(defscribefont implied2
  (dfont "implied2"))

(defscribefont impliedby
  (dfont "impliedby"))


(defscribefont infinity
  (dfont "infinity"))

(defscribefont intersect
  (dfont "intersect"))

(defscribefont join
  (dfont "join"))

(defscribefont lesseq
  (dfont "lesseq"))

(defscribefont meet
  (dfont "meet"))

(defscribefont member1
  (dfont "member1"))

(defscribefont minplus
  (dfont "mp"))


(defscribefont newpar
  (dfont "newpar"))

(defscribefont nonmember
  (dfont "nonmember"))

(defscribefont norm
  ;(dfont "dvbar")
  (dfont "norm1"))

(defscribefont north
  (dfont "north"))

(defscribefont northeast
  (dfont "northeast"))

(defscribefont northwest
  (dfont "northwest"))


(defscribefont notassert
  (dfont "notassert"))

(defscribefont noteq
  (dfont "noteq"))

(defscribefont notequiv
  (dfont "notequiv"))

(defscribefont notvalid
  (dfont "notvalid"))

;;; added 2JAN91 DAN
(defscribefont nullset
  (dfont "emptyset"))

(defscribefont phi2
  (dfont "phi2"))

;; Added 8/29/87 DAN

(defscribefont one
  (dfont "one"))

(defscribefont plusmin
  (dfont "pm"))

(defscribefont powerset
  (dfont "powerset"))

(defscribefont propersubset
  (dfont "PrSubset"))

(defscribefont propersuperset
  (dfont "PrSupset"))

(defscribefont setintersect
  (dfont "setintersect"))

(defscribefont setunion
  (dfont "setunion"))

(defscribefont similar
  (dfont "similar"))

(defscribefont south
  (dfont "south"))

(defscribefont southeast
  (dfont "southeast"))

(defscribefont sqrt
  (dfont "squareroot"))

(defscribefont square
  (dfont "square"))

(defscribefont star
  (dfont "star"))

(defscribefont subset
  (dfont "subset"))

(defscribefont superset
  (dfont "supset"))

(defscribefont tensor
  (dfont "tensor"))

(defscribefont times
  (dfont "times"))

(defscribefont truth
  (dfont "truth"))

(defscribefont union
  (dfont "union"))

(defscribefont valid
  (dfont "valid"))
