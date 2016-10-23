;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of CONCEPT-WFF)


;*;(nocompile
;*;(defvariable cfontfns
;*;  ('(version 8 "14-APR-81 14:47:22")
;*;   aleph alpha and angle approx assert assertedby asterisk bar beta
;*;   bigbar bolda boldb boldc boldd bolde boldf boldg boldh boldi
;*;   boldj boldk boldl boldm boldn boldo boldp boldq boldr bolds boldt
;*;   boldu boldv boldw boldx boldy boldz capdelta capgamma caplambda
;*;   capomega capphi cappi cappsi capsigma captheta capupsilon capxi
;*;   ceiling1 ceiling2 chi circledot circleminus compose congruent
;*;   defineeq del delta diamond directsum divide doubtilde epsilon
;*;   equiv eta exists falsehood flat floor1 floor2 forall gamma
;*;   gradient greateq iff1 iff2 imp1 imp2 imp3 implied1 implied2
;*;   impliedby implies infinity integral2 intersect iota join kappa
;*;   lambda leftcorner lesseq meet member1 minplus monus mu natural
;*;   neg newpar nonmember norm north northeast northwest not notassert
;*;   noteq notequiv notvalid nu nullset omega omicron or parallelogram
;*;   phi phi2 pi plusmin propersubset propersuperset psi quantifier rho
;*;   rightcorner scripta scriptb scriptc scriptd scripte scriptf
;*;   scriptg scripth scripti scriptj scriptk scriptl scriptm scriptn
;*;   scripto scriptp scriptq scriptr scripts scriptt scriptu scriptv
;*;   scriptw scriptx scripty scriptz setintersect setunion sigma
;*;   similar south southeast southwest sqrt square star sub0 sub1 sub2
;*;   sub3 sub4 sub5 sub6 sub7 sub8 sub9 subalpha subbeta subchi
;*;   subdelta subepsilon subeta subgamma subiota subkappa sublambda
;*;   sublparen submember submu subnu subnullset subomega subomicron
;*;   subphi subpi subpsi subrho subrparen subset subsigma subtau
;*;   subtheta subupsilon subxi subzeta sup0 sup1 sup2 sup3 sup4 sup5
;*;   sup6 sup7 sup8 sup9 supa supb supc supd supe superset supf supg
;*;   suph supi supj supk supl suplparen supm supminus supn supo supp supplus
;*;   supq supr suprparen sups supt supu supv supw supx supy supz tau tensor
;*;   theta times truth uncappi union untilde upsilon valid xi zeta))
;*;)

(part-of concept-wff)

(deffile cfont
  (part-of concept-wff)
  (extension lsp)
  (mhelp "Defines characters for printing and parsing on Concepts."))

(context concept-terminal)

(defcfont aleph
  (cfont 3 65))

(defcfont alpha
  (end-symbol t)
  (cfont 3 97))

(defcfont and
  (end-symbol t)
  (cfont 1 91))

(defcfont angle
  (cfont 3 78))

(defcfont approx
  (cfont 3 39))

(defcfont assert
  (cfont 1 123))

(defcfont assertedby
  (cfont 3 86))

(defcfont asterisk
  (end-symbol t)
  (cfont 3 90))

(defcfont bar
  (cfont 1 124))

(defcfont beta
  (end-symbol t)
  (cfont 3 98))

(defcfont bigbar
  (cfont 3 127))

(defcfont bolda
  (cfont 1 1))

(defcfont boldb
  (cfont 1 2))

(defcfont boldc
  (cfont 1 3))

(defcfont boldd
  (cfont 1 4))

(defcfont bolde
  (cfont 1 5))

(defcfont boldf
  (cfont 1 6))

(defcfont boldg
  (cfont 1 7))

(defcfont boldh
  (cfont 1 8))

(defcfont boldi
  (cfont 1 9))

(defcfont boldj
  (cfont 1 10))

(defcfont boldk
  (cfont 1 11))

(defcfont boldl
  (cfont 1 12))

(defcfont boldm
  (cfont 1 13))

(defcfont boldn
  (cfont 1 14))

(defcfont boldo
  (cfont 1 15))

(defcfont boldp
  (cfont 1 16))

(defcfont boldq
  (cfont 1 17))

(defcfont boldr
  (cfont 1 18))

(defcfont bolds
  (cfont 1 19))

(defcfont boldt
  (cfont 1 20))

(defcfont boldu
  (cfont 1 21))

(defcfont boldv
  (cfont 1 22))

(defcfont boldw
  (cfont 1 23))

(defcfont boldx
  (cfont 1 24))

(defcfont boldy
  (cfont 1 25))

(defcfont boldz
  (cfont 1 26))

(defcfont capdelta
  (cfont 3 68))

(defcfont capgamma
  (cfont 3 71))

(defcfont caplambda
  (cfont 3 76))

(defcfont capomega
  (cfont 3 87))

(defcfont capphi
  (cfont 3 70))

(defcfont cappi
  (cfont 3 80))

(defcfont cappsi
  (cfont 3 89))

(defcfont capsigma
  (cfont 3 83))

(defcfont captheta
  (cfont 3 81))

(defcfont capupsilon
  (cfont 3 85))

(defcfont capxi
  (cfont 3 67))

(defcfont ceiling1
  (cfont 3 74))

(defcfont ceiling2
  (cfont 3 75))

(defcfont chi
  (end-symbol t)
  (cfont 3 120))

(defcfont circledot
  (cfont 3 38))

(defcfont circleminus
  (cfont 3 29))

(defcfont compose
  (cfont 1 42))

(defcfont congruent
  (cfont 3 93))

(defcfont defineeq
  (cfont 3 124))

(defcfont del
  (cfont 3 30))

(defcfont delta
  (end-symbol t)
  (cfont 3 100))

(defcfont diamond
  (cfont 3 34))

(defcfont directsum
  (cfont 1 38))

(defcfont divide
  (cfont 1 36))

(defcfont doubtilde
  (cfont 3 96))

(defcfont epsilon
  (end-symbol t)
  (cfont 3 101))

(defcfont equiv
  (end-symbol t)
  (cfont 1 61))

(defcfont eta
  (end-symbol t)
  (cfont 3 104))

(defcfont exists
  (end-symbol t)
  (cfont 1 39))

(defcfont falsehood
  (end-symbol t)
  (cfont 3 88))

(defcfont flat
  (cfont 1 127))

(defcfont floor1
  (cfont 3 37))

(defcfont floor2
  (cfont 3 94))

(defcfont forall
  (end-symbol t)
  (cfont 1 59))

(defcfont gamma
  (end-symbol t)
  (cfont 3 103))

(defcfont gradient
  (cfont 3 72))

(defcfont greateq
  (cfont 3 63))

(defcfont iff1
  (cfont 3 35))

(defcfont iff2
  (cfont 1 43))

(defcfont imp1
  (cfont 1 35))

(defcfont imp2
  (cfont 1 95))

(defcfont imp3
  (cfont 3 33))

(defcfont implied1
  (cfont 1 64))

(defcfont implied2
  (cfont 3 64))

(defcfont impliedby
  (cfont 3 45))

(defcfont implies
  (end-symbol t)
  (cfont 1 45))

(defcfont infinity
  (cfont 3 73))

(defcfont integral2
  (cfont 3 31))

(defcfont intersect
  (end-symbol t)
  (cfont 1 44))

(defcfont iota
  (end-symbol t)
  (cfont 3 105))

(defcfont join
  (cfont 3 46))

(defcfont kappa
  (end-symbol t)
  (cfont 3 107))

(defcfont lambda
  (end-symbol t)
  (cfont 3 108))

(defcfont leftcorner
  (cfont 3 27))

(defcfont lesseq
  (cfont 1 63))

(defcfont meet
  (cfont 3 44))

(defcfont member1
  (cfont 3 118))

(defcfont minplus
  (cfont 1 28))

(defcfont monus
  (cfont 3 36))

(defcfont mu
  (end-symbol t)
  (cfont 3 109))

(defcfont natural
  (cfont 1 30))

(defcfont neg
  (cfont 1 29))

(defcfont newpar
  (cfont 1 33))

(defcfont nonmember
  (cfont 3 69))

(defcfont norm
  (cfont 1 93))

(defcfont north
  (cfont 3 91))

(defcfont northeast
  (cfont 3 60))

(defcfont northwest
  (cfont 3 82))

(defcfont not
  (cfont 1 96))

(defcfont notassert
  (cfont 3 123))

(defcfont noteq
  (cfont 3 61))

(defcfont notequiv
  (cfont 3 126))

(defcfont notvalid
  (cfont 3 125))

(defcfont nu
  (end-symbol t)
  (cfont 3 110))

(defcfont nullset
  (cfont 1 126))

(defcfont omega
  (end-symbol t)
  (cfont 3 119))

(defcfont omicron
  (end-symbol t)
  (cfont 3 111))

(defcfont or
  (end-symbol t)
  (cfont 1 92))

(defcfont parallelogram
  (cfont 3 77))

(defcfont phi
  (end-symbol t)
  (cfont 3 102))

(defcfont phi2
  (end-symbol t)
  (cfont 3 106))

(defcfont pi
  (end-symbol t)
  (cfont 3 112))

(defcfont plusmin
  (cfont 1 27))

(defcfont powerset
  (cfont 1 80))

(defcfont propersubset
  (end-symbol t)
  (cfont 1 58))

(defcfont propersuperset
  (end-symbol t)
  (cfont 1 34))

(defcfont psi
  (end-symbol t)
  (cfont 3 121))

(defcfont quantifier
  (cfont 1 31))

(defcfont rho
  (end-symbol t)
  (cfont 3 114))

(defcfont rightcorner
  (cfont 3 28))

(defcfont scripta
  (cfont 1 65))

(defcfont scriptb
  (cfont 1 66))

(defcfont scriptc
  (cfont 1 67))

(defcfont scriptd
  (cfont 1 68))

(defcfont scripte
  (cfont 1 69))

(defcfont scriptf
  (cfont 1 70))

(defcfont scriptg
  (cfont 1 71))

(defcfont scripth
  (cfont 1 72))

(defcfont scripti
  (cfont 1 73))

(defcfont scriptj
  (cfont 1 74))

(defcfont scriptk
  (cfont 1 75))

(defcfont scriptl
  (cfont 1 76))

(defcfont scriptm
  (cfont 1 77))

(defcfont scriptn
  (cfont 1 78))

(defcfont scripto
  (cfont 1 79))

(defcfont scriptp
  (cfont 1 80))

(defcfont scriptq
  (cfont 1 81))

(defcfont scriptr
  (cfont 1 82))

(defcfont scripts
  (cfont 1 83))

(defcfont scriptt
  (cfont 1 84))

(defcfont scriptu
  (cfont 1 85))

(defcfont scriptv
  (cfont 1 86))

(defcfont scriptw
  (cfont 1 87))

(defcfont scriptx
  (cfont 1 88))

(defcfont scripty
  (cfont 1 89))

(defcfont scriptz
  (cfont 1 90))

(defcfont setintersect
  (end-symbol t)
  (cfont 1 60))

(defcfont setunion
  (end-symbol t)
  (cfont 1 62))

(defcfont sigma
  (end-symbol t)
  (cfont 3 115))

(defcfont similar
  (cfont 3 59))

(defcfont south
  (cfont 3 92))

(defcfont southeast
  (cfont 3 62))

(defcfont southwest
  (cfont 3 84))

(defcfont sqrt
  (cfont 1 0))

(defcfont square
  (cfont 3 58))

(defcfont star
  (cfont 3 42))

(defcfont sub0
  (end-symbol t)
  (cfont 3 48))

(defcfont sub1
  (end-symbol t)
  (cfont 3 49))

(defcfont sub2
  (end-symbol t)
  (cfont 3 50))

(defcfont sub3
  (end-symbol t)
  (cfont 3 51))

(defcfont sub4
  (end-symbol t)
  (cfont 3 52))

(defcfont sub5
  (end-symbol t)
  (cfont 3 53))

(defcfont sub6
  (end-symbol t)
  (cfont 3 54))

(defcfont sub7
  (end-symbol t)
  (cfont 3 55))

(defcfont sub8
  (end-symbol t)
  (cfont 3 56))

(defcfont sub9
  (end-symbol t)
  (cfont 3 57))

(defcfont subalpha
  (end-symbol t)
  (cfont 1 97))

(defcfont subbeta
  (end-symbol t)
  (cfont 1 98))

(defcfont subchi
  (end-symbol t)
  (cfont 1 120))

(defcfont subdelta
  (end-symbol t)
  (cfont 1 100))

(defcfont subepsilon
  (end-symbol t)
  (cfont 1 101))

(defcfont subeta
  (end-symbol t)
  (cfont 1 104))

(defcfont subgamma
  (end-symbol t)
  (cfont 1 103))

(defcfont subiota
  (end-symbol t)
  (cfont 1 105))

(defcfont subkappa
  (end-symbol t)
  (cfont 1 107))

(defcfont sublambda
  (end-symbol t)
  (cfont 1 108))

(defcfont sublparen
  (end-symbol t)
  (cfont 1 40))

(defcfont submember
  (end-symbol t)
  (cfont 1 118))

(defcfont submu
  (end-symbol t)
  (cfont 1 109))

(defcfont subnu
  (end-symbol t)
  (cfont 1 110))

(defcfont subnullset
  (end-symbol t)
  (cfont 1 106))

(defcfont subomega
  (end-symbol t)
  (cfont 1 119))

(defcfont subomicron
  (end-symbol t)
  (cfont 1 111))

(defcfont subphi
  (end-symbol t)
  (cfont 1 102))

(defcfont subpi
  (end-symbol t)
  (cfont 1 112))

(defcfont subpsi
  (end-symbol t)
  (cfont 1 121))

(defcfont subrho
  (end-symbol t)
  (cfont 1 114))

(defcfont subrparen
  (end-symbol t)
  (cfont 1 41))

(defcfont subset
  (end-symbol t)
  (cfont 1 47))

(defcfont subsigma
  (end-symbol t)
  (cfont 1 115))

(defcfont subtau
  (end-symbol t)
  (cfont 1 116))

(defcfont subtheta
  (end-symbol t)
  (cfont 1 113))

(defcfont subupsilon
  (end-symbol t)
  (cfont 1 117))

(defcfont subxi
  (end-symbol t)
  (cfont 1 99))

(defcfont subzeta
  (end-symbol t)
  (cfont 1 122))

(defcfont sup0
  (end-symbol t)
  (cfont 1 48))

(defcfont sup1
  (end-symbol t)
  (cfont 1 49))

(defcfont sup2
  (end-symbol t)
  (cfont 1 50))

(defcfont sup3
  (end-symbol t)
  (cfont 1 51))

(defcfont sup4
  (end-symbol t)
  (cfont 1 52))

(defcfont sup5
  (end-symbol t)
  (cfont 1 53))

(defcfont sup6
  (end-symbol t)
  (cfont 1 54))

(defcfont sup7
  (end-symbol t)
  (cfont 1 55))

(defcfont sup8
  (end-symbol t)
  (cfont 1 56))

(defcfont sup9
  (end-symbol t)
  (cfont 1 57))

(defcfont supa
  (end-symbol t)
  (cfont 3 1))

(defcfont supb
  (end-symbol t)
  (cfont 3 2))

(defcfont supc
  (end-symbol t)
  (cfont 3 3))

(defcfont supd
  (end-symbol t)
  (cfont 3 4))

(defcfont supe
  (end-symbol t)
  (cfont 3 5))

(defcfont superset
  (end-symbol t)
  (cfont 3 47))

(defcfont supf
  (end-symbol t)
  (cfont 3 6))

(defcfont supg
  (end-symbol t)
  (cfont 3 7))

(defcfont suph
  (end-symbol t)
  (cfont 3 8))

(defcfont supi
  (end-symbol t)
  (cfont 3 9))

(defcfont supj
  (end-symbol t)
  (cfont 3 10))

(defcfont supk
  (end-symbol t)
  (cfont 3 11))

(defcfont supl
  (end-symbol t)
  (cfont 3 12))

(defcfont suplparen
  (end-symbol t)
  (cfont 3 41))

(defcfont supm
  (end-symbol t)
  (cfont 3 13))

(defcfont supminus
  (end-symbol t)
  (cfont 3 95))

(defcfont supn
  (end-symbol t)
  (cfont 3 14))

(defcfont supo
  (end-symbol t)
  (cfont 3 15))

(defcfont supp
  (end-symbol t)
  (cfont 3 16))

(defcfont supplus
  (end-symbol t)
  (cfont 3 43))

(defcfont supq
  (end-symbol t)
  (cfont 3 17))

(defcfont supr
  (end-symbol t)
  (cfont 3 18))

(defcfont suprparen
  (end-symbol t)
  (cfont 3 41))

(defcfont sups
  (end-symbol t)
  (cfont 3 19))

(defcfont supt
  (end-symbol t)
  (cfont 3 20))

(defcfont supu
  (end-symbol t)
  (cfont 3 21))

(defcfont supv
  (end-symbol t)
  (cfont 3 22))

(defcfont supw
  (end-symbol t)
  (cfont 3 23))

(defcfont supx
  (end-symbol t)
  (cfont 3 24))

(defcfont supy
  (end-symbol t)
  (cfont 3 25))

(defcfont supz
  (end-symbol t)
  (cfont 3 26))

(defcfont tau
  (end-symbol t)
  (cfont 3 116))

(defcfont tensor
  (cfont 1 94))

(defcfont theta
  (end-symbol t)
  (cfont 3 113))

(defcfont times
  (end-symbol t)
  (cfont 1 37))

(defcfont truth
  (cfont 3 66))

(defcfont uncappi
  (cfont 3 79))

(defcfont union
  (end-symbol t)
  (cfont 1 46))

(defcfont untilde
  (cfont 3 0))

(defcfont upsilon
  (end-symbol t)
  (cfont 3 117))

(defcfont valid
  (cfont 1 125))

(defcfont xi
  (end-symbol t)
  (cfont 3 99))

(defcfont zeta
  (end-symbol t)
  (cfont 3 122))

