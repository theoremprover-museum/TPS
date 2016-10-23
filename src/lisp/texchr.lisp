;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of TEX-WFF)

;;;
;;; File: TexChr
;;; Package: Tex-Wff
;;;
;;; defines a few Tex special characters.
;;;

(part-of tex-wff)

(deffile texchr
  (part-of tex-wff)
  (extension lsp)
  (mhelp "Defines some TeX characters."))

(context tex-style)


;;;------******-------

(deftexfont ->I
  (texname "arrowi"))

(deftexfont ->E
  (texname "arrowe"))

(deftexfont ae
  (texname "foralle"))

(deftexfont ai
  (texname "foralli"))

(deftexfont aleph
  (texname "aleph"))

(deftexfont alpha
  (texname "alpha"))

(deftexfont and
  (texname "land"))

(deftexfont andi
  (texname "andi"))

(deftexfont andnot
  (texname "andnot"))

(deftexfont angle 
  (texname "angle"))

(deftexfont approx
  (texname "approx"))

(deftexfont arrow
  (texname "rightarrow"))

(deftexfont assert
  (texname "vdash"))

(deftexfont assertedby
  (texname "dashv"))

(deftexfont asterisk
  (texname "ast"))

(deftexfont bar
  (texname "vert"))

(deftexfont beta
  (texname "beta"))

(deftexfont bigbar
  (texname "bigbar"))

(deftexfont bolda
  (texname "bolda"))

(deftexfont boldb
  (texname "boldb"))

(deftexfont boldc
  (texname "boldc"))

(deftexfont boldd
  (texname "boldd"))

(deftexfont bolde
  (texname "bolde"))

(deftexfont boldf
  (texname "boldf"))

(deftexfont boldg
  (texname "boldg"))

(deftexfont boldh
  (texname "boldh"))

(deftexfont boldi
  (texname "boldi"))

(deftexfont boldj
  (texname "boldj"))

(deftexfont boldk
  (texname "boldk"))

(deftexfont boldl
  (texname "boldl"))

(deftexfont boldm
  (texname "boldm"))

(deftexfont boldn
  (texname "boldn"))

(deftexfont boldo
  (texname "boldo"))

(deftexfont boldp
  (texname "boldp"))

(deftexfont boldq
  (texname "boldq"))

(deftexfont boldr
  (texname "boldr"))

(deftexfont bolds
  (texname "bolds"))

(deftexfont boldt
  (texname "boldt"))

(deftexfont boldu
  (texname "boldu"))

(deftexfont boldv
  (texname "boldv"))

(deftexfont boldw
  (texname "boldw"))

(deftexfont boldx
  (texname "boldx"))

(deftexfont boldy
  (texname "boldy"))

(deftexfont boldz
  (texname "boldz"))

(deftexfont bot
  (texname "botc"))

(deftexfont bottom
  (texname "perp"))

;;;----*******------

(deftexfont capalpha
  (texname "Alpha"))

(deftexfont capbeta
  (texname "Beta"))

(deftexfont capchi
  (texname "Chi"))

(deftexfont capdelta
  (texname "Delta"))

(deftexfont capepsilon
  (texname "Epsilon"))

(deftexfont capeta
  (texname "Eta"))

(deftexfont capgamma
  (texname "Gamma"))

(deftexfont capiota
  (texname "Iota"))

(deftexfont capkappa
  (texname "Kappa"))

(deftexfont caplambda
  (texname "Lambda"))

(deftexfont capmu
  (texname "Mu"))

(deftexfont capnu
  (texname "Nu"))

(deftexfont capomega
  (texname "Omega"))

(deftexfont capomicron
  (texname "Omicron"))

(deftexfont capphi
  (texname "Phi"))

(deftexfont cappi
  (texname "Pi"))

(deftexfont cappsi
  (texname "Psi"))

(deftexfont caprho
  (texname "Rho"))

(deftexfont capsigma
  (texname "Sigma"))

(deftexfont captau
  (texname "Tau"))

(deftexfont captheta
  (texname "Theta"))

(deftexfont capupsilon
  (texname "Upsilon"))

(deftexfont capxi
  (texname "Xi"))

(deftexfont capzeta
  (texname "Zeta"))

(deftexfont ceiling1
  (texname "lceil"))

(deftexfont ceiling2
  (texname "rceil"))

(deftexfont chi
  (texname "chi"))

(deftexfont circledot
  (texname "odot"))

(deftexfont circleminus
  (texname "ominus"))

(deftexfont compose
  (texname "circ"))

(deftexfont cond
  (texname "cond"))

(deftexfont contraction
  (texname "contr"))

(deftexfont cut
  (texname "cut"))

(deftexfont defn
  (texname "defn"))

(deftexfont del
  (texname "partial"))

(deftexfont delta
  (texname "delta"))

(deftexfont diamond
  (texname "diamond"))

(deftexfont directsum
  (texname "oplus"))

(deftexfont divide
  (texname "div"))

(deftexfont elbow
  (texname "neg"))

(deftexfont eqp 
  (texname "leqp"))

(deftexfont epsilon
  (texname "epsilon"))

(deftexfont equiv
  (texname "equiv"))

(deftexfont eta
  (texname "eta"))

(deftexfont exists
  (texname "exists"))

(deftexfont existsi
  (texname "existsi"))

(deftexfont existsnot
  (texname "existsnot"))

(deftexfont falsehood
  (texname "bot")) 

(deftexfont finite
  (texname "finite"))

(deftexfont flat
  (texname "flat"))

(deftexfont floor1
  (texname "lfloor")) 

(deftexfont floor2
  (texname "rfloor")) 

(deftexfont forall
  (texname "forall"))

(deftexfont foralli
  (texname "foralli"))

(deftexfont forallnot
  (texname "forallnot"))

(deftexfont gamma
  (texname "gamma"))

(deftexfont gradient
  (texname "nabla"))

(deftexfont greateq
  (texname "geq"))

(deftexfont iff1
  (texname "leftrightarrow"))

(deftexfont iff2
  (texname "Leftrightarrow"))

(deftexfont imp1
  (texname "rightarrow"))

(deftexfont imp2
  (texname "Rightarrow"))

(deftexfont implied1
  (texname "leftarrow"))

(deftexfont implied2
  (texname "Leftarrow"))

(deftexfont impliedby
  (texname "subset"))

(deftexfont implies
  (texname "supset"))

(deftexfont infinity
  (texname "infty"))

(deftexfont integral2
  (texname "lintegral"))

(deftexfont join
   (texname "bigvee"))

(deftexfont intersect 
  (texname "cap"))

(deftexfont iota
  (texname "iota"))

(deftexfont kappa
  (texname "kappa"))

(deftexfont lambda
  (texname "lambda"))

(deftexfont lesseq
  (texname "leq"))

(deftexfont meet
  (texname "bigwedge"))

(deftexfont member1
  (texname "in"))

(deftexfont minplus
  (texname "mp"))

(deftexfont mix
  (texname "mix"))

(deftexfont mu
  (texname "mu"))

(deftexfont nat
  (texname "nat"))

(deftexfont nc
  (texname "nc"))

(deftexfont neg
  (texname "neg"))

(deftexfont nonmember
  (texname "not\\in"))

(deftexfont norm
  (texname "Vert"))

(deftexfont north
  (texname "uparrow"))

(deftexfont northeast
  (texname "nearrow"))

(deftexfont northwest
  (texname "nwarrow"))

(deftexfont not
  (texname "sim"))

(deftexfont notassert
  (texname "not\\vdash"))

(deftexfont noteq
  (texname "not="))

(deftexfont notequiv
  (texname "not\\equiv"))

(deftexfont notnot
  (texname "notnot"))

(deftexfont notvalid
  (texname "not\\models"))

(deftexfont nu
  (texname "nu"))

(deftexfont nullset
  (texname "emptyset"))

(deftexfont omega
  (texname "omega"))

(deftexfont omicron
  (texname "omicron"))

(deftexfont one
  (texname "lone"))

(deftexfont or
  (texname "lor"))

(deftexfont ori
  (texname "ori"))

(deftexfont ornot
  (texname "ornot"))

(deftexfont phi
  (texname "phi"))

(deftexfont phi2
  (texname "emptyset"))

(deftexfont pi
  (texname "pi"))

(deftexfont plusmin
  (texname "pm"))

(deftexfont propersubset
  (texname "subset"))

(deftexfont propersuperset
  (texname "supset"))

(deftexfont powerset
  (texname "powerset"))

(deftexfont psi
  (texname "psi"))

(deftexfont recursion
  (texname "recursion"))

(deftexfont rho
  (texname "rho"))

;;;---***---

(deftexfont scripta
  (texname "scripta"))

(deftexfont scriptb
  (texname "scriptb"))

(deftexfont scriptc
  (texname "scriptc"))

(deftexfont scriptd
  (texname "scriptd"))

(deftexfont scripte
  (texname "scripte"))

(deftexfont scriptf
  (texname "scriptf"))

(deftexfont scriptg
  (texname "scriptg"))

(deftexfont scripth
  (texname "scripth"))

(deftexfont scripti
  (texname "scripti"))

(deftexfont scriptj
  (texname "scriptj"))

(deftexfont scriptk
  (texname "scriptk"))

(deftexfont scriptl
  (texname "scriptl"))

(deftexfont scriptm
  (texname "scriptm"))

(deftexfont scriptn
  (texname "scriptn"))

(deftexfont scripto
  (texname "scripto"))

(deftexfont scriptp
  (texname "scriptp"))

(deftexfont scriptq
  (texname "scriptq"))

(deftexfont scriptr
  (texname "scriptr"))

(deftexfont scripts
  (texname "scripts"))

(deftexfont scriptt
  (texname "scriptt"))

(deftexfont scriptu
  (texname "scriptu"))

(deftexfont scriptv
  (texname "scriptv"))

(deftexfont scriptw
  (texname "scriptw"))

(deftexfont scriptx
  (texname "scriptx"))

(deftexfont scripty
  (texname "scripty"))

(deftexfont scriptz
  (texname "scriptz"))

;;;---***----

(deftexfont setintersect 
  (texname "bigcap"))

(deftexfont setunion
  (texname "bigcup"))

(deftexfont sigma
  (texname "sigma"))

(deftexfont similar
  (texname "approx"))

(deftexfont south
  (texname "downarrow"))

(deftexfont southeast
  (texname "searrow"))

(deftexfont southwest
  (texname "swarrow"))

(deftexfont sqrt
  (texname "sqrt x"))

(deftexfont square
  (texname "square"))

(deftexfont star
  (texname "star"))

(deftexfont sub0
  (texname "sub0"))

(deftexfont sub1
  (texname "sub1"))

(deftexfont sub2
  (texname "sub2"))

(deftexfont sub3
  (texname "sub3"))

(deftexfont sub4
  (texname "sub4"))

(deftexfont sub5
  (texname "sub5"))

(deftexfont sub6
  (texname "sub6"))

(deftexfont sub7
  (texname "sub7"))

(deftexfont sub8
  (texname "sub8"))

(deftexfont sub9
  (texname "sub9"))

(deftexfont subalpha
  (texname "sub\\alpha"))

(deftexfont subbeta
  (texname "sub\\beta"))

(deftexfont succ
  (texname "succ"))

(deftexfont subchi
  (texname "sub\\chi"))

(deftexfont subdelta
  (texname "sub\\delta"))

(deftexfont subepsilon
  (texname "sub\\epsilon"))

(deftexfont subeta
  (texname "sub\\eta"))

(deftexfont subgamma
  (texname "sub\\gamma"))

(deftexfont subiota
  (texname "sub\\iota"))

(deftexfont subkappa
  (texname "sub\\kappa"))

(deftexfont sublambda
  (texname "sub\\lambda"))

(deftexfont sublparen
  (texname "sub ("))

(deftexfont submember
  (texname "sub\\in"))

(deftexfont submu
  (texname "sub\\mu"))

(deftexfont subnu
  (texname "sub\\nu"))

(deftexfont subnullset
  (texname "sub\\emptyset"))

(deftexfont subomega
  (texname "sub\\omega"))

(deftexfont subomicron
  (texname "sub\\omicron"))

(deftexfont subphi
  (texname "sub\\phi"))

(deftexfont subpi
  (texname "sub\\pi"))

(deftexfont subpsi
  (texname "sub\\psi"))

(deftexfont subrho
  (texname "sub\\rho"))

(deftexfont subrparen
  (texname "sub )"))

(deftexfont subset
  (texname "subseteq"))

(deftexfont subsigma
  (texname "sub\\sigma"))

(deftexfont subtau
  (texname "sub\\tau"))

(deftexfont subtheta
  (texname "sub\\theta"))

(deftexfont subupsilon
  (texname "sub\\upsilon"))

(deftexfont subxi
  (texname "sub\\xi"))

(deftexfont subzeta
  (texname "sub\\zeta"))

(deftexfont sup0
  (texname "sup0"))

(deftexfont sup1
  (texname "sup1"))

(deftexfont sup2
  (texname "sup2"))

(deftexfont sup3
  (texname "sup3"))

(deftexfont sup4
  (texname "sup4"))

(deftexfont sup5
  (texname "sup5"))

(deftexfont sup6
  (texname "sup6"))

(deftexfont sup7
  (texname "sup7"))

(deftexfont sup8
  (texname "sup8"))

(deftexfont sup9
  (texname "sup9"))

(deftexfont supa
  (texname "sup a"))

(deftexfont supb
  (texname "sup b"))

(deftexfont supc
  (texname "sup c"))

(deftexfont supd
  (texname "sup d"))

(deftexfont supe
  (texname "sup e"))

(deftexfont supf
  (texname "sup f"))

(deftexfont supg
  (texname "sup g"))

(deftexfont suph
  (texname "sup h"))

(deftexfont supi
  (texname "sup i"))

(deftexfont supj
  (texname "sup j"))

(deftexfont supk
  (texname "sup k"))

(deftexfont supl
  (texname "sup l"))

(deftexfont suplparen
  (texname "sup ("))

(deftexfont supm
  (texname "sup m"))

(deftexfont supminus
  (texname "sup -"))

(deftexfont supn
  (texname "sup n"))

(deftexfont supo
  (texname "sup o"))

(deftexfont supp
  (texname "sup p"))

(deftexfont supplus
  (texname "sup +"))

(deftexfont supq
  (texname "sup q"))

(deftexfont supr
  (texname "sup r"))

(deftexfont suprparen
  (texname "sup )"))

(deftexfont sups
  (texname "sup s"))

(deftexfont superset
  (texname "supseteq"))

(deftexfont supt
  (texname "sup t"))

(deftexfont supu
  (texname "sup u"))

(deftexfont supv
  (texname "sup v"))

(deftexfont supw
  (texname "sup w"))

(deftexfont supx
  (texname "sup x"))

(deftexfont supy
  (texname "sup y"))

(deftexfont supz
  (texname "sup z"))

(deftexfont supset
  (texname "supset"))

(deftexfont tau
  (texname "tau"))

(deftexfont tensor
  (texname "otimes"))

(deftexfont theta
  (texname "theta"))

(deftexfont times
  (texname "times"))

(deftexfont truth
  (texname "top")) 

(deftexfont turnstile
  (texname "vdash"))

(deftexfont union 
  (texname "cup"))

(deftexfont upsilon
  (texname "upsilon"))

(deftexfont valid
  (texname "models"))

(deftexfont xi
  (texname "xi"))

(deftexfont zero
  (texname "lzero"))

(deftexfont zeta
  (texname "zeta"))

(deftexfont <=
  (texname "leq"))

(in-package :ML)

(core::deftexfont %
  (texname "funimage"))
