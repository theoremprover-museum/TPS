;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of EXPANSION-TREE)

(deffile etrees-flags
  (part-of expansion-tree)
  (extension lisp)
  (mhelp "Macros and flags for expansion trees."))

(defcontext expansion-trees
  (short-id "Expansion Trees")
  (mhelp "TPS objects dealing with expansion trees."))

(context expansion-trees)

(defflag print-nodenames
  (flagtype boolean)
  (default t)
  (subjects printing etrees)
  (mhelp "T will print the names of expansion and selection nodes,
NIL will print either the deep or shallow formula of the node.
(see the flag PRINT-DEEP)."))

(defflag print-deep
  (flagtype boolean)
  (default t)
  (subjects printing etrees)
  (mhelp "T will print the deep formula of an expansion or selection node,
NIL will print the shallow formula, both only if PRINT-NODENAMES is NIL."))


(defflag min-quantifier-scope
  (flagtype boolean)
  (default nil)
  (subjects etrees mating-search ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 ms98-1 transmit)
  (mhelp "When this flag is T, the scope of quantifiers is minimized before
    starting expansion proofs.
If an eproof is found with this flag set to T, during the translation
of the eproof to an ND proof RULEQ is called to fill the gap between
the theorem as originally stated and its min-quantifier-scope version."))

;;;The following flag should be eliminated after we can apply substitution
;;;to the jform we are doing proof searching.

;;;Once an abreviation contains FASLEHOOD in its definition,
;;;it should be added into the following list.
(defvar truthvalues-hack-updatelist '(NULLSET))

(defflag truthvalues-hack
  (flagtype boolean)
  (default nil)
  (change-fn (lambda (flag value pvalue) 
                     (declare (ignore flag))
                (when (neq value pvalue)    
                      (flet ((ftemp (x) 
                                 (if value 
                                     (setf (get x 'defn) (falsehood-elim (get x 'defn)))
                                     (setf (get x 'defn) (not-truth-elim (get x 'defn))))))
                          (dolist (x truthvalues-hack-updatelist) (ftemp x))))))
  (subjects etrees mating-search transmit)
  (mhelp "When this flag is T, leaves of truthvalues will not deepened into
an empty disjunction or an empty conjunction. this allows us to deal with 
truthvalues in formulas, especially, higher-order formulas. In order to deal 
with truthvalues in definitions, such as NULLSET, the definitions containing 
falsehood should be rewritten. Please put new definitions containing falsehood
into truthvalues-hack-updatelist so that they can be rewritten appropriately."))

(defflag initial-bktrack-limit
  (flagtype integer+-or-infinity)
  (default INFINITY)
  (subjects mating-search ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 transmit)
  (relevancy-preconditions
   (ms-split (and ms-split (member default-ms '(ms91-6 ms89 ms88))))
   (default-ms (and ms-split (member default-ms '(ms91-6 ms89 ms88)))))
  (irrelevancy-preconditions
   (ms-split (not ms-split))
   (default-ms (not (member default-ms '(ms91-6 ms89 ms88)))))
  (mhelp "Initial backtrack limit.  If a mating exceeds this limit, a new
    mating will be started, and the limit incremented. If the value of the 
    flag is set to INFINITY, then this will never happen."))

;;Following defines the structure of an expansion proof without
;;the associated mating

(defstruct (eproof (:print-function print-eproof))
  (etree nil)            ;; The tree, or actually its root
  (jform nil)
  (all-banned nil) ;; A list of exp vars and a list of selected vars whose sel node occurs beneath the exp term -- needed to check acyclicity condition
  (inst-exp-vars-params nil) ;; An alist of exp vars that occur instantiated in the proof and the sel vars that occur in the term -- needed to check acyclicity condition
  (dissolve nil)			; a list of connections to be dissolved from the jform - cebrown 10/2/00
  (allow-nonleaf-conns nil)		; a list of symbols, names of (nonleaf) nodes to be included in the jform - cebrown 10/2/00
  (free-vars-in-etree nil) ;; An alist of free vars and the nodes in which they arise
  (skolem-constants nil)  ;;  An alist of constants and their required arities
  (substitution-list nil) ;; An alist of variables and the substitutions made
                          ;; for them
  (leaf-list nil)         ;; list of current leaves in the tree
  (skolem-method nil)     ;; NIL if not skolemizing, otherwise sk1, sk2, etc.
  (max-cgraph-counter 0)
  (bktrack-limit initial-bktrack-limit)
  connections-array
  incomp-clists-wrt-etree 
  (mating-list nil)
  incomp-clists
  cgraph
  (skolem-node-list nil)
  (stats (make-stats))
  max-incomp-clists-wrt-etree 
  (symmetry nil)
  (merged nil)         ;; true if etree has been merged
  ;; a hashtable of nodes in the etree and their statuses
  ;; if a node doesn't appear here, it's not part
  ;; of the etree of this proof
  (statuses (make-hash-table :test #'eq))
  (lemmas nil) ;; cebrown 10/14/01 - tree indicating lemmas in the expansion tree
					; ((nil) nil) means the tree has two lemmas
					; and the first lemma has one lemma.
					; A lemma for Q is of the form
					; (A implies A) implies Q
  ; identifies the particular eproof
  (name (intern (create-namestring eproof-name)
		(find-package "CL-USER"))))

(defvar *print-eproof-verbose* nil)

(defun print-eproof (eproof *standard-output* level)
  (declare (ignore level))
  (when eproof
;  (terpri)
  (princ "Eproof:") (princ (symbol-name (eproof-name eproof))) (princ " ")
  (when *print-eproof-verbose*
    (let ((print-nodenames t))
      (terpri) (princ "Etree: ") (princ (eproof-etree eproof))
      (terpri) (princ "Skolem Method: ") (princ (eproof-skolem-method eproof))
      (when (eproof-free-vars-in-etree eproof)
	    (terpri) (princ "Free Variables:") (terpri) (princ "     Node     Var")
	    (dolist (pair (eproof-free-vars-in-etree eproof))
		    (terpri) (princ "     ") 
		    (princ (cdr pair)) (princ "      ") (printwffhere (car pair))))
      (when (eproof-skolem-constants eproof)
	    (terpri) (princ "Skolem Constants:") (terpri) (princ "Skolem   Arity")
	    (dolist (pair (eproof-skolem-constants eproof))
		    (terpri) (princ "     ") (printwffhere (car pair)) (princ "        ")
		    (princ (cdr pair))))
      (when (eproof-substitution-list eproof)
	    (terpri) (princ "Substitutions") (terpri) (princ "     Variable Term")
	    (dolist (exp-var (eproof-substitution-list eproof))
		    (terpri) (princ "     ") (printwffhere (exp-var-var exp-var)) (princ "        ")
		    (printwffhere (exp-var-subst exp-var))))
      (terpri) (princ "Leaves:   ") (prinlc (eproof-leaf-list eproof))
      (if (eproof-mating-list eproof)
	  (progn 
	    (setf (eproof-mating-list eproof)
		  (delete-if-not #'mating-p (eproof-mating-list eproof)))
	    (terpri) (princ "Matings (active mating has *):")
	    (dolist (mating (eproof-mating-list eproof))
	      (terpri) (princ (symbol-name (mating-name mating)))
	      (if (eq mating active-mating) (msg "*"))))
	(progn (terpri) (princ "No matings in progress.")))))))

;;; The master eproof, which contains the master copy of the expansion tree

(defvar master-eproof nil)

(defflag duplication-strategy
  (flagtype symbol)
  (default dup-outer)
  (change-fn (lambda (flag value pvalue) 
                     (unless (memq value '(dup-outer dup-all))
                         (set flag pvalue)
                         (msg "Sorry, the value of the flag must be one of the followings:" T
                              "1)DUP-OUTER;" T
                              "2)DUP-ALL." T))))
  (subjects mating-search ms88 transmit)
  (mhelp "The name of a duplication strategy.  Currently, either
DUP-ALL or DUP-OUTER. Only applies to MS88."))

(defflag duplication-strategy-pfd
  (flagtype symbol)
  (default dup-inner)
  (change-fn (lambda (flag value pvalue) 
                     (unless (memq value '(dup-inner dup-outer))
                         (set flag pvalue)
                         (msg "Sorry, the value of the flag must be one of the followings:" T
                              "1)DUP-INNER;" T
                              "2)DUP-OUTER." T))))
  (subjects mating-search ms90-3 ms90-9 ms91-7 ms92-9 ms93-1 transmit)
  (mhelp "The name of a duplication strategy for path-focused procedures.
It may have either of two values: DUP-INNER and DUP-OUTER. DUP-INNER
means inner quantifiers get duplicated before outer ones, while DUP-OUTER
means vice versa."))

(definfo dup-all
  (mhelp "A setting for the flag DUPLICATION-STRATEGY.
When duplication of quantifiers is needed (in non-path-focused search),
duplicate all the quantifiers."))

(definfo dup-inner
  (mhelp "A setting for the flag DUPLICATION-STRATEGY-PFD.
When duplication of quantifiers is needed (in path-focused search),
duplicate the innermost quantifier first."))

(definfo dup-outer
  (mhelp "A setting for the flags DUPLICATION-STRATEGY-PFD 
and DUPLICATION-STRATEGY.
When duplication of quantifiers is needed in path-focused search,
duplicate the outermost quantifier first. In other searches,
duplicate the outermost quantifiers only."))

(defflag skolem-default
  (flagtype symbol)
  (default sk1)
  (subjects mating-search ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 ms98-1 transmit)
  (mhelp "Default method for skolemizing, in which wffs of the form
EXISTS y . M are replaced by M(g(...)). There are three possible ways to
do this:
SK1 is the original method due to Skolem, where the Skolem constants
 g take as arguments all the x such that FORALL x occurs in the wff 
 and EXISTS y . M is in its scope.
SK3 is the method in which the arguments of g are the free variables
 of EXISTS y . M.
NIL means don't Skolemize at all; use selection nodes instead."))

(definfo sk1
  (mhelp "A setting for the flag SKOLEM-DEFAULT.
SK1 is the original method due to Skolem, where wffs of the form
EXISTS y . M are replaced by M(g(...)), and the Skolem constants
g take as arguments all the x such that FORALL x occurs in the wff 
and EXISTS y . M is in its scope."))

(definfo sk3
  (mhelp "A setting for the flag SKOLEM-DEFAULT.
SK3 is a variant of the original method due to Skolem, where wffs 
of the form EXISTS y . M are replaced by M(g(...)), and the Skolem 
constants g take as arguments all the free variables
of EXISTS y . M.  When SK3 is used to find an expansion proof,
the translation to a natural deduction proof may fail, since
the appropriately general rules of inference are not implemented
in TPS at present."))

(definfo nil
  (mhelp "A setting for the flag SKOLEM-DEFAULT.
Instead of skolemizing a wff, use selection nodes and constrain the
unification tree, as explained in Miller's thesis."))

(defflag rewrite-defns
  (flagtype rewrite-defns-list)
  (default '(eager))
  (subjects mating-search important ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 ms98-1 transmit)
  (mhelp "A list whose first element is one of NONE, EAGER, LAZY1 and DUAL,
and whose other (optional) elements are lists whose first element is
one of these four options and whose other elements are the names of 
definitions.
The first element is the default behaviour for rewriting definitions,
and the other lists are lists of exceptions to this default, with a
different behaviour specified.
NONE:  do not rewrite this definition at all.
EAGER: rewrite all of these definitions, in one big step, as soon 
       as possible.
LAZY1: rewrite these, one step at a time, when there are no more
       EAGER rewrites to do.
DUAL: as LAZY1, but rewrite these abbreviations A to a conjunction of
       A and A, and then deepen only one of these conjuncts. (e.g.
       TRANSITIVE p becomes 
       TRANSITIVE p AND FORALL x y z . [pxy AND pyz] IMPLIES pxz
LAZY2: synonym for DUAL.

For example: the value
(EAGER)
would be interpreted as \"Rewrite every definition in one step.\"

(DUAL (EAGER TRANSITIVE) (NONE INJECTIVE SURJECTIVE))
would be interpreted as \"Rewrite TRANSITIVE whenever it appears.
Don't ever rewrite INJECTIVE or SURJECTIVE. Rewrite every other 
definition in the DUAL way.\""))

(definfo eager
  (mhelp "A flag setting for REWRITE-DEFNS.
When constructing an etree, rewrite all definitions (or a specified
list of definitions), in one big step, as soon as possible."))

(definfo none
  (mhelp "A flag setting for REWRITE-DEFNS.
When constructing an etree, do not rewrite the specified definitions.

A flag setting for REWRITE-EQUALITIES.
When constructing an etree, do not rewrite equalities.

A flag setting for DEFAULT-EXPAND.
Do not use option trees or option sets."))

(definfo lazy1
  (mhelp "A flag setting for REWRITE-DEFNS.
When constructing an etree, rewrite all definitions (or a specified
list of definitions), one step at a time, once there are no more
EAGER rewrites to do."))

(definfo lazy2
  (mhelp "A flag setting for REWRITE-DEFNS.
When constructing an etree, rewrite all definitions (or a specified
list of definitions), one step at a time, once there are no more
EAGER rewrites to do. Furthermore, rewrite each definition to a
conjunction (or disjunction) of a leaf containing that definition
and an etree containing a rewrite of the definition.

See Selectively Instantiating Definitions, CADE-15.

A flag setting for REWRITE-EQUALITIES.
As above for definitions, but with equalities."))

(definfo dual
  (mhelp "A flag setting for REWRITE-DEFNS.
When constructing an etree, rewrite all definitions (or a specified
list of definitions), one step at a time, once there are no more
EAGER rewrites to do. Furthermore, rewrite each definition to a
conjunction (or disjunction) of a leaf containing that definition
and an etree containing a rewrite of the definition.

See Selectively Instantiating Definitions, CADE-15.

A flag setting for REWRITE-EQUALITIES.
As above for definitions, but with equalities."))

(defmode naive
  (flag-settings
   (skolem-default sk1)
   (rewrite-defns '(lazy1))
   (rewrite-equalities 'auto::leibniz)
   (remove-leibniz t)
   (min-quantifier-scope nil)
   (use-rulep nil)
   (use-symsimp nil))
  (mhelp "Sets flags so all definitions and equalities will be rewritten,
skolemizing will be done using SK1, but equalities will be rewritten
using the Leibniz definition."))

(context mating-search)

(defflag interrupt-enable
  (flagtype boolean)
  (default t)
  (subjects mating-search ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1)
  (mhelp "When true, allows user to interrupt mating search by typing
a <RETURN>; otherwise mating search will continue until it succeeds
or is aborted by a CTRL-G.  You may want to set this flag to nil
if you are going to have input commands (examples to run, etc.) read
in from a file."))

;;; Added 22MAY90 DAN

(defflag query-user
  (flagtype querytype)
  (default nil)
  (subjects mating-search ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 ms03-7 transmit)
  (mhelp "Has the following effects according to its value:
T :  User will be queried by the mating search process as to whether 
  a duplication of variables should occur, unification
  depth should be increased, etc.
NIL :  The mating search process will take some action that makes sense.
QUERY-JFORMS : The mating search process will stop after printing each 
vpform and ask whether to search on this vpform or to generate another.
(Note: in MS90-3, this is pointless, since the vpform never changes.)
SHOW-JFORMS : Like QUERY-JFORMS, but automatically answers no to each 
question (and hence never actually proceeds with a search).
QUERY-SLISTS : In the TEST top level, stops after each setting of the
flags and asks whether to search with those settings."))

(definfo query-jforms
  (mhelp "A flag setting for QUERY-USER.
The mating search process will stop after printing each 
vpform and ask whether to search on this vpform or to generate another.
(Note: in MS90-3, this is pointless, since the vpform never changes.)"))

(definfo show-jforms
  (mhelp "A flag setting for QUERY-USER.
Like QUERY-JFORMS, but automatically answers no to each 
question (and hence never actually proceeds with a search)."))

(definfo query-slists
  (mhelp "A flag setting for QUERY-USER.
In the TEST top level, stops after each setting of the
flags and asks whether to search with those settings."))

(defflag mating-verbose
  (flagtype verbose)
  (default med)
  (change-fn (lambda (flag value pvalue) 
                     (declare (ignore pvalue))
                     (if (eq value T) (set flag 'MAX)
                         (if (eq value NIL) (set flag 'SILENT))))) 
  (subjects mating-search transmit)
  (mhelp "Should be one of SILENT, MIN, MED, or MAX.  Determines the amount of
information given about the current mating process."))

(defflag rec-ms-file
  (flagtype boolean)
  (default nil)
  (subjects events)
  (mhelp "If true, mating search events are recorded in file named by flag
rec-ms-filename. This only works for npfd procedures MS88, MS89 and MS91-6."))

(defflag rec-ms-filename
  (flagtype filespec)
  (default "mating.rec")
  (subjects events)
  (mhelp "Name of file in which mating search events are recorded. (See
REC-MS-FILE.)"))

(defflag new-mating-after-dup
  (flagtype boolean)
  (default nil)
  (subjects mating-search ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 transmit)
  (mhelp "This flag affects the way a complete mating is constructed after
    duplication.  If nil, mating search attempts to extend only those matings
    which were inextensible earlier. Otherwise, it starts constructing new
    matings."))

(context expansion-trees)

(defflag leaf-name
  (flagtype symbol)
  (default leaf)
  (subjects etrees)
  (mhelp "Prefix for labels associated with leaf nodes."))

(defflag edisj-name
  (flagtype symbol)
  (default disj)
  (subjects etrees)
  (mhelp "Prefix for labels associated with disjunction nodes."))

(defflag econj-name
  (flagtype symbol)
  (default conj)
  (subjects etrees)
  (mhelp "Prefix for labels associated with conjunction nodes."))

(defflag imp-name
  (flagtype symbol)
  (default imp)
  (subjects etrees)
  (mhelp "Prefix for labels associated with implication nodes."))

(defflag empty-dup-info-name
  (flagtype symbol)
  (default emp)
  (subjects etrees)
  (mhelp "Prefix for labels associated with empty-dup-info nodes."))

(defflag neg-name
  (flagtype symbol)
  (default neg)
  (subjects etrees)
  (mhelp "Prefix for labels associated with negation nodes."))

(defflag true-name
  (flagtype symbol)
  (default true)
  (subjects etrees)
  (mhelp "Prefix for labels associated with TRUTH nodes."))

(defflag false-name
  (flagtype symbol)
  (default false)
  (subjects etrees)
  (mhelp "Prefix for labels associated with FALSEHOOD nodes."))

(defflag rewrite-name
  (flagtype symbol)
  (default rew)
  (subjects etrees)
  (mhelp "Prefix for labels associated with rewrite nodes."))

(defflag expansion-name
  (flagtype symbol)
  (default exp)
  (subjects etrees)
  (mhelp "Prefix for labels associated with expansion nodes."))

(defflag selection-name
  (flagtype symbol)
  (default sel)
  (subjects etrees)
  (mhelp "Prefix for labels associated with selection nodes (in a 
non-skolem etree)."))

(defflag skolem-selection-name
  (flagtype symbol)
  (default skol)
  (subjects etrees)
  (mhelp "Prefix for labels associated with selection nodes (in a 
skolem etree)."))

(defflag eproof-name
  (flagtype symbol)
  (default epr)
  (subjects etrees)
  (mhelp "Prefix for names of expansion proofs."))

(defflag mating-name
  (flagtype symbol)
  (default mat)
  (subjects etrees)
  (mhelp "Prefix for names of matings."))

(context ms88)

(defflag first-order-mode-ms
  (flagtype boolean)
  (default nil)
  (subjects mating-search ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 ms98-1 transmit)
  (mhelp "If T first-order unification is called during mating search, else
higher-order unification is used. TPS changes the value of this flag
to T when it is called by  DIY to work on a first-order problem,
but not when it is called from MATE."))

(context ms89)

(defflag rank-eproof-fn
  (flagtype symbol)
  (subjects mating-search ms89 ms90-9 ms93-1 transmit)
  (default num-vpaths-ranking)
  (mhelp "The name of a function which should take as its single
argument an incomplete expansion proof, and return a nonnegative
integer ranking the proof's likelihood of success, with 0 meaning no
success (so don't try), and, otherwise, the better the likelihood, the
lower the returned value.
The only currently defined value for this flag is NUM-VPATHS-RANKING."))

(definfo num-vpaths-ranking
  (mhelp "A flag setting for RANK-EPROOF-FN.
Returns the number of vpaths in an expansion proof."))
