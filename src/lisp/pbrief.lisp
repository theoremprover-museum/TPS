;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :core)

(deffile pbrief
  (part-of otlnl)
  (extension clisp)
  (mhelp "Defines the commands PBRIEF, EXPLAIN, BUILD-PROOF-HIERARCHY and PRINT-PROOF-STRUCTURE"))

(context otl-printing)

; This file contains the code  
; for pbrief, explain, print-proof-structure, and 
; build-proof-hierarchy and the relevant flags.

; New proof properties:
;   hierarchy-builtp [same type as nextplan-no]

; New line properties:
;   hierarchy-builtp [boolean]
;   lineardependency [line/NIL]
;   printedp [boolean]

; New user flags:
;   print-combined-uis [boolean]
;   print-combined-ugens [boolean]
;   print-combined-egens [boolean]
;   print-until-ui-or-egen [boolean]

; This code is takes a proof outline and builds information
; corresponding to a hierarchy (a poset) of "subproofs".  First, there is
; an obvious partial ordering on the lines of a proof.  Namely, the
; transitive closure of the "justification" relation--that is, if a line
; j is used to justify a line l, then j <* l.  In fact, the "line number"
; ordering is just a linear completion of this partial ordering.
; The hierarchy built here is not a hierarchy on lines of the proof, but
; on sequences of lines--what I will call "linear chains of inferences".
; These "linear chains" will provide the "subproofs" which are to be ordered.

; We can define a "linear chain" by first defining an ordering <0 on lines.
; Consider lines
; l1) H1,m |- a1
; l2) H2,m :- a2
; where line l1 precedes l2, H1 is a subset of H2, and m is the most recent
; hypothesis of both lines (i.e., m is larger than the lines in H2 [and H1]
; with respect to the line number ordering).  
; Intuitively, we say l1 <0 l2 if the proof provides a proof of
; H2,l1 |- a2.  Formally, we could define this recursively.  (In fact,
; this is what the functions which set the property "lineardependency"
; of lines provide such a definition--a tricky bit involves dealing
; with universal generalization.)
; For lines without hypotheses, the definition above does not work.
; These "top level" lines are handled a bit differently.  Essentially,
; the assertions of these "top level" lines act as the most recent hypothesis,
; and we have l1 <0 l2 if the proof of l2 provides a corresponding proof
; of l1 |- l2.  (For example, if l2 is justified from l1 by rewriting,
; then we would have l1 <0 l2.  On the other hand, if l2 is justified
; from l1 by UGen, then we would NOT have l1 <0 l2, since l1 |- l2
; does not follow from l1 |- l1 by UGen.)
; Once we have defined <0 (by setting the "lineardependency" property),
; each line ln is associated with a unique "linear chain of inferences" 
; l1 <0 l2 <0 . . . <0 ln.  This is the longest chain so that the
; proof would justify a conversion from the lines
; li) Hi,m |- ai  
; (where H1 < H2 < . . . < Hn (subset relation), and m is the most recent 
; hypothesis of each line li) to lines
; l1') H1,m |- a1
; li+1') Hi+1,li |- ai+1.
; Intuitively, the Hn provides the "context" of the argument,
; and the linear chain chases the consequences of the hypothesis m
; to the conclusion ln.  Actually, in a complete proof the first
; line of any linear chain (except those at the top level) must 
; be the line which introduced the hypothesis m.

; Once we have such linear chains, we can order them using the justification
; ordering on lines.  That is, if line jm is used to justify line ln, and
; ln is the end of the linear chain l1 <0 l2 <0 . . . <0 ln, and jm is not
; the line ln-1, then the linear chain j1 <0 j2 <0 . . . <0 jm
; ending with jm is one step lower in the hierarchy.  If we denote this
; ordering on linear chains by <1, then we have 
; [j1 <0 . . . <0 jm] <1 [l1 <0 . . . <0 ln].

; Once this hierarchical information is built into the proof outline,
; pbrief, explain, and print-proof-structure, use the information to
; control which lines are printed.  Essentially, the lines included (if the
; depth is to be d) are those in the linear chain ending with the line 
; indicated (or, for pbrief, the last line of the proof outline), and those 
; lines necessary to explain the lines justifying those lines to depth d-1.

(defflag print-combined-uis
  (flagtype boolean)
  (default t)
  (subjects printing)
  (mhelp "When set to t, the commands PBRIEF and EXPLAIN will combine
lines which are a sequence of universal instantiations and print a
single line."))

(defflag print-combined-egens
  (flagtype boolean)
  (default t)
  (subjects printing)
  (mhelp "When set to t, the commands PBRIEF and EXPLAIN will combine
lines which are a sequence of existential generalizations and print a
single line."))

(defflag print-combined-ugens
  (flagtype boolean)
  (default t)
  (subjects printing)
  (mhelp "When set to t, the commands PBRIEF and EXPLAIN will combine
lines which are a sequence of universal generalizations and print a
single line."))

(defflag print-until-ui-or-egen
  (flagtype boolean)
  (default nil)
  (subjects printing)
  (mhelp "When set to t, the commands PBRIEF and EXPLAIN will
continue to print beyond the depth specified until a line justified
by UI or Egen is encountered.  The intuition is that these are
the real choice points in the proof.  When set to nil, PBRIEF 
and EXPLAIN print only to the depth specified."))

; my global, used only in this code
(defvar line-with-ui-or-egen-gathered nil)

(defmexpr pbrief
  (argtypes integer+)
  (argnames depth)
  (arghelp "Depth to Show") ; more help
  (defaultfns
    (lambda (depth) 
      (if (eq depth '$)
	  (list 2)
	(list depth))))
  (mhelp "This command prints a proof outline, hiding some lines.
In particular, the command BUILD-PROOF-HIERARCHY builds
dependency information into a proof outline which allows
the proof outline to be viewed as a hierarchy of subproofs (see
help for BUILD-PROOF-HIERARCHY).  The command PBRIEF shows
the lines included in the top levels of this hierarchy (to the 
specified depth). PBRIEF is essentially a call to the command EXPLAIN
with the last line of the proof outline as the LINE argument (see help 
for EXPLAIN).  Some flags which affect the printing include:
PRINT-COMBINED-UIS, PRINT-COMBINED-UGENS, PRINT-COMBINED-EGENS,
and PRINT-UNTIL-UI-OR-EGEN."))

(defmexpr explain
  (argtypes existing-line integer+)
  (argnames line depth)
  (arghelp "Line to Explain" "Depth to Show") ; more help
  (defaultfns
    (lambda (line depth) 
      (cond ((eq line '$)
	     (if (eq depth '$)
		 (list (car (last (proof-lines dproof))) 1)
	       (list (car (last (proof-lines dproof))) depth)))
	    ((eq depth '$)
	     (list line 1))
	    (t (list line depth)))))
  (mhelp "This command explains a line of a proof outline.
In particular, the command BUILD-PROOF-HIERARCHY builds
dependency information into a proof outline which allows
the proof outline to be viewed as a hierarchy of subproofs (see
help for BUILD-PROOF-HIERARCHY).  The command EXPLAIN shows
the lines included in the levels of this hierarchy (to the 
specified depth) starting at the level associated with the 
specified line.  Some flags which affect the printing include:
PRINT-COMBINED-UIS, PRINT-COMBINED-UGENS, PRINT-COMBINED-EGENS,
and PRINT-UNTIL-UI-OR-EGEN."))

(defmexpr build-proof-hierarchy ;more help
  (mhelp "This command builds hierarchical information into the proof outline.
The information includes associations between lines and linear chains
of inferences which trace the consequences of the most recent hypothesis
of a line.  That is, a line 

ln) Hn,m |- an

would be associated with a linear chain of lines l1,...,ln
where m is the line corresponding to the most recent hypothesis and
the proof would justify the modified lines

l1) H1,m |- l1
l2) H2,l1 |- l2
l3) H3,l2 |- l3  
. . .
ln) Hn,ln-1 |- ln

where H1 < H2 < . . . < Hn (subset relation). 

That is, we trace the consequences of the hypothesis m to the
consequence ln.  Such a linear chain is on one level of the hierarchy.
One level down on the hierarchy would be the linear chains associated
with each of the lines used to justify l1,...,ln (except those which
appear in the chain l1,...,ln).  If the proof is complete,
then lines l1 and m will be the same.

Lines without hypotheses are also associated with such
\"linear chains\", following the rule that l1 < l2 if
the proof justifies the inference l1 |- l2.

The resulting hierarchy information is used by PBRIEF, EXPLAIN,
and PRINT-PROOF-STRUCTURE to help users focus on the logical
structure of a proof."))

(defmexpr print-proof-structure 
  (mhelp "This prints the structure of the proof outline.  The
structure is generated by BUILD-PROOF-HIERARCHY.  Linear chains
of line numbers are printed which indicate the logical chains of
inferences.  Each link in a linear chain is indicated by an
arrow (l1)->(l2) where l1 and l2 are line numbers.  If line l2
does not follow in a single step from l1 (i.e., by a single application 
of an inference rules), then PRINT-PROOF-STRUCTURE will also show the
linear chains of inference used to justify (l1)->(l2).  Some lines
(such as those without hypotheses and planned lines) are
exceptions.  These top level lines are sometimes printed alone 
(instead of in arrow notation).  This could be read TRUE->(l) 
to maintain consistent notation, but the notation (l) appears 
more readable in practice."))

; build-proof-hierarchy code
(defun build-proof-hierarchy (&optional (p dproof))
  (proof-line-h (car (last (proof-lines p))))
  (dolist (l (proof-lines p)) ; ensures each line gets built
	  (if (not (get l 'hierarchy-builtp)) ; even in outlines
	      (proof-line-h l)))
  (dolist (l (proof-lines p))
	  (setf (get l 'hierarchy-builtp) nil))
  (setf (get p 'hierarchy-builtp) (get p 'nextplan-no)))

(defun proof-line-h (l)
  (if (get l 'hierarchy-builtp) ; this property is only used as
      nil ; a flag to prevent duplication of effort when building the hierarchy
    (let ((deps (line-dependencies l))
	  (hyps (line-hypotheses l)))
      (mapcar #'proof-line-h deps) ; recursively sets lineardependency
      (setf (get l 'hierarchy-builtp) t)
      (let ((m (most-recent-line hyps)))
	(let ((cdeps (if hyps
			 (get-current-level-deps 
			  m hyps (line-ugenvars l) deps)
		       (get-top-level-deps (line-ugenvars l) deps))))
	  (setf (get l 'lineardependency)
		(find-common-line-ancestor cdeps)))))))

(defun get-current-level-deps (m hyps ugenvars deps)
  (if deps
      (if (current-level-p m hyps ugenvars (car deps))
	  (adjoin (car deps)
		  (get-current-level-deps m hyps ugenvars (cdr deps)))
	(if (member m (line-hypotheses (car deps)))
	    (let ((ugenvars2 (union (line-ugenvars (car deps))
				    ugenvars
				    :test #'equal)))
	      (union
	       (get-current-level-deps
		m hyps ugenvars2
		(line-dependencies (car deps)))
	       (get-current-level-deps
		m hyps ugenvars
		(cdr deps))))
	  (get-current-level-deps
	   m hyps ugenvars (cdr deps))))
    nil))

(defun current-level-p (m hyps ugenvars l)
  (let ((h (line-hypotheses l)))
    (and (member m h) 
	 (subsetp h hyps)
	 (not (intersection ugenvars
			    (free-vars-of (line-assertion l))
			    :test #'equal)))))

(defun get-top-level-deps (ugenvars deps)
  (if deps
      (if (or (line-hypotheses (car deps))
	      (intersection ugenvars
			    (free-vars-of
			     (line-assertion (car deps)))
			    :test #'equal))
	  (union
	   (get-top-level-deps 
	    (union (line-ugenvars (car deps))
		   ugenvars :test #'equal)
	    (line-dependencies (car deps)))
	   (get-top-level-deps
	    ugenvars
	    (cdr deps)))
	(cons (car deps)
	      (get-top-level-deps ugenvars (cdr deps))))
    nil))

; If the line is justified by UGen, then return a list with
; the variable. (This could easily be extended to allow for multiple
; variables universally generalized in one line).  Otherwise,
; return nil.
(defun line-ugenvars (l)
  (if (equal (car (line-justification l)) "UGen")
      (cadr (line-justification l))
    nil))

(defun find-common-line-ancestor (lines)
  (if lines
      (if (cdr lines)
	  (let ((m (most-recent-line lines)))
	    (let ((l (line-lineardependency m)))
	      (if l
		  (find-common-line-ancestor
		   (union (list l) (remove m lines :test #'equal) 
			  :test #'equal))
		nil)))
	(car lines))
    nil))

(defun union-list (l)
  (if l
      (if (cdr l)
	  (union (car l) (union-list (cdr l)) :test #'equal)
	(car l))
    nil))

(defun most-recent-line (lines)
  (if lines
      (let ((m (most-recent-line (cdr lines))))
	(if (or (not m)
		(> (linealias (car lines)) (linealias m)))
	    (car lines)
	  m))
    nil))

; explain/pbrief code
(defun explain (line &optional (depth 1))
  (if (not (equal (get dproof 'hierarchy-builtp)
		  (get dproof 'nextplan-no)))
      (progn
	(build-proof-hierarchy)))
  (setq line-with-ui-or-egen-gathered nil)
  (print-partial-proof 
   (gather-subproof-lines line depth)))

(defun pbrief (&optional (depth 2))
  (if (not (equal (get dproof 'hierarchy-builtp)
		  (get dproof 'nextplan-no)))
      (progn
	(build-proof-hierarchy)))
  (setq line-with-ui-or-egen-gathered nil)
  (explain
   (car (last (proof-lines dproof))) depth))

(defun gather-subproof-lines (l &optional (depth 2))
  (let ((lch (get-linear-chain-of-line l)))
    (if (or (> depth 0) (and print-until-ui-or-egen
			     (not line-with-ui-or-egen-gathered)))
	(include-lines (apply #'append
			      (mapcar 
			       #'(lambda (x)
				   (gather-justification-subproof
				    x lch
				    depth))
			       lch))
		       lch)
      (adjoin (car lch) (last lch)))))

(defun gather-justification-subproof (l lch depth)
  (let ((ll (set-difference (line-dependencies l)
			    lch)))
    (if (or (> depth 0) (and print-until-ui-or-egen
			     (not line-with-ui-or-egen-gathered)))
	(include-lines
	 (apply #'append
		(mapcar #'(lambda (x)
			    (gather-subproof-lines
			     x 
			     (if (equal 
				  (car (line-justification x))
				  "UGen")
				 depth
			       (- depth 1))))
			ll))
	 nil)
      nil)))

(defun include-lines (ll ll2)
  (if ll
      (include-line (car ll)
		    (include-lines (cdr ll) ll2))
    ll2))

(defun include-line (l ll)
  (if ll
      (if (equal l (car ll))
	  ll
	(if (> (linealias l) (linealias (car ll)))
	    (cons (car ll) (include-line l (cdr ll)))
	  (progn
	    (if (member (car (line-justification l))
			'("UI" "EGen") :test #'equal)
		(setq line-with-ui-or-egen-gathered t))
	    (cons l ll))))
    (progn
      (if (member (car (line-justification l))
		  '("UI" "EGen") :test #'equal)
	  (setq line-with-ui-or-egen-gathered t))
      (list l))))

; returns linear chain (list of lines)
(defun get-linear-chain-of-line (l &optional source)
  (let ((l2 (line-lineardependency l)))
    (if l2
	(if (equal l2 source)
	    (cons l2 (list l))
	  (append (get-linear-chain-of-line l2) (list l)))
      (list l))))

; access functions (like structure access functions)
(defun line-lineardependency (l)
  (get l 'lineardependency))

(defun line-dependencies (l)
 (if (member l (line-hypotheses l))
     nil
   (caddr (line-justification l))))

(defun line-dependencies-with-linear (l)
  (let ((ld (line-lineardependency l)))
    (if ld
	(cons ld (remove ld (line-dependencies l)))
      (line-dependencies l))))

; printing code
(defun print-proof-structure ()
  (if (not (equal (get dproof 'hierarchy-builtp)
		  (get dproof 'nextplan-no)))
      (progn
	(build-proof-hierarchy)))
; Even though this is a dolist, if we have a complete
; proof with no lines which are not used to justify
; the conclusion, then only the first call
; (corresponding to (car (last (proof-lines dproof))))
; will have any effect.  I did it this more general
; way to handle proof outlines.
  (dolist (l (reverse (proof-lines dproof)))
	  (if (not (get l 'printedp))
	      (print-proof-structure-line l)))
  (dolist (l (proof-lines dproof))
	  (setf (get l 'printedp) nil)))

(defun print-proof-structure-line (l)
  (let ((lch (get-linear-chain-of-line l)))
    (print-linear-chain lch)
    (print-proof-structure-linear-chain lch)))

(defun print-linear-chain (lch)
  (if lch
      (do ((lch2 lch (cdr lch2)))
	  ((not (cdr lch2)) (format t "(~d)~%" (linealias (car lch2))))
	  (format t "(~d)->" (linealias (car lch2))))))

(defun print-justification-structure (source dest)
  (if (not (get dest 'printedp))
      (progn
	(setf (get dest 'printedp) t)
	(let ((deps (remove source 
			    (line-dependencies dest)
			    :test #'equal)))
	  (if (and (not (member dest (line-hypotheses dest) :test #'equal))
		   deps)
	      (let ((lchl (mapcar #'(lambda (x)
				      (get-linear-chain-of-line x source))
				  deps)))
		(if source
		    (format t "(~d)->(~d) using: " 
			    (linealias source) (linealias dest))
		  (format t "(~d) using: " (linealias dest)))
		(do ((lchl2 lchl (cdr lchl2)))
		    ((not (cdr lchl2))
		     (print-linear-chain
		      (car lchl2)))
		    (print-linear-chain (car lchl2))
		    (format t "        & "))
		(dolist (lch lchl)
			(print-proof-structure-linear-chain lch))))))))

(defun print-proof-structure-linear-chain (lch)
  (print-justification-structure nil (car lch))
  (do ((lch2 lch (cdr lch2)))
      ((not (cdr lch2)) nil)
      (print-justification-structure (car lch2) (cadr lch2))))

; (prtlines <lines>) is TPS's printing
; This is mine (which used TPS's prtline) which changes
; justification for lines depending on hidden lines.
(defun print-partial-proof (lines)
  (do ((ll lines (cdr ll)))
      ((not ll))
      (let ((deps (line-dependencies (car ll))))
	(if (subsetp deps lines)
	    (let ((just (line-justification (car ll))))
	      (cond ((and (cdr ll)
			  (or (and print-combined-uis
				   (equal (car just) "UI"))
			      (and print-combined-ugens
				   (equal (car just) "UGen"))
			      (and print-combined-egens
				   (equal (car just) "EGen"))))
		     (let (ll3)
		       (do ((ll2 ll (cdr ll2)))
			   ((or
			     (not (equal (car just)
					 (car (line-justification (car ll2)))))
			     (and ll3
				  (not (= (- (linealias (car ll2)) 1)
					  (linealias 
					   (caaddr (line-justification (car ll2))))))))
			    (prtline 
			     (combine-lines ll3)))
			   (if (not (equal (car ll2) (car ll)))
			       (setq ll (cdr ll)))
			   (setq ll3 (append ll3 (list (car ll2)))))))
		    (t
		     (prtline (car ll)))))
	  (let ((g (gensym))) 
	    (setf (get g 'hypotheses) (get (car ll) 'hypotheses))
	    (setf (get g 'assertion) (get (car ll) 'assertion))
	    (setf (get g 'linenumber) (get (car ll) 'linenumber))
	    (setf (get g 'justification)
		  (list "Subproof"
			nil ; Could I put lin dep here?
			(get-dependencies-in-lines
			 (line-dependencies-with-linear (car ll))
			 lines)))
	    (prtline g))))))

; combines lines with the same justification
(defun combine-lines (ll)
  (if ll
      (if (cdr ll)
	  (let ((g (gensym))
		(l (car (last ll))))
	    (setf (get g 'linenumber)
		  (linealias l))
	    (setf (get g 'hypotheses)
		  (union-list
		   (mapcar #'(lambda (x)
			       (line-hypotheses x))
			   ll)))
	    (setf (get g 'assertion)
		  (line-assertion l))
	    (setf (get g 'justification)
		  (list (car (line-justification l))
			(mapcar #'(lambda (x)
				    (caadr (line-justification x)))
				(if (equal (car (line-justification l))
					   "UI")
				    ll
				  (reverse ll)))
			(caddr (line-justification (car ll)))))
	    g)
	(car ll))
    nil))

(defun combine-strings (sl &optional (sep " "))
  (if sl
      (if (cdr sl)
	  (format nil "~d~d~d" (car sl) sep (combine-strings (cdr sl) sep))
	(car sl))
    ""))

(defun get-dependencies-in-lines (ll lines)
  (if ll
      (if (member (car ll) lines)
	  (union (list (car ll))
		 (get-dependencies-in-lines
		  (cdr ll) lines)
		 :test #'equal)
	(get-dependencies-in-lines
	 (line-dependencies-with-linear (car ll))
	 lines)) 
    nil))

