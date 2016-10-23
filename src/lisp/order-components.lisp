;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package "AUTO")
(part-of ms88)

(deffile order-components
  (part-of ms88)
  (extension lisp)
  (mhelp "The file order-components is used to rearrange the current 
jform with the help of some heuristics."))

(eval-when (load compile eval)
(defcategory ordercomponents
  (define defordercom)
  (properties
   (init-jform-mspath singlefn)
   (tree-sorting singlefn)
   (sort-ms90-3-jform singlefn)
   (mhelp single))
  (global-list global-ordercomlist)
  (mhelp-line "argument for order-components")
  (mhelp-fn princ-mhelp))
)

(defordercom common
  (init-jform-mspath common-init-jform-mspath)
  (tree-sorting common-tree-sorting)
  (sort-ms90-3-jform common-sort-ms90-3-jform)
  (mhelp "COMMON is the same as NIL. If the flag ORDER-COMPONENTS is set to COMMON
then the jform of the current eproof will not be modified by the mating search."))

(defordercom nil
  (init-jform-mspath common-init-jform-mspath)
  (tree-sorting common-tree-sorting)
  (sort-ms90-3-jform common-sort-ms90-3-jform)
  (mhelp "NIL is the same as COMMON. If the flag ORDER-COMPONENTS is set to NIL
then the jform of the current eproof will not be modified by the mating search."))

(defordercom pathnum
  (init-jform-mspath pathnum-init-jform-mspath)
  (tree-sorting pathnum-tree-sorting)
  (sort-ms90-3-jform pathnum-sort-ms90-3-jform)
  (mhelp "PATHNUM is the same as T. If the flag ORDER-COMPONENTS is set to PATHNUM
then the components of a jform node will be rearranged in order of the number 
of paths which lie below them (go through them). In ms90-*, this will sort the 
top-level conjuncts into decreasing order (based on the number of paths through 
them)."))

(defordercom t
  (init-jform-mspath pathnum-init-jform-mspath)
  (tree-sorting pathnum-tree-sorting)
  (sort-ms90-3-jform pathnum-sort-ms90-3-jform)
  (mhelp "T is the same as PATHNUM. If the flag ORDER-COMPONENTS is set to T
then the components of a jform node will be rearranged in order of the number 
of paths which lie below them (go through them)."))

(defordercom t-reversed
  (init-jform-mspath reversed-pathnum-init-jform-mspath)
  (tree-sorting reversed-pathnum-tree-sorting)
  (sort-ms90-3-jform reversed-pathnum-sort-ms90-3-jform)
  (mhelp "T-REVERSED is the same as PATHNUM-REVERSED. If the flag 
ORDER-COMPONENTS is set to T-REVERSED then the components of a jform node will 
be rearranged in reverse order of the number of paths which lie below them 
(go through them)."))

(defordercom pathnum-reversed
  (init-jform-mspath reversed-pathnum-init-jform-mspath)
  (tree-sorting reversed-pathnum-tree-sorting)
  (sort-ms90-3-jform reversed-pathnum-sort-ms90-3-jform)
  (mhelp "PATHNUM-REVERSED is the same as T-REVERSED. If the flag 
ORDER-COMPONENTS is set to T-REVERSED then the components of a jform node will 
be rearranged in reverse order of the number of paths which lie below them 
(go through them)."))

(defordercom reverse
   (init-jform-mspath reverse-init-jform-mspath)
   (tree-sorting common-tree-sorting)
   (sort-ms90-3-jform reverse-sort-ms90-3-jform)
   (mhelp "If the flag ORDER-COMPONENTS is set to REVERSE, then 
the order of the components in the jform of the current eproof will
be reversed before beginning the mating search."))

(defordercom prefer-rigid1
   (init-jform-mspath rigid1-init-jform-mspath)
   (tree-sorting rigid1-tree-sorting)
   (sort-ms90-3-jform rigid1-sort-ms90-3-jform)
   (mhelp "If the flag ORDER-COMPONENTS is set to PREFER-RIGID1, then 
the order of the components in the jform of the current eproof will be sorted 
in terms of the number of rigid literals in a jform before beginning the 
mating search."))

(defflag order-components
  (flagtype ordercom)
  (default T)
  (subjects mating-search jforms important ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 transmit)
  (relevancy-preconditions
   (default-ms (or (member default-ms '(MS88 MS89)) 
		   (and ms-split (eq default-ms 'MS91-6))))
   (ms-split (or (member default-ms '(MS88 MS89)) 
		 (and ms-split (eq default-ms 'MS91-6)))))
  (irrelevancy-preconditions
   (default-ms (not (or (member default-ms '(MS88 MS89)) 
			(and ms-split (eq default-ms 'MS91-6)))))
   (ms-split (and (eq default-ms 'MS91-6) (not ms-split))))
  (mhelp "When T or PATHNUM, the components of a jform node will be
rearranged in order of the number of paths which lie below them (go 
through them).
When T-REVERSED or PATHNUM-REVERSED, the components of a jform node will be 
rearranged in reverse order of the number of paths which lie below them (go 
through them).
When NIL or COMMON, then the jform of the current eproof will not be modified 
by the mating search;
When REVERSE, the order of the components in the jform of current eproof will 
be reversed;
When PREFER-RIGID2, the order of the components in the jform of the current 
eproof will be sorted in terms of the number of rigid literals in a jform 
before beginning the mating search.
When PREFER-RIGID3, the components in the jform of the current eproof will 
be sorted as for PREFER-RIGID2, but with preference given to literals that 
arise from DUAL rewriting.

(PREFER-RIGID1 is still available; it is an obsolete version of PREFER-RIGID2.)"))

;;;--------------------------------------------------

(defun common-init-jform-mspath (jform) jform)

(defun common-tree-sorting (jforms) jforms)

(defun common-sort-ms90-3-jform (jform key set-progress-ctr-p) 
   (declare (ignore key set-progress-ctr-p))
   jform)

;;;--------------------------------------------------

(defun reverse-init-jform-mspath (jform) 
  (case (jform-type jform)
        ((literal disjunction universal) jform)
        (conjunction (setf (conjunction-components jform)
                           (reverse (conjunction-components jform))))
        (t (throwfail (jform-type jform)
                  " is illegal in jforms. reverse-init-jform-mspath"))))

(defun reverse-sort-ms90-3-jform (jform key set-progress-ctr-p)
  (declare (ignore key set-progress-ctr-p))
  (case (jform-type jform)
   (literal jform)
   (conjunction  (setf (conjunction-components jform) (reverse (conjunction-components jform))))
   (disjunction  (setf (disjunction-components jform) (reverse (disjunction-components jform))))
   (universal jform)
   (t (throwfail "Unknown type of jform."))))

;;;--------------------------------------------------

;;;The following function is called by the function following it.
(defun pathnum-sorting-jforms (jforms)
   (sort jforms #'(lambda (x y) (if (= (jform-progress x) (jform-progress y))
	                            (< (jform-num-paths-below x) (jform-num-paths-below y))
	                            (> (jform-progress x) (jform-progress y))))))

;;;used for max-dup-paths
(defun pathnum-init-jform (jform)
  (case (jform-type jform)
    (literal (setf (jform-num-paths-below jform) 1))
    (conjunction
     (mapc #'pathnum-init-jform (conjunction-components jform))
     (setf (jform-num-paths-below jform)
           (or (apply-fn-with-key #'* (conjunction-components jform) 
                                  #'jform-num-paths-below)
               1)))
    (disjunction
     (mapc #'pathnum-init-jform (disjunction-components jform))
     (setf (jform-num-paths-below jform)
           (or (apply-fn-with-key #'+ (disjunction-components jform) 
                                  #'jform-num-paths-below)
               0)))	 
    (universal
     (pathnum-init-jform (universal-scope jform))
     (setf (jform-num-paths-below jform)
           (jform-num-paths-below (universal-scope jform))))
    (t (throwfail (jform-type jform)
                  " is illegal in jforms. pathnum-init-jform"))))

(defun pathnum-init-jform-mspath (jform)
  (case (jform-type jform)
    (literal (setf (jform-num-paths-below jform) 1))
    (conjunction
     (mapc #'pathnum-init-jform-mspath (conjunction-components jform))
     (setf (jform-num-paths-below jform)
           (or (apply-fn-with-key #'* (conjunction-components jform) 
                                  #'jform-num-paths-below)
               1))
     (setf (conjunction-components jform)
           (funcall #'pathnum-sorting-jforms (conjunction-components jform))))
    (disjunction
     (mapc #'pathnum-init-jform-mspath (disjunction-components jform))
     (setf (jform-num-paths-below jform)
           (or (apply-fn-with-key #'+ (disjunction-components jform) 
                                  #'jform-num-paths-below)
               0))	 
     (setf (disjunction-components jform)
           (funcall #'pathnum-sorting-jforms (disjunction-components jform))))
    (universal
     (pathnum-init-jform-mspath (universal-scope jform))
     (setf (jform-num-paths-below jform)
           (jform-num-paths-below (universal-scope jform))))
    (t (throwfail (jform-type jform)
                  " is illegal in jforms. pathnum-init-jform-mspath"))))

(defun pathnum-tree-sorting (jforms)
   (sort jforms #'< :key #'jform-num-paths-below))

(defun pathnum-sort-ms90-3-jform (jform key set-progress-ctr-p)
  (case (jform-type jform)
    (literal nil)
    (conjunction
     (when set-progress-ctr-p
       (let ((ctr (length (conjunction-components jform))))
         (dolist (conj (conjunction-components jform))
           (setf (jform-progress conj) (decf ctr)))))
     (setf (conjunction-components jform)
           (sort (conjunction-components jform) #'> :key key))
     ;the dolist below is new MB Wed Feb  5 13:36:38 1997
     (dolist (conj (conjunction-components jform))
       (pathnum-sort-ms90-3-jform conj key set-progress-ctr-p)))
    (disjunction
     ;the setf below is new MB Wed Feb  5 13:36:48 1997
     (setf (disjunction-components jform)
           (sort (disjunction-components jform) #'> :key key))
     (dolist (disj (disjunction-components jform))
       (pathnum-sort-ms90-3-jform disj key set-progress-ctr-p)))
    (universal (pathnum-sort-ms90-3-jform (universal-scope jform) key
                                  set-progress-ctr-p))))

;;;--------------------------------------------------

;;;The following function is called by the function following it.
(defun reversed-pathnum-sorting-jforms (jforms)
   (sort jforms #'(lambda (x y) (if (= (jform-progress x) (jform-progress y))
	                            (> (jform-num-paths-below x) (jform-num-paths-below y))
	                            (< (jform-progress x) (jform-progress y))))))

;;;used for max-dup-paths
(defun reversed-pathnum-init-jform (jform)
  (case (jform-type jform)
    (literal (setf (jform-num-paths-below jform) 1))
    (conjunction
     (mapc #'reversed-pathnum-init-jform (conjunction-components jform))
     (setf (jform-num-paths-below jform)
           (or (apply-fn-with-key #'* (conjunction-components jform) 
                                  #'jform-num-paths-below)
               1)))
    (disjunction
     (mapc #'reversed-pathnum-init-jform (disjunction-components jform))
     (setf (jform-num-paths-below jform)
           (or (apply-fn-with-key #'+ (disjunction-components jform) 
                                  #'jform-num-paths-below)
               0)))	 
    (universal
     (reversed-pathnum-init-jform (universal-scope jform))
     (setf (jform-num-paths-below jform)
           (jform-num-paths-below (universal-scope jform))))
    (t (throwfail (jform-type jform)
                  " is illegal in jforms. pathnum-init-jform"))))

(defun reversed-pathnum-init-jform-mspath (jform)
  (case (jform-type jform)
    (literal (setf (jform-num-paths-below jform) 1))
    (conjunction
     (mapc #'reversed-pathnum-init-jform-mspath (conjunction-components jform))
     (setf (jform-num-paths-below jform)
           (or (apply-fn-with-key #'* (conjunction-components jform) 
                                  #'jform-num-paths-below)
               1))
     (setf (conjunction-components jform)
           (funcall #'reversed-pathnum-sorting-jforms (conjunction-components jform))))
    (disjunction
     (mapc #'reversed-pathnum-init-jform-mspath (disjunction-components jform))
     (setf (jform-num-paths-below jform)
           (or (apply-fn-with-key #'+ (disjunction-components jform) 
                                  #'jform-num-paths-below)
               0))	 
     (setf (disjunction-components jform)
           (funcall #'reversed-pathnum-sorting-jforms (disjunction-components jform))))
    (universal
     (reversed-pathnum-init-jform-mspath (universal-scope jform))
     (setf (jform-num-paths-below jform)
           (jform-num-paths-below (universal-scope jform))))
    (t (throwfail (jform-type jform)
                  " is illegal in jforms. pathnum-init-jform-mspath"))))

(defun reversed-pathnum-tree-sorting (jforms)
   (sort jforms #'> :key #'jform-num-paths-below))

(defun reversed-pathnum-sort-ms90-3-jform (jform key set-progress-ctr-p)
  (case (jform-type jform)
    (literal nil)
    (conjunction
     (when set-progress-ctr-p
       (let ((ctr (length (conjunction-components jform))))
         (dolist (conj (conjunction-components jform))
           (setf (jform-progress conj) (decf ctr)))))
     (setf (conjunction-components jform)
           (sort (conjunction-components jform) #'< :key key))
     ;the dolist below is new MB Wed Feb  5 13:36:38 1997
     (dolist (conj (conjunction-components jform))
       (pathnum-sort-ms90-3-jform conj key set-progress-ctr-p)))
    (disjunction
     ;the setf below is new MB Wed Feb  5 13:36:48 1997
     (setf (disjunction-components jform)
           (sort (disjunction-components jform) #'< :key key))
     (dolist (disj (disjunction-components jform))
       (reversed-pathnum-sort-ms90-3-jform disj key set-progress-ctr-p)))
    (universal (reversed-pathnum-sort-ms90-3-jform (universal-scope jform) key
                                  set-progress-ctr-p))))

;;;-----------------------------------------------------------

(defvar rigid-weight -1)
(defvar flex-weight 1)

(defun prefer-rigid1 (jform)
  (case (jform-type jform)
    ((disjunction conjunction)
       (let* ((hxcomponents 
              (case (jform-type jform)
                    (disjunction (disjunction-components jform))
                    (conjunction (conjunction-components jform))))
             (rigid1-list (mapcar #'(lambda (x) (cons x (prefer-rigid1 x))) 
                                  hxcomponents))
             (rigid-weight (apply #'+  (mapcar #'cdr rigid1-list))))
          (case (jform-type jform)
                (disjunction (setf (disjunction-components jform)
                                   (mapcar #'car (sort rigid1-list #'< :key #'cdr))))
                (conjunction (setf (conjunction-components jform)
                                   (mapcar #'car (sort rigid1-list #'< :key #'cdr)))))
          rigid-weight)) 
    (universal (prefer-rigid1 (universal-scope jform)))
    (literal (if (rigidnode-p jform) rigid-weight flex-weight))))
  
(defun rigid1-init-jform-mspath (jform)
  (prefer-rigid1 jform) jform)

(defun rigid1-tree-sorting (jform) jform)

(defun rigid1-sort-ms90-3-jform (jform key set-progress-ctr-p)
  (declare (ignore key set-progress-ctr-p))
  (prefer-rigid1 jform) jform)

;;;-----------------------------------------------------------

(defordercom prefer-rigid2
   (init-jform-mspath rigid2-init-jform-mspath)
   (tree-sorting rigid2-tree-sorting)
   (sort-ms90-3-jform rigid2-sort-ms90-3-jform)
   (mhelp "If the flag ORDER-COMPONENTS is set to PREFER-RIGID2, then 
the order of the components in the jform of the current eproof will be sorted 
in terms of the number of rigid literals in a jform before beginning the 
mating search."))

(defun rigid2-init-jform-mspath (jform)
  (prefer-rigid2 jform) jform)

(defun rigid2-tree-sorting (jform) jform)

(defun rigid2-sort-ms90-3-jform (jform key set-progress-ctr-p)
  (declare (ignore key set-progress-ctr-p))
  (prefer-rigid2 jform) jform)

(defun prefer-rigid2 (jform)
  (case (jform-type jform)
    ((disjunction conjunction)
       (let* ((disj (eq (jform-type jform) 'disjunction))
              (hxcomponents 
                (if disj
                    (disjunction-components jform)
                    (conjunction-components jform)))
              (length (1- (length hxcomponents)))
              (rigid2-list (mapcar #'(lambda (x) (cons x (prefer-rigid2 x))) 
                                  hxcomponents))
              (rigid-weight (apply #'+  (mapcar #'cadr rigid2-list)))
              (struct-weight (apply #'+ (mapcar #'cddr rigid2-list))))
             (if disj
                (setf (disjunction-components jform)
                      (mapcar #'car (sort rigid2-list 
                                         #'(lambda (x y) (or (< (car x) (car y))
                                                             (and (= (car x) (car y)) 
                                                                  (< (cdr x) (cdr y)))))
                                         :key #'cdr)))
                (setf (conjunction-components jform)
                      (mapcar #'car (sort rigid2-list                                 
                                         #'(lambda (x y) (or (< (car x) (car y))
                                                             (and (= (car x) (car y)) 
                                                                  (< (cdr x) (cdr y)))))
                                         :key #'cdr))))
             (cons rigid-weight 
                   (if disj (- struct-weight length) (+ struct-weight length)))))
    (universal (prefer-rigid2 (universal-scope jform)))
    (literal (if (rigidnode-p jform) (cons rigid-weight 0) (cons flex-weight 0)))))


;;;-----------------------------------------------------------
(defordercom prefer-rigid3
   (init-jform-mspath rigid3-init-jform-mspath)
   (tree-sorting rigid3-tree-sorting)
   (sort-ms90-3-jform rigid3-sort-ms90-3-jform)
   (mhelp "If the flag ORDER-COMPONENTS is set to PREFER-RIGID3, then 
the components in the jform of the current eproof will be sorted 
as for PREFER-RIGID2, but with preference given to literals that 
arise from DUAL rewriting."))

(defun rigid3-init-jform-mspath (jform)
  (prefer-rigid3 jform) jform)

(defun rigid3-tree-sorting (jform) jform)

(defun rigid3-sort-ms90-3-jform (jform key set-progress-ctr-p)
  (declare (ignore key set-progress-ctr-p))
  (prefer-rigid3 jform) jform)

(defvar defn-weight -100) ;i.e. *very* light! MB Thu Feb 20 14:36:59 1997

(defun prefer-rigid3 (jform)
  (declare (special *hacked-rewrites-list* current-eproof))
  (when (and (lazy2-used) *hacked-rewrites-list*)
	(fiddle-with-def-leaves (eproof-etree current-eproof)))
  (prefer-rigid3-real jform))

(defun prefer-rigid3-real (jform)
  (declare (special *hacked-rewrites-list*))
  (case (jform-type jform)
    ((disjunction conjunction)
       (let* ((disj (eq (jform-type jform) 'disjunction))
              (hxcomponents 
                (if disj
                    (disjunction-components jform)
                    (conjunction-components jform)))
              (length (1- (length hxcomponents)))
              (rigid3-list (mapcar #'(lambda (x) (cons x (prefer-rigid3-real x))) 
                                  hxcomponents))
              (rigid-weight (apply #'+  (mapcar #'cadr rigid3-list)))
              (struct-weight (apply #'+ (mapcar #'cddr rigid3-list))))
             (if disj
                (setf (disjunction-components jform)
                      (mapcar #'car (sort rigid3-list 
                                         #'(lambda (x y) (or (< (car x) (car y))
                                                             (and (= (car x) (car y)) 
                                                                  (< (cdr x) (cdr y)))))
                                         :key #'cdr)))
                (setf (conjunction-components jform)
                      (mapcar #'car (sort rigid3-list                                 
                                         #'(lambda (x y) (or (< (car x) (car y))
                                                             (and (= (car x) (car y)) 
                                                                  (< (cdr x) (cdr y)))))
                                         :key #'cdr))))
             (cons rigid-weight 
                   (if disj (- struct-weight length) (+ struct-weight length)))))
    (universal (prefer-rigid3-real (universal-scope jform)))
    (literal (if (and *hacked-rewrites-list* (member (literal-name jform) 
						     (mapcar #'(lambda (x) (and (cddr x) (leaf-name (cddr x))))
							     *hacked-rewrites-list*) :test 'equal))
		 (cons defn-weight 0)
	       (if (rigidnode-p jform) 
		   (cons rigid-weight 0) 
		 (cons flex-weight 0))))))


;;;-----------------------------------------------------------
;;;The following functions are used in ms90-3. The idea is
;;;to record the order of universal quantifiers, then recover
;;;it after search is done.

(defun init-position (jform)
  (case (jform-type jform)
        ((conjunction disjunction)
         (let ((hxcomponents 
               (case (jform-type jform)
                     (conjunction (conjunction-components jform))
                     (disjunction (disjunction-components jform))))
               (i -1))
            (dolist (hxcomponent hxcomponents nil)
                    (progn (init-position hxcomponent)
                           (setf (jform-position hxcomponent) (incf i))))
            (case (jform-type jform)
                  (conjunction (setf (conjunction-components jform) hxcomponents))
                  (disjunction (setf (disjunction-components jform) hxcomponents)))))
        (universal (setf (universal-scope jform) (init-position (universal-scope jform))))
        (literal jform))
   jform)         
   
(defun recover-position (jform)
  (case (jform-type jform)
        ((conjunction disjunction)
         (let* ((hxcomponents 
                (case (jform-type jform)
                      (conjunction (conjunction-components jform))
                      (disjunction (disjunction-components jform)))) 
                (hxnewcomponents (sort (mapcar #'recover-position hxcomponents) 
                                        #'< :key #'jform-position)))
              (case (jform-type jform)
                    (conjunction (setf (conjunction-components jform) hxnewcomponents))
                    (disjunction (setf (disjunction-components jform) hxnewcomponents)))
              jform))
         (universal (progn (setf (universal-scope jform) 
                                 (recover-position (universal-scope jform)))
                           jform))
         (literal jform)))

