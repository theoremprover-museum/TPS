;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1990 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
(part-of ms90-3)

(deffile ms90-3-unif-match
  (part-of ms90-3)
  (mhelp "Implementation of Huet's Match routine for Path-focused
duplication."))

#+comment(defun listify-type (type)
  (do ((typelist type (car typelist))
       (counter 1 (1+ counter)))
      ((atom typelist)
       (let ((type-vector (make-array counter)))
	 (declare (simple-array type-vector))
	 (decf counter)
	 (dotimes (i counter
		     (progn
		      (setf (aref type-vector counter) type)
		      type-vector))
	   (setf (aref type-vector i) (cdr type))
	   (setq type (car type)))))))
;already got this in unif-match

#+comment(defun prefix-lambda (term vector k)
  (do ((k (1- k) (1- k)))
      ((minusp k) term)
    (setq term (acons (aref vector k) 'lambda term))))
;already got this in unif-match

(defun ho-construct-type (type vector k)
  (do ((k (1- k) (1- k)))
      ((minusp k) type)
    (setq type (cons type (aref vector k)))))

(defun ho-construct-term (term vector k)
  (dotimes (i k term)
    (setq term (cons term (aref vector i)))))

(defun ho-construct-E-i (term type w-vars type-fhead k)
  (let* ((type (ho-construct-type type type-fhead k))
	 (head (cons (ren-var-uni-ho h-var-prefix type) -2)))
    (cons term (ho-construct-term head w-vars k))))

(defun ho-construct-E-j (term type w-vars type-fhead k v-vars
                              v-vars-typevector k1)
  (let* ((type (ho-construct-type 
		(ho-construct-type type v-vars-typevector k1)
		type-fhead k))
	 (head (cons (ren-var-uni-ho h-var-prefix type) -2)))
    (cons term (ho-construct-term (ho-construct-term head w-vars k)
				  v-vars k1)))) 

(defun ho-construct-projection-1 (head w-i-typevector m w-vars type-fhead k)
  (let ((term head))
    (dotimes (i m (prefix-lambda term w-vars k))
      (setq term (ho-construct-E-i term (aref w-i-typevector i) w-vars
                                   type-fhead k)))))

(defun projections-ho (w-vars type-fhead p1 fhead)
  (if (memq fhead imitation-h-var-list) nil
      (let ((projections nil)
            (resultype (aref type-fhead (1- (length type-fhead)))))
        (dotimes (i p1 projections)
          (let ((type-w-i (listify-type (aref type-fhead i))))
            (if (eq (aref type-w-i (1- (length type-w-i))) resultype)
                (push (cons fhead
                            (ho-construct-projection-1
                             (aref w-vars i) type-w-i (1- (length type-w-i))
                             w-vars type-fhead p1))
                      projections)))))))

(defun projections-ho-count (type-fhead p1 fhead)
  (if (memq fhead imitation-h-var-list) 0
      (let ((projections-count 0)
            (resultype (aref type-fhead (1- (length type-fhead)))))
        (dotimes (i p1 projections-count)
          (let ((type-w-i (listify-type (aref type-fhead i))))
            (when (eq (aref type-w-i (1- (length type-w-i))) resultype)
	      (incf projections-count)))))))

(defun ho-construct-imitation-term (rhead w-vars type-fhead type-rhead k
                                          p2-p1+k)
  (let ((term rhead))
    (dotimes (counter p2-p1+k
                      (progn (when (eq rhead 'not)
                               (push (find-fhead (cdr term)) neg-h-var-list))
                             (prefix-lambda term w-vars k)))
      (setq term (ho-construct-E-i
                  term (aref type-rhead counter) w-vars type-fhead
                  k)))))

(defun pfd-ho-dneg-imitation-term (w-vars type-fhead k)
  (let* ((type (ho-construct-type 'O type-fhead k))
	 (head (cons (ren-var-uni-ho h-var-prefix type) -2)))
    (push head neg-h-var-list)
    (push head imitation-h-var-list)
    (prefix-lambda (ho-construct-term head w-vars k) w-vars k)))

(defun construct-sk-imitation-term-ho (rhead w-vars type-fhead type-rhead k p2-p1+k nan)
    (let ((term rhead))
      (dotimes (counter p2-p1+k
                      (progn (when (eq rhead 'not)
                               (push (find-fhead (cdr term)) neg-h-var-list))
                             (prefix-lambda term w-vars k)))
         (setq term (if (< counter nan)
                        (cons term (cons (ren-var-uni-ho h-var-prefix 
                                                         (aref type-rhead counter)) -2))
                        (ho-construct-E-i term (aref type-rhead counter) w-vars type-fhead k))))))
                  
(defun imitation-ho (rhead w-vars type-fhead type-rhead p1 rterm-binder fhead)
  (unless (memq rhead rterm-binder) 
    (let ((nan (sk-const-p rhead)))
         (if nan
             (list (cons fhead
                        (construct-sk-imitation-term-ho
                             rhead w-vars type-fhead type-rhead p1 
                             (1- (length type-rhead)) nan)))
        (if (eq rhead 'not)
            (if (memq fhead neg-h-var-list)
                (if (memq fhead imitation-h-var-list) nil
                    (list (cons fhead (pfd-ho-dneg-imitation-term
                                      w-vars type-fhead p1))))
                (list (cons fhead
                            (ho-construct-imitation-term
                            rhead w-vars type-fhead type-rhead p1
                            (1- (length type-rhead))))
                      (cons fhead (pfd-ho-dneg-imitation-term
                                   w-vars type-fhead p1))))
            (list (cons fhead
                        (ho-construct-imitation-term
                         rhead w-vars type-fhead type-rhead p1
                         (1- (length type-rhead))))))))))


;;;The following function is the original one, which cannot
;;;handle Skolem functions correctly.
#+comment
(defun imitation-ho (rhead w-vars type-fhead type-rhead p1 rterm-binder fhead)
  (if (memq rhead rterm-binder) nil
      (if (eq rhead 'not)
          (if (memq fhead neg-h-var-list)
              (if (memq fhead imitation-h-var-list) nil
                  (list (cons fhead (pfd-ho-dneg-imitation-term
                                     w-vars type-fhead p1))))
              (list (cons fhead
                          (ho-construct-imitation-term
                           rhead w-vars type-fhead type-rhead p1
                           (1- (length type-rhead))))
                    (cons fhead (pfd-ho-dneg-imitation-term
                                 w-vars type-fhead p1))))
          (list (cons fhead
                      (ho-construct-imitation-term
                       rhead w-vars type-fhead type-rhead p1
                       (1- (length type-rhead))))))))

;;; can return only 0, 1 or 2.
;;; special handling of negation can produce 2 substitutions: 
;;; an imitations as in Huet's paper and an identity substitution.

(defun imitation-ho-count (rhead rterm-binder fhead)
  (if (memq rhead rterm-binder) 0
      (if (eq rhead 'not)
          (if (memq fhead neg-h-var-list)
              (if (memq fhead imitation-h-var-list) 0 1)
	      2)
	  1)))

(defun ren-var-uni-ho (nameroot new-type  &optional (counter nil))
  (let* ((counter (or counter (let ((num (getren-counter nameroot)))
                                (setren-counter nameroot (1+ num))
                                num)))
	 (name (make-symbol (format nil "~A^~D~A" nameroot counter
                                    (type-to-string new-type)))))
    (putprop name new-type 'type)
    (putprop name new-type 'unabbreviated-type)
    ;;(set-superscript-face (intern-str (format nil "~A^~D" nameroot counter)))
    name))

(defun create-new-w-vars (w-vars-typevector p1)
  (let ((w-var-vector (make-array p1 :element-type 'symbol)))
    (declare (simple-array w-var-vector))
    (dotimes (i p1 w-var-vector)
      (setf (aref w-var-vector i)
	    (ren-var-uni-ho w-var-prefix (aref w-vars-typevector i))))))

(defun ho-match-top (rhead rterm-binder fhead)
  (let* ((type-fhead (listify-type (type (car fhead))))
         (p1 (1- (length type-fhead)))
         (w-vars (create-new-w-vars type-fhead p1)))
    (nconc
     (projections-ho w-vars type-fhead p1 fhead)
     (imitation-ho rhead w-vars type-fhead (listify-type (type rhead))
                   p1 rterm-binder fhead))))

(defun find-fhead (term)
  (if (numberp (cdr term)) term
      (if (lambda-bd-p term)
	  (find-fhead (cdr term))
	  (find-fhead (car term)))))

(defun ho-match-top-count (rhead rterm-binder fhead)
  (let* ((type-fhead (listify-type (type (car fhead))))
         (p1 (1- (length type-fhead))))
    (+ (projections-ho-count type-fhead p1 fhead)
       (imitation-ho-count rhead rterm-binder fhead))))
