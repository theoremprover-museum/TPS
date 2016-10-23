;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
;(part-of ms88)

;;;
;;; File: MATING-MOVE
;;; 
;;; Defines mating-search moving commands assuming WFFMVE has been read.
;;;


(deffile mating-move
;  (part-of ms88)
  (part-of mating)
  (extension lsp)
  (mhelp "Defines mating-search moving operations from wff operations."))

(context moving)

(defmateop \0
  (mate-move-fn t)
  (mate-alias negate-last-move)
  (mate-applicable-p etree-p)
  (mhelp "Move back to previous node, e.g., undo the last L or R
command. Note that 0 stands for the numeral zero."))

(defmateop ^
  (mate-alias move-to-root)
  (mate-move-fn t)
  (mate-applicable-p etree-p)
  (mhelp "Move upwards to root of expansion tree."))

;;;
;;; MOVE-TO-ROOT moves from the current topnode to the global topnode
;;;

(defun move-to-root (current)
  (declare (ignore current))
  (eproof-etree current-eproof))


(defmateop l
  (mate-alias move-to-left)
  (mate-move-fn t)
  (mate-result-> current-topnode)
  (mhelp "For an infix etree node, move to the left argument.")
  (matewff-argname etree))


(defun move-to-left (etree)
  (if (infix-p etree)
      (car (etree-components etree))
      (throwfail (etree . etree) " is not an infix node.")))



(defmateop r
  (mate-alias move-to-right)
  (mate-move-fn t)
  (mate-result-> current-topnode)
  (mhelp "For an infix etree node, move to the right argument.")
  (matewff-argname etree))

(defun move-to-right (etree)
  (if (infix-p etree) 
      (cadr (etree-components etree))
      (throwfail (etree . etree) " is not an infix node.")))
 

(defmateop fi
  (mate-alias find-infix-etree)
  (mate-move-fn t)
  (mate-result-> current-topnode)
  (mate-applicable-p etree-p)
  (mhelp "Find an infix node.")
  (matewff-argname etree))

(defwffop find-infix-etree
  (argtypes etree)
  (resulttype etree)
  (argnames etree)
  (arghelp "etree")
  (mhelp "Find first infix node in etree."))

(defun find-infix-etree (etree)
  (let ((new (find-etree-node #'infix-p etree)))
    (if new 
	new
	(throwfail "No infix node found."))))

(defmateop fb
  (mate-alias find-binder-etree)
  (mate-move-fn t)
  (mate-result-> current-topnode)
  (mate-applicable-p etree-p)
  (mhelp "Find the topmost binder.")
  (matewff-argname etree))


(defun find-binder-etree (etree) 
  (let ((new (find-etree-node #'(lambda (x)
				  (memq (type-of x) '(skolem selection expansion)))
			      etree)))
    (if new
	new
	(throwfail "No bound node found."))))

(defun move-to-successor (n)
  (declare (special nodestack))
  (if (and (> n 0)
	   (nth (1- n) (etree-components current-topnode)))
      (progn 
       (push current-topnode nodestack)
       (setq current-topnode (nth (1- n) (etree-components current-topnode))))
       (throwfail (current-topnode . etree) (format nil " has no ~:R successor." n))))


(defmateop up
  (mate-alias up-node)
  (mate-move-fn t)
  (mate-applicable-p etree-p)
  (mate-result-> current-topnode)
  (mhelp "Move up one node in etree.")
  (matewff-argname etree))

(defun up-node (etree)
  (or (etree-parent etree)
      (throwfail "You are at the top of the tree.")))

(defmateop d
  (mate-alias down-node)
  (mate-move-fn t)
  (mate-result-> current-topnode)
  (mate-applicable-p etree-p)
  (mhelp "Move down one node in etree (to leftmost node if more than
one successor).")
  (matewff-argname etree))

(defun down-node (etree)
  (or (car (etree-components etree))
      (throwfail "Current node has no successors.")))

(defmateop goto
  (mate-alias goto-node)
  (mate-result-> current-topnode)
  (mhelp "Move to a specified node.")
  (matewff-argname etree))

(defwffop goto-node 
  (argnames node etree)
  (argtypes symbol etree)
  (mhelp "Move to specified node in an etree.")
  (resulttype etree))


(defun goto-node (node etree)
  (declare (special nodestack))
  (let* ((new-node (show-internal-name node))
	 (new (find-etree-node-name new-node etree)))
  (if new
      (progn 
       (push current-topnode nodestack)
       (setq current-topnode new))
      (throwfail node " not found in etree."))))

