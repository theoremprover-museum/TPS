;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of theorems)

(deffile theorem-mac
  (part-of theorems)
  (extension lsp)
  (mhelp "Define defines the category of THEOREM with its various attributes."))

(context tps-theorems)

(eval-when (compile load eval)
(defun theorem-p (xxx)
  (and (symbolp xxx) (get xxx '%theorem%))))


(defcategory %theorem%
  (define deftheorem)
  (properties
   (assertion string)			; The wff
   (thm-type single)			; BOOK, EXERCISE, PRACTICE, LIBRARY
					; or TEST-PROBLEM. This implies a mode.
   (required-lemmas theoremlist)	; Lemmas to derive wff from
   (allowed-lemma-p single)		; A function of two args
   (allowed-cmd-p single)		; A function of two args
   (score single)			; Score (?) or NIL
   (remarks single)			; Maybe a help message from the teacher
   (fol single)                         ;T for exercises in first order logic.
   (mhelp single))			; optional MHELP
  (global-list global-theoremlist)
  (mhelp-line "theorem")
  (mhelp-fn theorem-help))

(defun library-theorem-p (lib-theorem)
  (and (symbolp lib-theorem)
       (get lib-theorem '%theorem%)
       (eq (get lib-theorem 'thm-type) 'library)))

(defun book-theorem-p (book-theorem)
  (and (symbolp book-theorem)
       (get book-theorem '%theorem%)
       (eq (get book-theorem 'thm-type) 'book)))

(defun practice-p (practice)
  (and (symbolp practice)
       (get practice '%theorem%)
       (eq (get practice 'thm-type) 'practice)))

(defun theorem-help (theorem category)
  (case (get theorem 'thm-type)
    (book (msgf "Book Theorem: "))
    (exercise (msgf "Exercise: "))
    (practice (msgf "Practice Exercise: "))
    (test-problem (msgf "Test Problem: "))
    (library (msgf "Library Theorem: "))
    (t (msgf "Unknown type of theorem: ")))
  (prtwff (funcall (get 'theorem-type 'getfn) theorem)
	  (leftmargin (curpos)))
  (when (get theorem 'allowed-cmd-p)
	(let ((helpfn (prepend (get theorem 'allowed-cmd-p) '-help)))
	  (when (fboundp helpfn) (funcall helpfn theorem))))
  (when (get theorem 'allowed-lemma-p)
	(let ((helpfn (prepend (get theorem 'allowed-lemma-p) '-help)))
	  (when (fboundp helpfn) (funcall helpfn theorem))))
  (princ-mhelp theorem category))

(defgwff-type theorem-type
  (checkfn theorem-p)
  (getfn theorem-type-wfftype-getfn)
  (mhelp "theorem: a theorem (exercise, practice, or theorem from the book)."))

(defun theorem-type-wfftype-getfn (theorem)
  (cond ((stringp (get theorem 'assertion))
	 (let ((assertion (rd-string (get theorem 'assertion))))
	   (putprop theorem assertion 'assertion)
	   assertion))
	(t (get theorem 'assertion))))

;; Allowed-lemma-p is a function of two arguments, the theorem in question
;; and the candidate lemma.  Here are some some library functions.

(defun allow-no-lemmas (theorem lemma)
  (declare (ignore theorem))
  (book-theorem-p lemma))

(defun allow-no-lemmas-help (theorem)
  (declare (ignore theorem))
  (msgf "Only book theorems may be ASSERTed as lemmas without proof.
For a complete list of book theorems, type PROBLEMS."))

;; allow-lower-nos works only if every theorem is of the form <letter><digits>
;; e.g. X2102, P2103, T1000, but not THM3.

(defun allow-lower-nos (theorem lemma)
  (< (theorem-no lemma) (theorem-no theorem)))

(defun allow-lower-nos-help (theorem)
  (declare (ignore theorem))
  (msgf "All theorems with lower numbers may be ASSERTed as lemmas without proof.
All book theorems may also be ASSERTed (type PROBLEMS for a list of them)."))

(defun theorem-no (theorem)
  (parse-integer (string theorem) :start 1))

(defun theorem-no-help (theorem)
  (msgf "To prove this result, you may ASSERT theorem " theorem " without proof.
You may also ASSERT any necessary book theorem (type PROBLEMS for a list of them)."))

;;; ONLY-REQUIRED-LEMMAS checks to see if the lemma is required for
;;; the exercise.

(defun only-required-lemmas (theorem lemma)
  (or (member lemma (get theorem 'required-lemmas))
      (book-theorem-p lemma)))

(defun only-required-lemmas-help (theorem)
  (msgf "You may ASSERT the following theorems, without proof, as lemmas : " (get theorem 'required-lemmas) t "You may also ASSERT any book theorem (type PROBLEMS for a list of them)."))

;;;
;;; Now some allowed-cmd-p which allows advice for all practice exercises
;;;

(defun allow-rulep (theorem command)
  (or (not (member command '(advice)))
      (practice-p theorem)))

(defun allow-rulep-help (theorem)
  (if (practice-p theorem)
      (msgf "All rules and commands are allowed.")
      (msgf "You may not use ADVICE, but all other rules and commands are allowed.")))

(defun allow-all (theorem command)
  (declare (ignore theorem command))
  t)

(defun allow-all-help (theorem)
  (declare (ignore theorem))
  (msgf "All rules and commands, including ADVICE, are allowed."))

(defun disallow-rulep (theorem command)
  (or (not (member command '(advice rulep)))
      (and (eq command 'advice) (practice-p theorem))))

(defun disallow-rulep-help (theorem)
  (if (practice-p theorem)
      (msgf "You may not use RULEP, but all other rules and commands are allowed.")
      (msgf "You may not use RULEP or ADVICE, but all other rules and commands
are allowed.")))
