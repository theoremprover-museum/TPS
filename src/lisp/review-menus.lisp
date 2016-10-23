;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of REVIEW-FLAGS)

;;;
;;; File REVIEW
;;;
;;; defines REVIEW menus
;;; cebrown 5/6/02

(deffile review-menus
  (part-of review-flags)
  (extension clisp)
  (mhelp "Defines menus for review top-level."))

(context subtoplevels)

(defmenuitem REVIEW0
  (display-name "REVIEW")
  (placement 6)
  (command "REVIEW")
  (parent TOP-LEVELS)
  (hotkey #\r)
  (etps t)
  (mhelp ""))

(defmenu REVIEW
  (display-name "Review")
  (placement 2)
  (parent REVIEW-TOP)
  (mhelp ""))

(defmenuitem CHANGED-FLAGS
  (display-name "CHANGED-FLAGS")
  (placement 2)
  (command "CHANGED-FLAGS")
  (parent REVIEW)
  (etps t)
  (mhelp ""))

(defmenuitem DESCRIBE
  (display-name "DESCRIBE")
  (placement 3)
  (command "DESCRIBE")
  (parent REVIEW)
  (etps t)
  (mhelp ""))

(defmenuitem DESCRIBE*
  (display-name "DESCRIBE*")
  (placement 4)
  (command "DESCRIBE*")
  (parent REVIEW)
  (etps t)
  (mhelp ""))

(defmenuitem KEY2
  (display-name "KEY")
  (placement 5)
  (command "KEY")
  (parent REVIEW)
  (etps t)
  (mhelp ""))

(defmenuitem LIST
  (display-name "LIST")
  (placement 6)
  (command "LIST")
  (parent REVIEW)
  (etps t)
  (mhelp ""))

(defmenuitem SET
  (display-name "SET")
  (placement 7)
  (command "SET")
  (parent REVIEW)
  (etps t)
  (mhelp ""))

(defmenuitem SETFLAG
  (display-name "SETFLAG")
  (placement 8)
  (command "SETFLAG")
  (parent REVIEW)
  (etps t)
  (mhelp ""))

(defmenuitem SETFLAGS1
  (display-name "SETFLAGS1")
  (placement 9)
  (command "SETFLAGS1")
  (parent REVIEW)
  (etps t)
  (mhelp ""))

(defmenuitem SETFLAGS2
  (display-name "SETFLAGS2")
  (placement 10)
  (command "SETFLAGS2")
  (parent REVIEW)
  (etps t)
  (mhelp ""))

(defmenuitem SUBJECTS
  (display-name "SUBJECTS")
  (placement 11)
  (command "SUBJECTS")
  (parent REVIEW)
  (etps t)
  (mhelp ""))

(defmenuitem UPDATE
  (display-name "UPDATE")
  (placement 12)
  (command "UPDATE")
  (parent REVIEW)
  (etps t)
  (mhelp ""))

(defmenuitem UPDATE-RELEVANT
  (display-name "UPDATE-RELEVANT")
  (placement 13)
  (command "UPDATE-RELEVANT")
  (parent REVIEW)
  (etps t)
  (mhelp ""))

(defmenu REVIEW-UNIFICATION
  (display-name "Review Unification")
  (placement 14)
  (parent REVIEW)
  (mhelp ""))

(defmenuitem UNIF-DEPTHS
  (display-name "UNIF-DEPTHS")
  (placement 2)
  (command "UNIF-DEPTHS")
  (parent REVIEW-UNIFICATION)
  (mhelp ""))

(defmenuitem UNIF-NODEPTHS
  (display-name "UNIF-NODEPTHS")
  (placement 3)
  (command "UNIF-NODEPTHS")
  (parent REVIEW-UNIFICATION)
  (mhelp ""))

(defmenuitem LEAVE6
  (display-name "LEAVE")
  (placement 100)
  (command "LEAVE")
  (parent REVIEW)
  (etps t)
  (mhelp ""))

(defmenu MODES
  (display-name "Modes")
  (placement 2)
  (parent REVIEW-TOP)
  (mhelp ""))

(defmenuitem FIND-MODE
  (display-name "FIND-MODE")
  (placement 2)
  (command "FIND-MODE")
  (parent MODES)
  (mhelp ""))

(defmenuitem ADD-FLAG-TO-MODE
  (display-name "ADD-FLAG-TO-MODE")
  (placement 3)
  (command "ADD-FLAG-TO-MODE")
  (parent MODES)
  (etps t)
  (mhelp ""))

(defmenuitem COMPARE-MODES
  (display-name "COMPARE-MODES")
  (placement 4)
  (command "COMPARE-MODES")
  (parent MODES)
  (etps t)
  (mhelp ""))

(defmenuitem COPY-MODE
  (display-name "COPY-MODE")
  (placement 5)
  (command "COPY-MODE")
  (parent MODES)
  (etps t)
  (mhelp ""))

(defmenuitem MODE
  (display-name "MODE")
  (placement 6)
  (command "MODE")
  (parent MODES)
  (etps t)
  (mhelp ""))

(defmenuitem REMOVE-FLAG-FROM-MODE
  (display-name "REMOVE-FLAG-FROM-MODE")
  (placement 7)
  (command "REMOVE-FLAG-FROM-MODE")
  (parent MODES)
  (etps t)
  (mhelp ""))

