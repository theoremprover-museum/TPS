;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
;;;
;;; File TEST-TOP-MENUS
;;;
;;; defines menus for the test-top level.
;;;

(deffile test-top-menus
  (part-of mating)
  (extension lisp)
  (mhelp "Defines menus for test-top."))

(context subtoplevels)

(defmenuitem TEST0
  (display-name "TEST")
  (placement 300)
  (command "TEST")
  (parent TOP-LEVELS)
  (hotkey #\t)
  (mhelp ""))

(defmenuitem UNIFORM-SEARCH
  (display-name "UNIFORM-SEARCH")
  (placement 301)
  (command "UNIFORM-SEARCH")
  (parent TOP-LEVELS)
  (mhelp ""))

(defmenuitem UNIFORM-SEARCH-L
  (display-name "UNIFORM-SEARCH-L")
  (placement 302)
  (command "UNIFORM-SEARCH-L")
  (parent TOP-LEVELS)
  (mhelp ""))

(defmenu TEST
  (display-name "Test")
  (placement 1)
  (parent TEST-TOP)
  (mhelp ""))

(defmenuitem BREADTH-FIRST-SEARCH
  (display-name "BREADTH-FIRST-SEARCH")
  (placement 2)
  (command "BREADTH-FIRST-SEARCH")
  (parent TEST)
  (mhelp ""))

(defmenuitem CLOSE-TESTWIN2
  (display-name "CLOSE-TESTWIN")
  (placement 3)
  (command "CLOSE-TESTWIN")
  (parent TEST)
  (mhelp ""))

(defmenuitem CONTINUE
  (display-name "CONTINUE")
  (placement 4)
  (command "CONTINUE")
  (parent TEST)
  (mhelp ""))

(defmenuitem EXHAUSTIVE-SEARCH
  (display-name "EXHAUSTIVE-SEARCH")
  (placement 5)
  (command "EXHAUSTIVE-SEARCH")
  (parent TEST)
  (mhelp ""))

(defmenuitem FIND-BEST-MODE
  (display-name "FIND-BEST-MODE")
  (placement 6)
  (command "FIND-BEST-MODE")
  (parent TEST)
  (mhelp ""))

(defmenuitem GO2345
  (display-name "GO")
  (placement 7)
  (command "GO")
  (parent TEST)
  (mhelp ""))

(defmenuitem OPEN-TESTWIN
  (display-name "OPEN-TESTWIN")
  (placement 8)
  (command "OPEN-TESTWIN")
  (parent TEST)
  (mhelp ""))

(defmenuitem PRESS-DOWN
  (display-name "PRESS-DOWN")
  (placement 9)
  (command "PRESS-DOWN")
  (parent TEST)
  (mhelp ""))

(defmenuitem PRESS-DOWN-2
  (display-name "PRESS-DOWN-2")
  (placement 10)
  (command "PRESS-DOWN-2")
  (parent TEST)
  (mhelp ""))

(defmenuitem PUSH-UP
  (display-name "PUSH-UP")
  (placement 11)
  (command "PUSH-UP")
  (parent TEST)
  (mhelp ""))

(defmenuitem PUSH-UP-2
  (display-name "PUSH-UP-2")
  (placement 12)
  (command "PUSH-UP-2")
  (parent TEST)
  (mhelp ""))

(defmenuitem SEARCH-PLACEMENT2
  (display-name "SEARCH-PLACEMENT")
  (placement 13)
  (command "SEARCH-PLACEMENT")
  (parent TEST)
  (mhelp ""))

(defmenuitem LEAVE7
  (display-name "LEAVE")
  (placement 100)
  (command "LEAVE")
  (parent TEST)
  (mhelp ""))

(defmenu SEARCHLISTS
  (display-name "Searchlists")
  (placement 2)
  (parent TEST-TOP)
  (mhelp ""))

(defmenuitem ADD-FLAG
  (display-name "ADD-FLAG")
  (placement 2)
  (command "ADD-FLAG")
  (parent SEARCHLISTS)
  (mhelp ""))

(defmenuitem ADD-FLAG*
  (display-name "ADD-FLAG*")
  (placement 3)
  (command "ADD-FLAG*")
  (parent SEARCHLISTS)
  (mhelp ""))

(defmenuitem ADD-FUNCTION
  (display-name "ADD-FUNCTION")
  (placement 4)
  (command "ADD-FUNCTION")
  (parent SEARCHLISTS)
  (mhelp ""))

(defmenuitem ADD-SUBJECTS
  (display-name "ADD-SUBJECTS")
  (placement 5)
  (command "ADD-SUBJECTS")
  (parent SEARCHLISTS)
  (mhelp ""))

(defmenuitem NEW-SEARCHLIST
  (display-name "NEW-SEARCHLIST")
  (placement 6)
  (command "NEW-SEARCHLIST")
  (parent SEARCHLISTS)
  (mhelp ""))

(defmenuitem QUICK-DEFINE
  (display-name "QUICK-DEFINE")
  (placement 7)
  (command "QUICK-DEFINE")
  (parent SEARCHLISTS)
  (mhelp ""))

(defmenuitem REM-FLAG
  (display-name "REM-FLAG")
  (placement 8)
  (command "REM-FLAG")
  (parent SEARCHLISTS)
  (mhelp ""))

(defmenuitem REM-FLAG*
  (display-name "REM-FLAG*")
  (placement 9)
  (command "REM-FLAG*")
  (parent SEARCHLISTS)
  (mhelp ""))

(defmenuitem REVISE-DEFAULTS
  (display-name "REVISE-DEFAULTS")
  (placement 10)
  (command "REVISE-DEFAULTS")
  (parent SEARCHLISTS)
  (mhelp ""))

(defmenuitem SCALE-DOWN
  (display-name "SCALE-DOWN")
  (placement 11)
  (command "SCALE-DOWN")
  (parent SEARCHLISTS)
  (mhelp ""))

(defmenuitem SCALE-UP
  (display-name "SCALE-UP")
  (placement 12)
  (command "SCALE-UP")
  (parent SEARCHLISTS)
  (mhelp ""))

(defmenuitem SEARCHLISTS2
  (display-name "SEARCHLISTS")
  (placement 13)
  (command "SEARCHLISTS")
  (parent SEARCHLISTS)
  (mhelp ""))

(defmenuitem SHOW-SEARCHLIST
  (display-name "SHOW-SEARCHLIST")
  (placement 14)
  (command "SHOW-SEARCHLIST")
  (parent SEARCHLISTS)
  (mhelp ""))

(defmenuitem VARY-MODE
  (display-name "VARY-MODE")
  (placement 15)
  (command "VARY-MODE")
  (parent SEARCHLISTS)
  (mhelp ""))

(defmenu TEST-LIB
  (display-name "Test Lib")
  (placement 3)
  (parent TEST-TOP)
  (mhelp ""))

(defmenuitem DELETE23
  (display-name "DELETE")
  (placement 2)
  (command "DELETE")
  (parent TEST-LIB)
  (mhelp ""))

(defmenuitem FETCH23
  (display-name "FETCH")
  (placement 3)
  (command "FETCH")
  (parent TEST-LIB)
  (mhelp ""))

(defmenuitem INSERT2
  (display-name "INSERT")
  (placement 4)
  (command "INSERT")
  (parent TEST-LIB)
  (mhelp ""))
