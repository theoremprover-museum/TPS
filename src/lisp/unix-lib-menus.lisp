;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
;;;
;;; File UNIX-LIBRARY
;;;
;;; defines UNIX-LIBRARY, FLAGS etc.
;;;

(deffile unix-lib-menus
  (part-of library)
  (extension lisp)
  (mhelp "Defines top-level menus for unix-style library interface."))

(context subtoplevels)

(defmenuitem UNIX-STYLE
  (display-name "UNIXLIB")
  (placement 3)
  (command "UNIXLIB")
  (parent LIBRARY-TOP-LEVELS)
  (mhelp ""))

(defmenu UNIX-STYLE-LIB
  (display-name "Unix-style Lib")
  (placement 2)
  (parent UNIX-LIBRARY-TOP)
  (mhelp ""))

(defmenu UNIXLIB-READ
  (display-name "Read")
  (placement 2)
  (parent UNIX-STYLE-LIB)
  (mhelp ""))

(defmenuitem DESTROY2
  (display-name "DESTROY")
  (placement 2)
  (command "DESTROY")
  (parent UNIXLIB-READ)
  (mhelp ""))

(defmenuitem FETCH2
  (display-name "FETCH")
  (placement 3)
  (command "FETCH")
  (parent UNIXLIB-READ)
  (mhelp ""))

(defmenu UNIXLIB-DISPLAY
  (display-name "Display")
  (placement 2)
  (parent UNIX-STYLE-LIB)
  (mhelp ""))

(defmenuitem PINTERSECT2
  (display-name "PINTERSECT")
  (placement 2)
  (command "PINTERSECT")
  (parent UNIXLIB-DISPLAY)
  (mhelp ""))

(defmenuitem PINTERSECT*2
  (display-name "PINTERSECT*")
  (placement 3)
  (command "PINTERSECT*")
  (parent UNIXLIB-DISPLAY)
  (mhelp ""))

(defmenuitem SHOW2
  (display-name "SHOW")
  (placement 4)
  (command "SHOW")
  (parent UNIXLIB-DISPLAY)
  (mhelp ""))

(defmenuitem SHOW-ALL-WFFS2
  (display-name "SHOW-ALL-WFFS")
  (placement 5)
  (command "SHOW-ALL-WFFS")
  (parent UNIXLIB-DISPLAY)
  (mhelp ""))

(defmenuitem SHOW-HELP2
  (display-name "SHOW-HELP")
  (placement 6)
  (command "SHOW-HELP")
  (parent UNIXLIB-DISPLAY)
  (mhelp ""))

(defmenuitem SHOW-WFF2
  (display-name "SHOW-WFF")
  (placement 7)
  (command "SHOW-WFF")
  (parent UNIXLIB-DISPLAY)
  (mhelp ""))

(defmenuitem SHOW-WFF&HELP2
  (display-name "SHOW-WFF&HELP")
  (placement 8)
  (command "SHOW-WFF&HELP")
  (parent UNIXLIB-DISPLAY)
  (mhelp ""))

(defmenu UNIXLIB-CLASS
  (display-name "Unixlib Class")
  (placement 2)
  (parent UNIX-STYLE-LIB)
  (mhelp ""))

(defmenuitem CD
  (display-name "CD")
  (placement 2)
  (command "CD")
  (parent UNIXLIB-CLASS)
  (mhelp ""))

(defmenuitem CLASSIFY-ITEM2
  (display-name "CLASSIFY-ITEM")
  (placement 3)
  (command "CLASSIFY-ITEM")
  (parent UNIXLIB-CLASS)
  (mhelp ""))

(defmenuitem CP
  (display-name "CP")
  (placement 4)
  (command "CP")
  (parent UNIXLIB-CLASS)
  (mhelp ""))

(defmenuitem LN
  (display-name "LN")
  (placement 5)
  (command "LN")
  (parent UNIXLIB-CLASS)
  (mhelp ""))

(defmenuitem MKDIR
  (display-name "MKDIR")
  (placement 7)
  (command "MKDIR")
  (parent UNIXLIB-CLASS)
  (mhelp ""))

(defmenuitem PWD
  (display-name "PWD")
  (placement 8)
  (command "PWD")
  (parent UNIXLIB-CLASS)
  (mhelp ""))

(defmenuitem RM
  (display-name "RM")
  (placement 9)
  (command "RM")
  (parent UNIXLIB-CLASS)
  (mhelp ""))

(defmenuitem LEAVE3
  (display-name "Leave")
  (placement 300)
  (command "Leave")
  (parent UNIX-STYLE-LIB)
  (mhelp ""))

(defmenuitem UNIXLIB-SHOWPATH
  (display-name "UNIXLIB-SHOWPATH")
  (placement 17)
  (command "UNIXLIB-SHOWPATH")
  (parent LIBRARY-FLAGS)
  (mhelp ""))

