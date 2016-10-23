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
;;; File LIB-MENUS
;;;
;;; defines LIB MENUS
;;;

(deffile lib-menus
  (part-of library)
  (extension lisp)
  (mhelp "Defines top-level menus for library."))

(context subtoplevels)

(defmenu LIBRARY
  (display-name "Library")
  (placement 2)
  (parent LIBRARY-TOP)
  (mhelp ""))

(defmenu LIB-DISPLAY
  (display-name "Lib Display")
  (placement 2)
  (parent LIBRARY)
  (mhelp ""))

(defmenuitem KEY
  (display-name "KEY")
  (placement 2)
  (command "KEY")
  (parent LIB-DISPLAY)
  (mhelp ""))

(defmenuitem LIBFILES
  (display-name "LIBFILES")
  (placement 3)
  (command "LIBFILES")
  (parent LIB-DISPLAY)
  (mhelp ""))

(defmenuitem LIBOBJECTS-IN-FILE
  (display-name "LIBOBJECTS-IN-FILE")
  (placement 4)
  (command "LIBOBJECTS-IN-FILE")
  (parent LIB-DISPLAY)
  (mhelp ""))

(defmenuitem LIST-OF-LIBOBJECTS
  (display-name "LIST-OF-LIBOBJECTS")
  (placement 5)
  (command "LIST-OF-LIBOBJECTS")
  (parent LIB-DISPLAY)
  (mhelp ""))

(defmenuitem SCRIBE-ALL-WFFS
  (display-name "SCRIBE-ALL-WFFS")
  (placement 6)
  (command "SCRIBE-ALL-WFFS")
  (parent LIB-DISPLAY)
  (mhelp ""))

(defmenuitem SCRIBELIBDIR
  (display-name "SCRIBELIBDIR")
  (placement 7)
  (command "SCRIBELIBDIR")
  (parent LIB-DISPLAY)
  (remote-expert T)
  (mhelp ""))

(defmenuitem SCRIBELIBFILE
  (display-name "SCRIBELIBFILE")
  (placement 8)
  (command "SCRIBELIBFILE")
  (parent LIB-DISPLAY)
  (remote-expert T)
  (mhelp ""))

(defmenuitem SEARCH2
  (display-name "SEARCH")
  (placement 9)
  (command "SEARCH")
  (parent LIB-DISPLAY)
  (mhelp ""))

(defmenuitem SEARCH22
  (display-name "SEARCH2")
  (placement 10)
  (command "SEARCH2")
  (parent LIB-DISPLAY)
  (mhelp ""))

(defmenuitem SHOW
  (display-name "SHOW")
  (placement 11)
  (command "SHOW")
  (parent LIB-DISPLAY)
  (mhelp ""))

(defmenuitem SHOW*-WFF
  (display-name "SHOW*-WFF")
  (placement 12)
  (command "SHOW*-WFF")
  (parent LIB-DISPLAY)
  (mhelp ""))

(defmenuitem SHOW-ALL-WFFS
  (display-name "SHOW-ALL-WFFS")
  (placement 13)
  (command "SHOW-ALL-WFFS")
  (parent LIB-DISPLAY)
  (mhelp ""))

(defmenuitem SHOW-HELP
  (display-name "SHOW-HELP")
  (placement 14)
  (command "SHOW-HELP")
  (parent LIB-DISPLAY)
  (mhelp ""))

(defmenuitem SHOW-OBJECTS-IN-FILE
  (display-name "SHOW-OBJECTS-IN-FILE")
  (placement 15)
  (command "SHOW-OBJECTS-IN-FILE")
  (parent LIB-DISPLAY)
  (mhelp ""))

(defmenuitem SHOW-TIMING
  (display-name "SHOW-TIMING")
  (placement 16)
  (command "SHOW-TIMING")
  (parent LIB-DISPLAY)
  (mhelp ""))

(defmenuitem SHOW-WFF
  (display-name "SHOW-WFF")
  (placement 17)
  (command "SHOW-WFF")
  (parent LIB-DISPLAY)
  (mhelp ""))

(defmenuitem SHOW-WFF&HELP
  (display-name "SHOW-WFF&HELP")
  (placement 18)
  (command "SHOW-WFF&HELP")
  (parent LIB-DISPLAY)
  (mhelp ""))

(defmenuitem SHOW-WFFS-IN-FILE
  (display-name "SHOW-WFFS-IN-FILE")
  (placement 19)
  (command "SHOW-WFFS-IN-FILE")
  (parent LIB-DISPLAY)
  (mhelp ""))

(defmenuitem TEX-ALL-WFFS
  (display-name "TEX-ALL-WFFS")
  (placement 20)
  (command "TEX-ALL-WFFS")
  (parent LIB-DISPLAY)
  (remote-expert T)
  (mhelp ""))

(defmenuitem TEXLIBDIR
  (display-name "TEXLIBDIR")
  (placement 21)
  (command "TEXLIBDIR")
  (parent LIB-DISPLAY)
  (remote-expert T)
  (mhelp ""))

(defmenuitem TEXLIBFILE
  (display-name "TEXLIBFILE")
  (placement 22)
  (command "TEXLIBFILE")
  (parent LIB-DISPLAY)
  (remote-expert T)
  (mhelp ""))

(defmenu STRUCT
  (display-name "Struct")
  (placement 2)
  (parent LIBRARY)
  (mhelp ""))

(defmenuitem COPY-LIBDIR
  (display-name "COPY-LIBDIR")
  (placement 2)
  (command "COPY-LIBDIR")
  (parent STRUCT)
  (remote-expert T)
  (mhelp ""))

(defmenuitem COPY-LIBFILE
  (display-name "COPY-LIBFILE")
  (placement 3)
  (command "COPY-LIBFILE")
  (parent STRUCT)
  (remote-expert T)
  (mhelp ""))

(defmenuitem CREATE-LIB-DIR
  (display-name "CREATE-LIB-DIR")
  (placement 4)
  (command "CREATE-LIB-DIR")
  (parent STRUCT)
  (remote-expert T)
  (mhelp ""))

(defmenuitem CREATE-LIB-SUBDIR
  (display-name "CREATE-LIB-SUBDIR")
  (placement 5)
  (command "CREATE-LIB-SUBDIR")
  (parent STRUCT)
  (remote-expert T)
  (mhelp ""))

(defmenuitem DELETE-LIB-DIR
  (display-name "DELETE-LIB-DIR")
  (placement 6)
  (command "DELETE-LIB-DIR")
  (parent STRUCT)
  (remote-expert T)
  (mhelp ""))

(defmenuitem DELETE-LIBFILE
  (display-name "DELETE-LIBFILE")
  (placement 7)
  (command "DELETE-LIBFILE")
  (parent STRUCT)
  (remote-expert T)
  (mhelp ""))

(defmenuitem MOVE-LIBFILE
  (display-name "MOVE-LIBFILE")
  (placement 8)
  (command "MOVE-LIBFILE")
  (parent STRUCT)
  (remote-expert T)
  (mhelp ""))

(defmenuitem RENAME-LIBDIR
  (display-name "RENAME-LIBDIR")
  (placement 9)
  (command "RENAME-LIBDIR")
  (parent STRUCT)
  (remote-expert T)
  (mhelp ""))

(defmenuitem RENAME-LIBFILE
  (display-name "RENAME-LIBFILE")
  (placement 10)
  (command "RENAME-LIBFILE")
  (parent STRUCT)
  (remote-expert T)
  (mhelp ""))

(defmenuitem UPDATE-LIBDIR
  (display-name "UPDATE-LIBDIR")
  (placement 11)
  (command "UPDATE-LIBDIR")
  (parent STRUCT)
  (remote-expert T)
  (mhelp ""))

(defmenu WRITE
  (display-name "Write")
  (placement 2)
  (parent LIBRARY)
  (remote-expert T)
  (mhelp ""))

(defmenuitem CHANGE-PROVABILITY
  (display-name "CHANGE-PROVABILITY")
  (placement 2)
  (command "CHANGE-PROVABILITY")
  (parent WRITE)
  (remote-expert T)
  (mhelp ""))

(defmenuitem CHECK-NEEDED-OBJECTS
  (display-name "CHECK-NEEDED-OBJECTS")
  (placement 2.5)
  (command "CHECK-NEEDED-OBJECTS")
  (parent LIBRARY)
  (mhelp ""))

(defmenuitem COPY-LIBOBJECT
  (display-name "COPY-LIBOBJECT")
  (placement 4)
  (command "COPY-LIBOBJECT")
  (parent WRITE)
  (remote-expert T)
  (mhelp ""))

(defmenuitem DELETE2
  (display-name "DELETE")
  (placement 5)
  (command "DELETE")
  (parent WRITE)
  (remote-expert T)
  (mhelp ""))

(defmenuitem FIX-MODES
  (display-name "FIX-MODES")
  (placement 6)
  (command "FIX-MODES")
  (parent WRITE)
  (remote-expert T)
  (mhelp ""))

(defmenuitem IMPORT-NEEDED-OBJECTS
  (display-name "IMPORT-NEEDED-OBJECTS")
  (placement 7)
  (command "IMPORT-NEEDED-OBJECTS")
  (parent WRITE)
  (remote-expert T)
  (mhelp ""))

(defmenuitem INSERT
  (display-name "INSERT")
  (placement 8)
  (command "INSERT")
  (parent WRITE)
  (remote-expert T)
  (mhelp ""))

(defmenuitem MOVE-LIBOBJECT
  (display-name "MOVE-LIBOBJECT")
  (placement 9)
  (command "MOVE-LIBOBJECT")
  (parent WRITE)
  (remote-expert T)
  (mhelp ""))

(defmenuitem REFORMAT
  (display-name "REFORMAT")
  (placement 10)
  (command "REFORMAT")
  (parent WRITE)
  (remote-expert T)
  (mhelp ""))

(defmenuitem REINDEX
  (display-name "REINDEX")
  (placement 11)
  (command "REINDEX")
  (parent WRITE)
  (remote-expert T)
  (mhelp ""))

(defmenuitem RENAME-OBJECT
  (display-name "RENAME-OBJECT")
  (placement 12)
  (command "RENAME-OBJECT")
  (parent WRITE)
  (remote-expert T)
  (mhelp ""))

(defmenuitem SORT
  (display-name "SORT")
  (placement 13)
  (command "SORT")
  (parent WRITE)
  (remote-expert T)
  (mhelp ""))

(defmenuitem SPRING-CLEAN
  (display-name "SPRING-CLEAN")
  (placement 14)
  (command "SPRING-CLEAN")
  (parent WRITE)
  (remote-expert T)
  (mhelp ""))

(defmenu KEYWORDS
  (display-name "Keywords")
  (placement 2)
  (parent LIBRARY)
  (mhelp ""))

(defmenuitem ADD-KEYWORD
  (display-name "ADD-KEYWORD")
  (placement 2)
  (command "ADD-KEYWORD")
  (remote-expert T)
  (parent KEYWORDS)
  (mhelp ""))

(defmenuitem CHANGE-KEYWORDS
  (display-name "CHANGE-KEYWORDS")
  (placement 3)
  (command "CHANGE-KEYWORDS")
  (parent KEYWORDS)
  (remote-expert T)
  (mhelp ""))

(defmenuitem SHOW-KEYWORDS
  (display-name "SHOW-KEYWORDS")
  (placement 4)
  (command "SHOW-KEYWORDS")
  (parent KEYWORDS)
  (mhelp ""))

(defmenuitem UPDATE-KEYWORDS
  (display-name "UPDATE-KEYWORDS")
  (placement 5)
  (command "UPDATE-KEYWORDS")
  (parent KEYWORDS)
  (remote-expert T)
  (mhelp ""))

(defmenu BEST-MODES
  (display-name "Best Modes")
  (placement 2)
  (parent LIBRARY)
  (mhelp ""))

(defmenuitem ADD-BESTMODE
  (display-name "ADD-BESTMODE")
  (placement 2)
  (command "ADD-BESTMODE")
  (parent BEST-MODES)
  (remote-expert T)
  (mhelp ""))

(defmenuitem DELETE-BESTMODE
  (display-name "DELETE-BESTMODE")
  (placement 3)
  (command "DELETE-BESTMODE")
  (parent BEST-MODES)
  (remote-expert T)
  (mhelp ""))

(defmenuitem FIND-DUP-MODES
  (display-name "FIND-DUP-MODES")
  (placement 4)
  (command "FIND-DUP-MODES")
  (parent BEST-MODES)
  (mhelp ""))

(defmenuitem MODIFY-BESTMODE
  (display-name "MODIFY-BESTMODE")
  (placement 5)
  (command "MODIFY-BESTMODE")
  (parent BEST-MODES)
  (remote-expert T)
  (mhelp ""))

(defmenuitem SHOW-BESTMODE
  (display-name "SHOW-BESTMODE")
  (placement 6)
  (command "SHOW-BESTMODE")
  (parent BEST-MODES)
  (mhelp ""))

(defmenuitem SHOW-NEW-BESTMODES
  (display-name "SHOW-NEW-BESTMODES")
  (placement 7)
  (command "SHOW-NEW-BESTMODES")
  (parent BEST-MODES)
  (mhelp ""))

(defmenuitem UPDATE-PROVABILITY
  (display-name "UPDATE-PROVABILITY")
  (placement 8)
  (command "UPDATE-PROVABILITY")
  (parent BEST-MODES)
  (remote-expert T)
  (mhelp ""))

(defmenu LIB-CLASS
  (display-name "Lib Class")
  (placement 2)
  (parent LIBRARY)
  (mhelp ""))

(defmenuitem CLASSIFY-CLASS
  (display-name "CLASSIFY-CLASS")
  (placement 2)
  (command "CLASSIFY-CLASS")
  (parent LIB-CLASS)
  (mhelp ""))

(defmenuitem CLASSIFY-ITEM
  (display-name "CLASSIFY-ITEM")
  (placement 3)
  (command "CLASSIFY-ITEM")
  (parent LIB-CLASS)
  (mhelp ""))

(defmenuitem CREATE-CLASS-SCHEME
  (display-name "CREATE-CLASS-SCHEME")
  (placement 4)
  (command "CREATE-CLASS-SCHEME")
  (parent LIB-CLASS)
  (mhelp ""))

(defmenuitem CREATE-LIBCLASS
  (display-name "CREATE-LIBCLASS")
  (placement 5)
  (command "CREATE-LIBCLASS")
  (parent LIB-CLASS)
  (mhelp ""))

(defmenuitem FETCH-DOWN
  (display-name "FETCH-DOWN")
  (placement 6)
  (command "FETCH-DOWN")
  (parent LIB-CLASS)
  (mhelp ""))

(defmenuitem FETCH-LIBCLASS
  (display-name "FETCH-LIBCLASS")
  (placement 7)
  (command "FETCH-LIBCLASS")
  (parent LIB-CLASS)
  (mhelp ""))

(defmenuitem FETCH-LIBCLASS*
  (display-name "FETCH-LIBCLASS*")
  (placement 8)
  (command "FETCH-LIBCLASS*")
  (parent LIB-CLASS)
  (mhelp ""))

(defmenuitem FETCH-UP
  (display-name "FETCH-UP")
  (placement 9)
  (command "FETCH-UP")
  (parent LIB-CLASS)
  (mhelp ""))

(defmenuitem GOTO-CLASS
  (display-name "GOTO-CLASS")
  (placement 10)
  (command "GOTO-CLASS")
  (parent LIB-CLASS)
  (mhelp ""))

(defmenuitem PCLASS
  (display-name "PCLASS")
  (placement 11)
  (command "PCLASS")
  (parent LIB-CLASS)
  (mhelp ""))

(defmenuitem PCLASS-SCHEME-TREE
  (display-name "PCLASS-SCHEME-TREE")
  (placement 12)
  (command "PCLASS-SCHEME-TREE")
  (parent LIB-CLASS)
  (mhelp ""))

(defmenuitem PCLASS-TREE
  (display-name "PCLASS-TREE")
  (placement 13)
  (command "PCLASS-TREE")
  (parent LIB-CLASS)
  (mhelp ""))

(defmenuitem PINTERSECT
  (display-name "PINTERSECT")
  (placement 14)
  (command "PINTERSECT")
  (parent LIB-CLASS)
  (mhelp ""))

(defmenuitem PINTERSECT*
  (display-name "PINTERSECT*")
  (placement 15)
  (command "PINTERSECT*")
  (parent LIB-CLASS)
  (mhelp ""))

(defmenuitem PSCHEMES2
  (display-name "PSCHEMES")
  (placement 16)
  (command "PSCHEMES")
  (parent LIB-CLASS)
  (mhelp ""))

(defmenuitem ROOT-CLASS
  (display-name "ROOT-CLASS")
  (placement 17)
  (command "ROOT-CLASS")
  (parent LIB-CLASS)
  (mhelp ""))

(defmenuitem UNCLASSIFY-CLASS
  (display-name "UNCLASSIFY-CLASS")
  (placement 18)
  (command "UNCLASSIFY-CLASS")
  (parent LIB-CLASS)
  (mhelp ""))

(defmenuitem UNCLASSIFY-ITEM
  (display-name "UNCLASSIFY-ITEM")
  (placement 19)
  (command "UNCLASSIFY-ITEM")
  (parent LIB-CLASS)
  (mhelp ""))

(defmenuitem LEAVE2
  (display-name "Leave")
  (placement 300)
  (command "Leave")
  (parent LIBRARY)
  (mhelp ""))

(defmenu LIBRARY-FLAGS
  (display-name "Library Flags")
  (placement 2)
  (parent FLAGS)
  (mhelp ""))

(defmenuitem DEFAULT-BUG-DIR
  (display-name "DEFAULT-BUG-DIR")
  (placement 2)
  (command "DEFAULT-BUG-DIR")
  (parent LIBRARY-FLAGS)
  (mhelp ""))

(defmenuitem USE-DEFAULT-BUG-DIR
  (display-name "USE-DEFAULT-BUG-DIR")
  (placement 3)
  (command "USE-DEFAULT-BUG-DIR")
  (parent LIBRARY-FLAGS)
  (mhelp ""))

(defmenuitem CLASS-DIRECTION
  (display-name "CLASS-DIRECTION")
  (placement 4)
  (command "CLASS-DIRECTION")
  (parent LIBRARY-FLAGS)
  (mhelp ""))

(defmenuitem CLASS-SCHEME
  (display-name "CLASS-SCHEME")
  (placement 5)
  (command "CLASS-SCHEME")
  (parent LIBRARY-FLAGS)
  (mhelp ""))

(defmenuitem ADD-SUBDIRECTORIES
  (display-name "ADD-SUBDIRECTORIES")
  (placement 6)
  (command "ADD-SUBDIRECTORIES")
  (parent LIBRARY-FLAGS)
  (mhelp ""))

(defmenuitem BACKUP-LIB-DIR
  (display-name "BACKUP-LIB-DIR")
  (placement 7)
  (command "BACKUP-LIB-DIR")
  (parent LIBRARY-FLAGS)
  (mhelp ""))

(defmenuitem DEFAULT-LIB-DIR
  (display-name "DEFAULT-LIB-DIR")
  (placement 8)
  (command "DEFAULT-LIB-DIR")
  (parent LIBRARY-FLAGS)
  (mhelp ""))

(defmenuitem DEFAULT-LIBFILE-TYPE
  (display-name "DEFAULT-LIBFILE-TYPE")
  (placement 9)
  (command "DEFAULT-LIBFILE-TYPE")
  (parent LIBRARY-FLAGS)
  (mhelp ""))

(defmenuitem DEFAULT-LIBINDEX-TYPE
  (display-name "DEFAULT-LIBINDEX-TYPE")
  (placement 10)
  (command "DEFAULT-LIBINDEX-TYPE")
  (parent LIBRARY-FLAGS)
  (mhelp ""))

(defmenuitem LIB-BESTMODE-FILE
  (display-name "LIB-BESTMODE-FILE")
  (placement 11)
  (command "LIB-BESTMODE-FILE")
  (parent LIBRARY-FLAGS)
  (mhelp ""))

(defmenuitem LIB-KEYWORD-FILE
  (display-name "LIB-KEYWORD-FILE")
  (placement 12)
  (command "LIB-KEYWORD-FILE")
  (parent LIBRARY-FLAGS)
  (mhelp ""))

(defmenuitem LIB-MASTERINDEX-FILE
  (display-name "LIB-MASTERINDEX-FILE")
  (placement 13)
  (command "LIB-MASTERINDEX-FILE")
  (parent LIBRARY-FLAGS)
  (mhelp ""))

(defmenuitem RECORDFLAGS
  (display-name "RECORDFLAGS")
  (placement 14)
  (command "RECORDFLAGS")
  (parent LIBRARY-FLAGS)
  (mhelp ""))

(defmenuitem REMOVE-TRAILING-DIR
  (display-name "REMOVE-TRAILING-DIR")
  (placement 15)
  (command "REMOVE-TRAILING-DIR")
  (parent LIBRARY-FLAGS)
  (mhelp ""))

(defmenuitem SHOW-ALL-LIBOBJECTS
  (display-name "SHOW-ALL-LIBOBJECTS")
  (placement 16)
  (command "SHOW-ALL-LIBOBJECTS")
  (parent LIBRARY-FLAGS)
  (mhelp ""))

(defmenu LIB
  (display-name "Lib")
  (placement 2)
  (parent MISC-COMMANDS)
  (mhelp ""))

(defmenuitem BUG-DELETE
  (display-name "BUG-DELETE")
  (placement 2)
  (command "BUG-DELETE")
  (parent LIB)
  (remote-expert T)
  (mhelp ""))

(defmenuitem BUG-HELP
  (display-name "BUG-HELP")
  (placement 3)
  (command "BUG-HELP")
  (parent LIB)
  (mhelp ""))

(defmenuitem BUG-LIST
  (display-name "BUG-LIST")
  (placement 4)
  (command "BUG-LIST")
  (parent LIB)
  (mhelp ""))

(defmenuitem BUG-RESTORE
  (display-name "BUG-RESTORE")
  (placement 5)
  (command "BUG-RESTORE")
  (parent LIB)
  (mhelp ""))

(defmenuitem BUG-SAVE
  (display-name "BUG-SAVE")
  (placement 6)
  (command "BUG-SAVE")
  (parent LIB)
  (remote-expert T)
  (mhelp ""))

(defmenuitem PSCHEMES
  (display-name "PSCHEMES")
  (placement 7)
  (command "PSCHEMES")
  (parent LIB)
  (mhelp ""))

(defmenuitem DATEREC
  (display-name "DATEREC")
  (placement 8)
  (command "DATEREC")
  (parent LIB)
  (remote-expert T)
  (mhelp ""))

(defmenuitem MODEREC
  (display-name "MODEREC")
  (placement 9)
  (command "MODEREC")
  (parent LIB)
  (remote-expert T)
  (mhelp ""))

(defmenuitem DISPLAYFILE
  (display-name "DISPLAYFILE")
  (placement 10)
  (command "DISPLAYFILE")
  (parent LIB)
  (mhelp ""))

(defmenuitem LIB-ABBR
  (display-name "LIB-ABBR")
  (placement 9)
  (command "LIB-ABBR")
  (parent ABBREV-OPS)
  (mhelp ""))

(defmenu LIB-READ
  (display-name "Lib Read")
  (placement 2)
  (parent LIBRARY)
  (mhelp ""))

(defmenuitem DESTROY
  (display-name "DESTROY")
  (placement 2)
  (command "DESTROY")
  (parent LIB-READ)
  (mhelp ""))

(defmenuitem FETCH
  (display-name "FETCH")
  (placement 3)
  (command "FETCH")
  (parent LIB-READ)
  (mhelp ""))

(defmenuitem FIND-PROVABLE
  (display-name "FIND-PROVABLE")
  (placement 4)
  (command "FIND-PROVABLE")
  (parent LIB-READ)
  (mhelp ""))

(defmenuitem RESTORE-MASTERINDEX
  (display-name "RESTORE-MASTERINDEX")
  (placement 5)
  (command "RESTORE-MASTERINDEX")
  (parent LIB-READ)
  (mhelp ""))

(defmenuitem RETRIEVE-FILE
  (display-name "RETRIEVE-FILE")
  (placement 6)
  (command "RETRIEVE-FILE")
  (parent LIB-READ)
  (mhelp ""))
