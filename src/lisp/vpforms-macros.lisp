;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of VPFORMS)

(deffile vpforms-macros
  (part-of vpforms)
  (extension clisp)
  (mhelp "VPFORM Macro file."))

(context jforms1)

(defflag vpd-lit-name
  (flagtype symbol)
  (default v)
  (subjects jforms) 
(mhelp "Prefix for labels associated with literals when VP diagrams are
created automatically within the editor."))

(defflag printvpdflag
  (flagtype boolean)
  (default nil)
  (subjects jforms editor)
  (mhelp "If T, vertical path diagrams are written into the VPD-FILENAME
whenever wffs are written into the PRINTEDTFILE.  In particular PRINTEDTFLAG
must be T, for the automatic writing to take place."))

(defflag vpd-filename
  (flagtype filespec)
  (default "vpd.vpf")
  (subjects jforms)
  (mhelp "Default filename when printing VP diagrams in a file."))

(defflag vpd-ptypes
  (flagtype boolean)
  (default T)
  (subjects jforms)
  (mhelp "If T, print types when printing VP diagrams in a file."))

(defflag vpd-brief
  (flagtype vpformat)
  (default L)
  (subjects jforms)
  (mhelp "The default value for BRIEF when printing VP diagrams in a file.
Currently the options are:
T   = no atom values will show in VP diagram
A   = atom values but no labels will appear in VP diagram
NIL = atom values and labels will show in VP diagram
LT  = atom values and  labels and a legend will show in VP diagram 
L   = labels but no atom values will show in VP diagram,
      and a legend will show both
B   = boxed labels and atoms will show in the VP diagram.
BT  = boxed labels will show in the diagram, and the atom values
      will be listed below.
B and BT only work in TeX format (i.e. with the VPT command)."))

(defflag vpd-vpfpage
  (flagtype posinteger)
  (default 78)
  (subjects jforms)
  (mhelp
 "The default value for the width of the page when printing VP diagrams in a file."))

(defflag vpd-style
  (flagtype vpstyle)
  (default generic)
  (subjects jforms)
  (mhelp "The default value for STYLE when printing VP diagrams in a file."))

(defflag vpform-labels
  (flagtype boolean)
  (default nil)
  (subjects jforms)
  (mhelp "In the editor, a value of T for this flag will suppress
printing of labels in vpforms; if it is NIL, labels and atom values
will be printed.
If this flag is set the default value for argument BRIEF will
be A. Unless one decides to override the default value, labels will not be
printed.  This flag has no effect on the editor command VPD, and on the
wffop DISPLAY-VPD.  To suppress labels when using these commands, please
set the flag VPD-BRIEF to A."))

