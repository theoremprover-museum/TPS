# This makefile can be used to build a version of TPS or ETPS.
# To compile and build a version of TPS, enter
# % make tps
# To compile and build a version of ETPS, enter
# % make etps
# To just build a version of TPS, enter
# % make tps_build
# To just build a version of ETPS, enter
# % make etps_build
# Likewise you can call make with the argument tps_compile and etps_compile,
# which will compile the files for TPS and ETPS, respectively, but not
# construct the core image.

# The scripts tps-build, tps-compile, etps-build and etps-compile are
# provided to make this process even easier.  They merely call make with
# the proper arguments.  Any arguments you give them will be merely
# passed on the make.

# When you compile files, this makefile will cause
# to be created a file called tps3.sys or etps.sys, depending on which
# system you are compiling.  This file contains information which depends
# on the values of the parameters in this file, and is used in the compilation 
# and build processes.  You should never need to modify these files, just  
# this makefile and the .depend file.

# If you wish to set up TPS or ETPS in a particular way (with a special value
# for some TPS flag, say), then use the tps3.ini and etps.ini files for that
# purpose.  For example, we have (setq expertflag t) in our tps3.ini file,
# but (setq expertflag nil) in the etps.ini file. 

# We assume that you have set up your directory structure as it comes in the 
# TPS distribution.  Changing that would spell disaster here.

#-----------------------------------------------------------------------
# PARAMETERS YOU MIGHT WANT TO EDIT ARE GIVEN BELOW
#-----------------------------------------------------------------------

# The name of the lisp that will be used is in the file .depend, which should
# be edited before making TPS or ETPS.
# Generally, users who are compiling TPS on several versions of lisp will find 
# that they can have a standard Makefile for all their lisps, and a different
# .depend file for each.
# However, some operating systems seem to execute a Makefile
# without reading the .depend file, in which case 
# the information in the .depend file should be placed here
# (uncommented and adjusted to the way you start up your lisp) 
# as in this example:
#lisp = cl
#lisp = sbcl
#lisp = /opt/cmucl-20b/bin/cmucl20b
lisp = lisp
java = java
javac = $(java)c

# If your lisp is not in the following list, put a string value here for the
# extension to be used for compiled files ("fasl" will be used if you don't).
# Be sure to escape the quotation marks with backslashes if you do, e.g., 
# compiled-extension = \"pmaxf\".
# The following looks goofier than it should, because some make programs 
# can't handle a # character in a macro definition.

compiled-extension = (or (if (member :lucid *features*) \"lbin\") (if (or (member (find-symbol \"KCL\") *features*) (member (find-symbol \"IBCL\") *features*)) \"o\") (if (member :cmu *features*) \"fasl\") (if (member :sbcl *features*) \"fasl\") (if (member :excl *features*) \"fasl\") (if (member :allegro *features*) \"fasl\") \"fasl\")

# mail-remarks should either be nil, if you don't want remarks mailed to 
# anyone, or should be a string with the addresses of the people to whom
# mail should be sent, e.g., \"andrews@cs.cmu.edu nesmith@cs.cmu.edu\"
# Be sure to escape the quotation marks with backslashes.

mail-remarks = nil

# If you want remarks to be sent to a file instead of being mailed, then 
# set the following flag to the file name.  No need to put this in quotes.
# This file will be put in the sys-dir.

remarks-file = tps3.remarks

# The following is a list of those people who have EXPERTFLAG set to T.
# These people are allowed to evaluate any Lisp form, and may have a 
# tps3.ini file loaded on startup. Don't forget to escape all the 
# quotation marks with backslashes.

experts = (\"andrews\" \"pfenning\" \"pa01\" \"cebrown\" \"fp\"  \"mathweb\"  \"theorem\"  \"pmckenne\"  \"remyc\")


# This is where the saved core image should be put.  You can also specify
# a specific file by e.g,  tps-save-file = /usr/tmp/tps3.  Note that some
# lisps warn against saving a core image over a NFS-mounted file system,
# so here you might want to put something like 
# tps-save-file = /usr/tmp/$(tps-core-name)
# You also might want to do this to keep the new TPS from overwriting 
# your old version.  No need to put this in quotes.

tps-save-file = $(sys-dir)$(tps-core-name)
etps-save-file = $(sys-dir)$(etps-core-name)

# Add any lisp forms here that you want to have loaded when TPS/ETPS is built.
# An example we use is sys-file-extras = (pushnew :andrew *features*) when
# we are building on the Andrew system.
# Escape any double quotes with a backslash

sys-file-extras = 

#-----------------------------------------------------------------------
# DON'T CHANGE ANYTHING BELOW THIS.
#-----------------------------------------------------------------------

# This is the current directory.  No need to set this by hand.

sys-dir = `pwd`/

# The rootname of the .patch, .news, and .ini files.  Don't see any 
# reason you'd really want to change these.  If you did, you'd have 
# to change the names of those files.

building-start-time = \"`date`\"
tps-core-name = tps3
etps-core-name = etps


tps:	tps_compile tps_build java_compile

etps:	etps_compile etps_build java_compile

tps_compile:	bin lisp tps-compile.lisp make_tps_sys 
	@date
	@/bin/sh -ec 'echo "(load \"../tps-compile.lisp\") (core:exit-from-lisp)"' | (cd bin; $(lisp))

tps_build:	bin lisp tps-build.lisp tps3.sys 
	@date
	@/bin/sh -ec 'echo "(progn (load \"../tps-build.lisp\") (eval (read-from-string \"(tps3-save)\"))) (core:exit-from-lisp)"' | (cd bin; $(lisp))

etps_compile:	bin lisp etps-compile.lisp make_etps_sys 
	@date
	@/bin/sh -ec 'echo "(load \"../etps-compile.lisp\") (core:exit-from-lisp)"' | (cd bin; $(lisp))

etps_build:	bin lisp etps-build.lisp etps.sys
	@date
	@/bin/sh -ec 'echo "(progn (load \"../etps-build.lisp\") (eval (read-from-string \"(tps3-save)\"))) (core:exit-from-lisp)"' | (cd bin; $(lisp))

java_compile:  java_compile_tps java
	(cd java; $(javac) *.java)

java_compile_tps:  java/tps
	(cd java/tps; $(javac) *.java)

tps3.sys: make_tps_sys
etps.sys: make_etps_sys


clean:
	rm -i *.sys

very_clean: 	clean
	rm -f bin/*



#This constructs the tps3.sys file for use in compiling and building.
make_tps_sys:
	@echo Making new tps3.sys.
	@rm -f tps3.sys
	@echo  "(in-package :cl-user)\
	        (defvar core-name \"$(tps-core-name)\")\
                (defvar sys-dir \"$(sys-dir)\")\
                (defvar save-file \"$(tps-save-file)\")\
                (defvar compiled-extension $(compiled-extension))\
                (defvar mail-remarks $(mail-remarks))\
                (defvar remarks-file \"$(remarks-file)\")\
                (defvar news-dir \"$(sys-dir)\")\
                (defvar patch-file-dir \"$(sys-dir)\") \
                (defvar ini-file-dir \"$(sys-dir)\")\
                (defvar compiled-dir  \"$(sys-dir)bin/\")\
		(defconstant expert-list '$(experts))\
		(defconstant building-start-time $(building-start-time))\
                (defvar source-dir \"$(sys-dir)lisp/\")\
                (defvar source-path (list compiled-dir source-dir))\
		(defvar java-comm \"cd $(sys-dir)java/ ; $(java) TpsStart\")\
                (defvar source-extension \"lisp\")\
                (pushnew :TPS *features*)" >> tps3.sys
	@echo "#+lucid (progn (push compiled-extension *load-binary-pathname-types*) (push source-extension *load-source-pathname-types*)  (setq *ignore-binary-dependencies* t)  (proclaim '(optimize (compilation-speed 1) (speed 3) (safety 3))))" >> tps3.sys
	@echo ";;; Following sets up CTRL-G as interrupt character in CMU Lisp" >> tps3.sys
	@echo ";;; Preambles used when printing proofs to a file in scribe or tex format" >> tps3.sys
	@echo "(setq *print-case* :upcase) \
              #+ibcl(setq si::*default-time-zone* 5)\
              #+ibcl(setq si::*notify-gbc* t)\
              #+ibcl(setq si::*ignore-eof-on-terminal-io* t)\
              #+:excl(setq *gcprint* nil)\
              #+:allegro(setq excl:*global-gc-behavior* :auto)\
              #+:cmu(ext:unlock-all-packages)" >> tps3.sys
	@echo "$(sys-file-extras)" >> tps3.sys

#This constructs the etps.sys file for use in compiling and building.
make_etps_sys:
	@echo Making new etps.sys.
	@rm -f etps.sys
	@echo  "(in-package :cl-user)\
	        (defvar core-name \"$(etps-core-name)\")\
                (defvar sys-dir \"$(sys-dir)\")\
                (defvar save-file \"$(etps-save-file)\")\
                (defvar compiled-extension $(compiled-extension))\
                (defvar mail-remarks $(mail-remarks))\
                (defvar remarks-file \"$(remarks-file)\")\
                (defvar news-dir \"$(sys-dir)\")\
                (defvar patch-file-dir \"$(sys-dir)\") \
                (defvar ini-file-dir \"$(sys-dir)\")\
                (defvar compiled-dir  \"$(sys-dir)bin/\")\
		(defconstant expert-list '$(experts))\
		(defconstant building-start-time $(building-start-time))\
                (defvar source-dir \"$(sys-dir)lisp/\")\
                (defvar source-path (list compiled-dir source-dir))\
		(defvar java-comm \"cd $(sys-dir)java/ ; $(java) TpsStart\")\
                (defvar source-extension \"lisp\")\
                (pushnew :ETPS *features*)" >> etps.sys
	@echo "#+lucid (progn (push compiled-extension *load-binary-pathname-types*) (push source-extension *load-source-pathname-types*)  (setq *ignore-binary-dependencies* t)  (proclaim '(optimize (compilation-speed 1) (speed 3) (safety 3))))" >> etps.sys
	@echo ";;; Following sets up CTRL-G as interrupt character in CMU Lisp" >> etps.sys
	@echo ";;; Preambles used when printing proofs to a file in scribe or tex format" >> etps.sys
	@echo "(setq *print-case* :upcase) \
              #+ibcl(setq si::*default-time-zone* 5)\
              #+ibcl(setq si::*notify-gbc* t)\
              #+ibcl(setq si::*ignore-eof-on-terminal-io* t)\
              #+:excl(setq *gcprint* nil)\
              #+:allegro(setq excl:*global-gc-behavior* :auto)\
              #+:cmu(ext:unlock-all-packages)" >> etps.sys
	@echo "$(sys-file-extras)" >> etps.sys

