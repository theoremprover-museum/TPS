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
;;; File: LIB-OPS
;;; Package: library
;;;

(deffile lib-ops
  (part-of library)
  (extension lisp)
  (mhelp "Defines LIBRARY operations."))

(defun initialise-assumed ()
  (if remove-trailing-dir
      (do* ((dirs (append default-lib-dir backup-lib-dir))
	    (shortest (if dirs (1- (reduce 'min (mapcar 'length dirs))) 0))
	    (string-so-far "" (concatenate 'string string-so-far (princ-to-string (elt (car dirs) count))))
	    (count 0 (1+ count)))
	   ((or (zerop shortest)
		(> count shortest)
		(> (length (remove-duplicates (mapcar #'(lambda (x) (elt x count)) dirs) :test 'equal)) 1))
	    (setq assumed-initial-dir string-so-far)))
    (setq assumed-initial-dir "")))

(defun pfile (name &optional (libstyle style))
  (if (or (eq libstyle 'write-to-file)
	  (not (string= assumed-initial-dir name :end2 (length assumed-initial-dir))))
      name
    (subseq name (length assumed-initial-dir) (length name))))

(defun complete-lib-filename (filename &optional (type default-libfile-type) (silent nil))
  (let* ((fname (if (symbolp filename) (string-downcase (princ-to-string filename)) filename))
	 (temp-fname (merge-pathnames fname
				      (make-pathname% :directory (car default-lib-dir) :type type))))
    (unless (probe-file temp-fname)
	    (setq temp-fname
		  (dolist (direc (append (cdr default-lib-dir) backup-lib-dir) nil)
			  (let ((f1 (merge-pathnames fname 
						     (make-pathname% :directory direc :type type))))
			    (when (probe-file f1) (return f1))))))
    (unless (or temp-fname silent) (msgf "Can't find the file " filename t))
    temp-fname))

(defun complete-lib-filename-choose (filename &optional (type default-libfile-type) (writeable nil))
  (let* ((fname (if (symbolp filename) (string-downcase (princ-to-string filename)) filename))
	 (temp-fname (merge-pathnames fname
				      (make-pathname% :directory (car default-lib-dir) :type type)))
	 (existing-names nil)
	 (count 0))
    (when (probe-file temp-fname) (push temp-fname existing-names))
    (unless (string= (directory-namestring filename) (directory-namestring temp-fname))
	    (dolist (direc (if writeable (cdr default-lib-dir)
			     (append (cdr default-lib-dir) backup-lib-dir)))
		    (let ((f1 (merge-pathnames fname 
					       (make-pathname% :directory direc :type type))))
		      (when (probe-file f1) (push f1 existing-names))))
	    (setq existing-names (reverse existing-names))
	    (when (cdr existing-names)
		  (msgf (length existing-names) " matching filenames have been found." t)
		  (dolist (elt existing-names)
			  (incf count)
			  (msgf count ") " (pfile (namestring elt)) t))
		  (incf count)
		  (msgf count ") None of these." t)
		  (setq existing-names (list (nth (1- (get-a-number count)) existing-names)))))
    (if (null (car existing-names))
	(throwfail t "Can't find the file " filename (if writeable " in DEFAULT-LIB-DIR." "."))
      (car existing-names))))

(defun get-lib-printfn (type)
  (or (get type 'lib-printfn nil)
      #'(lambda (item libstyle) (declare (ignore libstyle)) (prin1 item))))

(defun get-lib-descr-readfn (type)
  (or (get type 'lib-descr-readfn) #'(lambda (object) object)))

(defun get-lib-attr-readfn (type)
  (or (get type 'lib-attr-readfn) #'(lambda (object) object)))

(defun make-lib-tpsobject (type)
  (or (get type 'lib-tpsobject)
      (throwfail "Cannot retrieve items of type: " type
		 ". Please notify your system maintainer.")))

(defun locate-item (name &key (type nil) (multiple show-all-libobjects) (preferred-dir nil) (writeable nil))
  (declare (special *lib-masterindex*))
  (let* ((value (gethash name *lib-masterindex*)) ;NIL isn't a valid value, so we can do this.
	 (dld (mapcar 'directory-namestring default-lib-dir))
	 (bld (mapcar 'directory-namestring backup-lib-dir))
	 (pd (and preferred-dir (directory-namestring preferred-dir)))
	 (count 0)
	 (ds (mapcar #'(lambda (x) (incf count) (cons x count)) 
		     (append
		      (if pd 
			  (cons pd (append dld bld))
			(append dld bld))
		      (mapcar #'(lambda (x)   ; appended this to the end in case the master index
				  (directory-namestring ; knows the object is in a library directory
				   (cdr x)))  ; not listed in default- or backup-lib-dir  - cebrown 8/4/99
			      value)))))
    (when value
	  (when type
		(setq value (remove-if #'(lambda (x) (neq (car x) type)) value)))
	  (when writeable 
		(setq value (remove-if-not #'(lambda (x) (member (directory-namestring (cdr x)) dld
								 :test 'string=))
					   value)))
	  (setq value (sort value #'(lambda (x y) (< (cdr (assoc (directory-namestring (cdr x)) ds :test 'string=))
						     (cdr (assoc (directory-namestring (cdr y)) ds :test 'string=))))))
	  (unless multiple (setq value (one-of-each-type value)))
	  (if (dolist (elt value t)
		      (unless (probe-file (cdr elt)) (return nil)))
	      value
	    (progn (restore-lib-hashtable) 
		   (locate-item name :type type :multiple multiple :preferred-dir preferred-dir :writeable writeable))))))

(defun one-of-each-type (l &optional (types-found nil))
  (when l
	(if (memq (caar l) types-found) 
	    (one-of-each-type (cdr l) types-found)
	  (cons (car l) (one-of-each-type (cdr l) (cons (caar l) types-found))))))

(defun restore-lib-hashtable (&optional (silent nil))
  (declare (special *lib-masterindex*))
  (unless (gethash 'dont-hash *lib-masterindex*) ; if dont-hash is set, we're delaying building the lib hash table,
					; eg, during initialization we may set default-lib-dir & backup-lib-dir
					; in the ini files, and then set *lib-masterindex* afterwards - cebrown 5/24/02
    (clrhash *lib-masterindex*)
    (clrhash lib-bestmodes)
    #-(or :allegro :sbcl :cmu :lucid :kcl :clisp)(setq add-subdirectories nil)
    (when add-subdirectories (find-subdirs))
    (setq default-lib-dir (remove-duplicates (mapcar 'directory-namestring default-lib-dir) :test 'string=))
    (setq backup-lib-dir (remove-if #'(lambda (x) (member x default-lib-dir :test 'string=)) 
				    (remove-duplicates (mapcar 'directory-namestring backup-lib-dir) :test 'string=)))
    #-:clisp(setq default-lib-dir (remove-if-not 'probe-file default-lib-dir))
    (dolist (direc default-lib-dir)
      (let ((p (make-pathname% :directory direc :name (namestring (pathname-name lib-masterindex-file))
			       :type default-libindex-type))
	    (q (make-pathname% :directory direc :name (namestring (pathname-name lib-keyword-file))
			       :type default-libindex-type))
	    (r (make-pathname% :directory direc :name (namestring (pathname-name lib-bestmode-file))
			       :type default-libindex-type)))
	(unless (probe-file p)
	  (reroute-output p *default-pathname-defaults* (msg t)))
	(unless (probe-file r) 
	  (when (string= direc (car default-lib-dir))
	    (reroute-output r *default-pathname-defaults* (msg t))))
	(unless (probe-file q)
	  (reroute-output q *default-pathname-defaults* (msg lib-keyword-list t)))))
    (setq backup-lib-dir (remove-if-not 
			  #'(lambda (x) (probe-file 
					 (make-pathname% :directory x :name (namestring (pathname-name lib-masterindex-file))
							 :type default-libindex-type)))
			  backup-lib-dir))
    (unless (or (string= (initialise-assumed) "") silent (not load-warn-p))
      (msgf "All library directories begin with " assumed-initial-dir t))
    (let* ((def-common-prefix (or (car default-lib-dir) ""))
	   (bkp-common-prefix (or (car backup-lib-dir) ""))
	   (ndef (length def-common-prefix))
	   (nbkp (length bkp-common-prefix))
	   (deflibclass (make-libclass :name 'CL-USER::DEFAULT))
	   (bkplibclass (make-libclass :name 'CL-USER::BACKUP))
	   (rootlibclass (make-libclass :name 'CL-USER::LIBDIR
					:kids (list deflibclass bkplibclass)))
	   (fakesym (gensym)))
      (setf (get fakesym 'libclass) rootlibclass)
      (eval (list 'def-class-scheme 'CL-USER::LIBDIR
		  (list 'class-direction class-direction)
		  (list 'libclass fakesym)
		  '(mhelp "LIBDIR is a classification scheme built based purely on the
directory structure of the library directories in DEFAULT-LIB-DIR
and BACKUP-LIB-DIR.  Other classification schemes may be stored in
and retrieved from the library.

See Also:  UNIXLIB, DEFAULT-LIB-DIR, BACKUP-LIB-DIR")))
      (setf (libclass-parents bkplibclass) (list rootlibclass))
      (setf (libclass-parents deflibclass) (list rootlibclass))
      (dolist (libdir (cdr default-lib-dir))
	(if (< (length libdir) ndef)
	    (setq ndef (length libdir))
	  (do ((i 0 (1+ i)))
	      ((or (= i ndef) (neq (aref def-common-prefix i) (aref libdir i)))
	       (setq ndef i)))))
      (dolist (libdir (cdr backup-lib-dir))
	(if (< (length libdir) nbkp)
	    (setq nbkp (length libdir))
	  (do ((i 0 (1+ i)))
	      ((or (= i nbkp) (neq (aref bkp-common-prefix i) (aref libdir i)))
	       (setq nbkp i)))))
      (dolist (direc (reverse (append default-lib-dir backup-lib-dir)))
	(let ((lib-keyword-file 
	       (make-pathname% :name (file-namestring (pathname-name (pathname lib-keyword-file)))
			       :type default-libindex-type
			       :directory direc)))
	  (when (probe-file lib-keyword-file)
	    (unless (or silent (not load-warn-p)) 
	      (msgf "Restoring library keywords, ")); (pfile (directory-namestring lib-keyword-file))))
	    (with-open-file (*input-stream* lib-keyword-file :direction :input)
			    (do ((item nil (read *input-stream* nil control-d-char)))
				((eq item control-d-char))
			      (when item 
				(setq lib-keyword-list 
				      (remove-duplicates 
				       (append 
					(mapcar #'(lambda (x) (cons (car x) (concatenate 'string "\"" (cdr x) "\""))) item)
					lib-keyword-list) :key 'car)))))))
	(let ((lib-bestmode-file 
	       (make-pathname% :name (file-namestring (pathname-name (pathname lib-bestmode-file)))
			       :type default-libindex-type
			       :directory direc)))
	  (when (probe-file lib-bestmode-file)
	    (unless (or silent (not load-warn-p)) 
	      (msg "best modes, ")); (pfile (directory-namestring lib-bestmode-file))))
	    (with-open-file (*input-stream* lib-bestmode-file :direction :input)
			    (do ((item nil (read *input-stream* nil control-d-char)))
				((eq item control-d-char))
			      (when item 
				(stuff-bestmodes-into-hashtable item 
								(if (member direc default-lib-dir :test #'string=) t nil)))))))
	(let ((lib-masterindex-file 
	       (make-pathname% :name (file-namestring (pathname-name (pathname lib-masterindex-file)))
			       :type default-libindex-type
			       :directory direc)))
	  (when (probe-file lib-masterindex-file)
	    (unless (or silent (not load-warn-p)) 
	      (msg "index ... " (pfile (directory-namestring lib-masterindex-file))))
	    (with-open-file
	     (*input-stream* lib-masterindex-file :direction :input)
	     (let ((cl (find-or-create-libclass
			(if (member direc default-lib-dir)
			    (cons 'CL-USER::DEFAULT (stringpath-to-symbollist direc ndef))
			  (cons 'CL-USER::BACKUP (stringpath-to-symbollist direc nbkp)))
			rootlibclass)))
	       (do ((item nil (read *input-stream* nil control-d-char))
		    (exists nil))
		   ((eq item control-d-char))
		 (when item 
		   (let* ((filename (file-namestring 
				     (pathname-name (pathname (princ-to-string (cddr item))))))
			  (newitem 
			   (make-pathname% :directory direc :name filename
					   :type default-libfile-type))
			  (clf (find-or-create-libclass
				(list (intern-str (string-upcase filename))) cl)))
		     (unless (member (cddr item) exists :test 'string-equal)
		       (if (probe-file newitem) (push (cddr item) exists) (setq newitem nil)))
		     (if newitem
			 (progn
			   (setf (libclass-libitems clf)
				 (adjoin (car item) (libclass-libitems clf)))
			   (pushnew (cons (cadr item) newitem)
				    (gethash (car item) *lib-masterindex* nil) 
				    :key 'cdr :test 'equal))
		       (when (and (not silent) load-warn-p
				  (member (namestring direc) default-lib-dir :test 'string=))
					;no point warning about dirs you can't write to...
			 (msgf "Can't find " (namestring direc) (cddr item) t
			       "   which supposedly contains " (car item) "." t))))))))))))
    (when (car default-lib-dir) (shuffle-backups))
    (unless (or silent (not load-warn-p)) (msgf "done." t))))

(defun stringpath-to-symbollist (str i)
  (let ((ret nil)
	(tempstr "")
	(n (length str)))
    (do ((j i (1+ j)))
	((>= j n)
	 (when (> (length tempstr) 0)
	   (push (intern-str (string-upcase tempstr)) ret))
	 (reverse ret))
	(if (memq (aref str j) '(#\/ #\\))
	    (when (> (length tempstr) 0)
	      (push (intern-str (string-upcase tempstr)) ret)
	      (setq tempstr ""))
	  (setq tempstr (format nil "~d~d" tempstr (aref str j)))))))
	     

#+:mswindows
(defun get-directories-below (dir)
  (declare (ignore dir))
  "Under Windows, Users Must Explicitly List Library Subdirectories
in DEFAULT-LIB-DIR BACKUP-LIB-DIR"
  nil)

#-:mswindows
(defun get-directories-below (dir)
  (let* ((junkfile (concatenate 'string "/tmp/directory" (princ-to-string (gensym))))
	 (junkfile1 (concatenate 'string "/tmp/directory" (princ-to-string (gensym))))
	 #+(or kcl allegro)(aclcommand1 (concatenate 'string "(ls -F " dir " | egrep / ) >> " junkfile1))
	 #+(or lucid sbcl cmu clisp)(cmucommand1 (list "-F" dir))
;	 (aclcommand2 (concatenate 'string "egrep / " junkfile " >> " junkfile1))
	 #+(or lucid sbcl cmu clisp)(cmucommand2 (list "/" junkfile))
	 retlist)
    (when (probe-file junkfile) (delete-file junkfile))
    (when (probe-file junkfile1) (delete-file junkfile1))
    ;;we have to do this in two parts because cmucl won't let us use parens & so the redirection is ambiguous.
    #+:cmu(ext:run-program "ls" cmucommand1 :wait t :output junkfile)
    #+:sbcl(sb-ext:run-program "ls" cmucommand1 :wait t :output junkfile)
    ; mkaminski 10/19/2005 -- clisp support added
    #+:clisp(ext:run-program "ls" :arguments cmucommand1 :wait t :output junkfile)
    #+allegro(excl:run-shell-command aclcommand1 :wait t)
    #+lucid(run-unix-program "ls" :arguments cmucommand1 :wait t :output junkfile)
    #+kcl(system aclcommand1 :wait t)
    ;;junkfile now contains ls -F dir
    #+:cmu(ext:run-program "egrep" cmucommand2 :wait t :output junkfile1)
    #+:sbcl(sb-ext:run-program "egrep" cmucommand2 :wait t :output junkfile1)
    #+:clisp(ext:run-program "egrep" :arguments cmucommand2 :wait t :output junkfile1)
    #+lucid(run-unix-program "egrep" :arguments cmucommand2 :wait t :output junkfile1)
    (with-open-file (ifile junkfile1 :direction :input)
		    (do ((line (read-line ifile nil control-d-char)
			       (read-line ifile nil control-d-char)))
			((eq control-d-char line))
		      (push (concatenate 'string dir line) retlist)))
    (when (probe-file junkfile) (delete-file junkfile))
    (when (probe-file junkfile1) (delete-file junkfile1))
    retlist))

(defun find-subdirs ()
  (let ((real-dld default-lib-dir)
	(real-bld backup-lib-dir))
    (setq default-lib-dir nil backup-lib-dir nil)
    (do ((dirlist real-dld))
	((null dirlist) (progn (when load-warn-p
				 (msgf "Adding the following subdirectories to DEFAULT-LIB-DIR:")
				 (dolist (d default-lib-dir)
				   (unless (member d real-dld :test 'string=) (msgf d))))
			       (setq default-lib-dir (nreverse default-lib-dir))))
      (if (probe-file (make-pathname% :directory (directory-namestring (car dirlist))
				      :name (pathname-name lib-masterindex-file)
				      :type default-libindex-type))
	  ;;this is a library directory because it has a libindex.rec
	  (progn (push (car dirlist) default-lib-dir)
		 (setq dirlist (append (get-directories-below (car dirlist)) (cdr dirlist))))
	(setq dirlist (cdr dirlist))))
    (do ((dirlist real-bld))
	((null dirlist) (progn (when load-warn-p
				 (msgf "Adding the following subdirectories to BACKUP-LIB-DIR:")
				 (dolist (d backup-lib-dir)
				   (unless (member d real-bld :test 'string=) (msgf d))))
			       (setq backup-lib-dir (nreverse backup-lib-dir))))
      (if (and (probe-file (make-pathname% :directory (directory-namestring (car dirlist))
					   :name (pathname-name lib-masterindex-file)
					   :type default-libindex-type))
	       (not (member (car dirlist) default-lib-dir :test 'string=)))
	;;this is a library directory because it has a libindex.rec
	  (progn (push (car dirlist) backup-lib-dir)
		 (setq dirlist (append (get-directories-below (car dirlist)) (cdr dirlist))))
	(setq dirlist (cdr dirlist))))))
       
(defun directory% (dir)
  (if (or (null (file-namestring dir)) (string= "" (file-namestring dir)))
      (setq dir (namestring dir))
      (setq dir (concatenate 'string (namestring dir) "/")))
  ;dir is to be the directory of everything that is returned.
  (let* ((l (mapcar 'namestring (directory dir)))
	 (l1 (mapcar 'file-namestring (remove-if-not 'pathname-name l)))
	 (l2 (remove-duplicates (mapcar 'directory-namestring l) :test 'string=))
	 (l2 (mapcar 'file-namestring (mapcar #'(lambda (x) (adjust-array x (1- (length x)))) l2)))
	 (l2 (mapcar #'(lambda (x) (concatenate 'string x "/")) l2)))
    ; mkaminski 19/10/2005 -- For some reasons, the clisp procedure directory
    ; behaves more like probe-file than the way it is supposed to.
    ; The issue may be fixed in future versions of clisp.
    #+:clisp (list dir)
    #-:clisp (cons dir 
	  (mapcar #'(lambda (x) (pathname (concatenate 'string dir x))) (append l1 l2)))))
;the above stops the directory command from changing /afs/cs/... into /afs/cs.cmu.edu/...

(defun delete-item-from-file (name old-file &optional (type nil) (silent nil))
  (let ((temporary-file (make-pathname% :directory (directory-namestring old-file)
					:name (pathname-name (new-filename))))
	(copied-something nil))
    (unless silent (msg t "Rewriting & parsing file... please wait..."))
    (with-open-file (*output* temporary-file :direction :output)
       (with-open-file (*input* old-file :direction :input)
	  (do ((item (read *input* nil control-d-char)
		     (read *input* nil control-d-char)))
	      ((eq item control-d-char))
	      (if (eq (nth 1 item) name)
		  (if type (if (eq (nth 3 item) type) 
			       nil
			     (progn
			       (write-libitem-quick item *output*)
			       (setq copied-something t)))
		    nil)
		(progn 
		  (write-libitem-quick item *output*)
		  (setq copied-something t))))))
    (delete-file old-file) ; mkaminski 11/17/2005
    (rename-file temporary-file old-file)
    (if (not copied-something) (delete-file old-file))))

(defun modify-item-in-file (name old-file libitem)
  (let ((temporary-file (concatenate 'string (namestring (car default-lib-dir)) (namestring (new-filename))))
	(type (libitem-type libitem)))
    (msg t "Rewriting & parsing file... please wait...")
    (with-open-file (*output-stream* temporary-file :direction :output :if-does-not-exist :create)
       (with-open-file (*input-stream* old-file :direction :input)
		       (do ((item (read *input-stream* nil control-d-char)
				  (read *input-stream* nil control-d-char)))
			   ((eq item control-d-char))
			   (if (eq (cadr (memq :name item)) name)
			       (if (eq (cadr (memq :type item)) type)
				   (write-libitem libitem 'write-to-file *output-stream*)
				 (write-libitem-quick item *output-stream*))
			     (write-libitem-quick item *output-stream*)))))
    (delete-file old-file)  ; mkaminski 10/30/2005 -- rename-file in clisp
                            ;   complains when when old-file is still there
    (rename-file temporary-file old-file)))

(defun insert-item-in-file (libitem)
  (let ((filename (libitem-file libitem)))
    (with-open-file (*output-stream* filename :direction :output
				     :if-exists :append
				     :if-does-not-exist :create)
      (write-libitem libitem 'write-to-file *output-stream*))
    (add-to-masterindex libitem)))

(defun rewrite-masterindex (&optional (libitem nil) (direc nil))
  (declare (special *lib-masterindex*))
  (let ((reroute-close-message nil)
	(objlist nil)
	(dir (or direc (directory-namestring (libitem-file libitem)))))
    (declare (special reroute-close-message))
    (maphash #'(lambda (key val)
		 (when key (dolist (elt val) (setq objlist (cons (list key elt) objlist)))))
	     *lib-masterindex*)
    (setq objlist (remove-if-not #'(lambda (x) 
				     (string= (directory-namestring (cdadr x)) dir))
				 objlist))
    (setq objlist (sort objlist #'string-lessp :key #'car))
    (reroute-output
     (make-pathname% :directory dir
		     :name (pathname-name lib-masterindex-file)
		     :type default-libindex-type)
     *default-pathname-defaults*
     (dolist (obj objlist)
	     (msgf "(" (car obj) " " (caadr obj) " . \"" (file-namestring (cdadr obj)) "\")" t)))))

(defun add-to-masterindex (libitem)
  (let ((cons (cons (libitem-type libitem) (concatenate 'string "\"" (file-namestring (libitem-file libitem)) "\"")))
	(reroute-close-message nil))
    (declare (special reroute-close-message))
    (reroute-output-append
     (make-pathname% :directory (directory-namestring (libitem-file libitem))
		     :name (pathname-name lib-masterindex-file)
		     :type default-libindex-type)
     *default-pathname-defaults*
     (msg (cons (libitem-name libitem) cons))
     (terpri))))

(defun write-libitem (libitem &optional (libstyle style)
			      (*standard-output* *standard-output*) (tex nil))
  (when (eq libstyle 'write-to-file) (msg "("))
  (msg ":name " (libitem-name libitem) t)
  (msg ":type " (libitem-type libitem) t)
  (msg ":context ") (prin1 (libitem-context libitem))
  (msgf ":keywords ") (prin1 (libitem-keywords libitem))
  (msgf ":provability ") (prin1 (libitem-provability libitem))
  (msgf ":proof-date ") (prin1 (libitem-proof-date libitem))
  (msg t ":description ")
  (funcall (get-lib-printfn (libitem-type libitem))
	   (libitem-description libitem) libstyle)
  (if (and tex (neq style 'tex-1) (memq (libitem-type libitem) '(rrule gwff abbr lib-const dpairset)))
      (princ "$"))
  (msg t ":needed-objects" 3 (libitem-needed-objects libitem))
  (msg t ":mhelp ")
  (if tex
      (let ((strout ""))
	(let ((*standard-output* (make-string-output-stream)))
	  (prin1 (libitem-mhelp libitem))
	  (setq strout (get-output-stream-string *standard-output*)))
	(princ-tex-verbatim strout))
    (prin1 (libitem-mhelp libitem)))
  (msg t ":other-attributes ")
  (if (and tex (listp (libitem-other-attributes libitem)))
      (let ((strout ""))
	(let ((*standard-output* (make-string-output-stream)))
	  (insert-lots-of-spaces (libitem-other-attributes libitem))
	  (setq strout (get-output-stream-string *standard-output*)))
	(princ-tex-verbatim strout))
    (prin1 (libitem-other-attributes libitem)))
  (msg t ":other-remarks ")
  (if tex
      (let ((strout ""))
	(let ((*standard-output* (make-string-output-stream)))
	  (prin1 (libitem-other-remarks libitem))
	  (setq strout (get-output-stream-string *standard-output*)))
	(princ-tex-verbatim strout))
    (prin1 (libitem-other-remarks libitem)))
  (when (eq libstyle 'write-to-file) (msg ")" -3)))

(defun write-libitem-quick (unparsed-libitem &optional (*standard-output* *standard-output*))
  (msg "(")
  (do ((item unparsed-libitem (cddr item)))
      ((null item) (msg ")" -3))
    (msg ":") (princ (string-downcase (car item)))
    (msg " ") (prin1 (cadr item)) (terpri)))

(defun insert-lots-of-spaces (listoflists)
  (progn
  (prin1 (car listoflists))
  (if (null (cdr listoflists))
      ()
    (progn
      (princ " ")
      (insert-lots-of-spaces (cdr listoflists))))))

(defun write-description-only (libitem &optional (libstyle style)
			       (*standard-output* *standard-output*) (tex nil))
  (when (eq libstyle 'write-to-file) (msg "("))
  (msg t ":description ")
  (funcall (get-lib-printfn (libitem-type libitem))
	   (libitem-description libitem) libstyle)
  (if (and tex (neq style 'tex-1) (memq (libitem-type libitem) '(rrule gwff abbr lib-const dpairset)))
      (princ "$"))
  (when (libitem-provability libitem)
	(if tex (princ "\\par\\vskip6pt ") (terpri))
	(msg ":provability : " (libitem-provability libitem) t)
	(when (libitem-proof-date libitem) 
	      (msg " since " (libitem-proof-date libitem))))
  (when (libitem-keywords libitem)
	(if tex (princ "\\par\\vskip6pt ") (terpri))
	(msg ":keywords : " (libitem-keywords libitem) t))
  (when (eq libstyle 'write-to-file)
    (msg ")" -2)))

(defun write-brief-description (libitem &optional (libstyle style)
					(*standard-output* *standard-output*) (tex nil))
  (when (eq libstyle 'write-to-file) (msg "("))
  (funcall (get-lib-printfn (libitem-type libitem))
	   (libitem-description libitem) libstyle)
  (if (and tex (neq style 'tex-1) (neq style 'tex-1) (memq (libitem-type libitem) '(rrule gwff abbr lib-const dpairset)))
      (princ "$"))
  (when (eq libstyle 'write-to-file)
	(msg ")" -2)))

(defun write-brief-d-and-m (libitem &optional (libstyle style)
					(*standard-output* *standard-output*) (tex nil))
  (when (eq libstyle 'write-to-file) (msg "("))
  (funcall (get-lib-printfn (libitem-type libitem))
	   (libitem-description libitem) libstyle)
  (if (and tex (neq style 'tex-1) (memq (libitem-type libitem) '(rrule gwff abbr lib-const dpairset)))
      (princ "$"))
  (if tex (princ "\\par{\\vskip6pt}") (terpri))
  (if tex
      (let ((strout ""))
	(let ((*standard-output* (make-string-output-stream)))
	  (princ (libitem-mhelp libitem))
	  (setq strout (get-output-stream-string *standard-output*)))
	(princ-tex-verbatim strout))
    (princ (libitem-mhelp libitem)))
  (when (libitem-provability libitem)
	(if tex (princ "\\par{\\vskip6pt}") (terpri))
	(msg "Provability : " (libitem-provability libitem) t)
	(when (libitem-proof-date libitem) 
	      (msg " since " (libitem-proof-date libitem))))
  (when (libitem-keywords libitem)
	(if tex (princ "\\par\\vskip6pt ") (terpri))
	(msg ":keywords : " (string-downcase (princ-to-string (libitem-keywords libitem))) t))
  (when (eq libstyle 'write-to-file)
	(msg ")" -2)))

(defun write-mhelp-only (libitem &optional (libstyle style)
			       (*standard-output* *standard-output*))
  (when (eq libstyle 'write-to-file) (msg "("))
  (msg t ":mhelp ")
  (prin1 (libitem-mhelp libitem))
  (when (eq libstyle 'write-to-file)
    (msg ")" -2)))

(defun write-libitem-time (libitem &optional (libstyle style)
			      (*standard-output* *standard-output*) (tex nil))
  (when (eq libstyle 'write-to-file) (msg "("))
  (msg ":name " (libitem-name libitem) t)
  (msg t ":description ")
  (funcall (get-lib-printfn (libitem-type libitem))
	   (libitem-description libitem) libstyle)
  (if (and tex (neq style 'tex-1) (memq (libitem-type libitem) '(rrule gwff abbr lib-const dpairset)))
      (princ "$"))
  (msgf ":provability " (libitem-provability libitem))
  (msgf ":proof-date " (libitem-proof-date libitem))
  (msgf ":keywords " (libitem-keywords libitem))
  (msg t t "Timings: ")
  (if tex
      (let ((strout ""))
	(let ((*standard-output* (make-string-output-stream)))
	  (prin-filter (libitem-other-remarks libitem) 0 nil)
	  (setq strout (get-output-stream-string *standard-output*)))
	(princ-tex-verbatim strout))
    (prin-filter (libitem-other-remarks libitem) 0 nil))
  (when (eq libstyle 'write-to-file) (msg ")" -3)))

(defun prin-filter (string pointer printing)
  (unless (> (+ pointer 16) (length string))
   (when (and (not printing)
	      (or (string-equal "Monday" string :start2 pointer :end2 (+ pointer 6))
		  (string-equal "Tuesday" string :start2 pointer :end2 (+ pointer 7))
		  (string-equal "Wednesday" string :start2 pointer :end2 (+ pointer 9))
		  (string-equal "Thursday" string :start2 pointer :end2 (+ pointer 8))
		  (string-equal "Friday" string :start2 pointer :end2 (+ pointer 6))
		  (string-equal "Saturday" string :start2 pointer :end2 (+ pointer 8))
		  (string-equal "Sunday" string :start2 pointer :end2 (+ pointer 6))))
	 (msg t t) (setq printing T))
   (when (and (not printing) (string-equal "Mating Search" string :start2 pointer :end2 (+ pointer 13)))
	 (setq printing T))
   (when (and (not printing) (string-equal "(LAST-MODE-NAME" string :start2 pointer :end2 (+ pointer 15)))
	 (setq printing 42))
   (do ((char (char string pointer) (char string temp-pointer))
	(temp-pointer (+ 1 pointer) (+ 1 temp-pointer)))
       ((eq char #\Newline) (setq pointer temp-pointer) (if printing (msg t)))
       (if printing (msg char)))
   (when (eq printing 42) (setq printing NIL))
   (when (and printing
	      (or (> (+ pointer 16) (length string))
		  (string-equal "Preprocessing" string :start2 pointer :end2 (+ pointer 13))
		  (string-equal "Postprocessing" string :start2 pointer :end2 (+ pointer 14))
		  (string-equal "Merging" string :start2 pointer :end2 (+ pointer 7))
		  (string-equal "Proof Transfor" string :start2 pointer :end2 (+ pointer 14))
		  (string-equal "Printing Proof" string :start2 pointer :end2 (+ pointer 14))
		  (string-equal "Statistics" string :start2 pointer :end2 (+ pointer 10))
		  (string-equal "(" string :start2 pointer :end2 (+ pointer 1))))
	 (setq printing nil))
   (when (<= (+ pointer 14) (length string)) (prin-filter string pointer printing))))

(defun delete-item-from-masterindex (name found
					  &key (delete-from-index-file t)) ; cebrown 8/3/99
  (declare (special *lib-masterindex*))
  (setf (gethash name *lib-masterindex*)
	(delete found (gethash name *lib-masterindex* nil) 
		:test #'(lambda (x y) (and (equal (car x) (car y))
					   (equal (pathname (cdr x)) (pathname (cdr y)))))))
  (when delete-from-index-file
	(rewrite-masterindex nil (directory-namestring (cdr found)))))
  
(defun store-item (libitem)
  (declare (special *lib-masterindex*))
  (let* ((value (gethash (libitem-name libitem) *lib-masterindex* nil))
	 (useful (mapcar 'namestring (mapcar 'cdr value))))
    (if (member (namestring (libitem-file libitem)) ; cebrown 8/2/99 added namestring
		useful :test 'string=)  ; so we are comparing strings, not pathnames
	;then we're modifying, and we shouldn't be
	(complain "Item " (libitem-name libitem) " already exists.")
      ;otherwise we're adding
      (progn
	(push (cons (libitem-type libitem) (libitem-file libitem))
	      (gethash (libitem-name libitem) *lib-masterindex* nil))
	(insert-item-in-file libitem))))
  t)

(defun update-libitem (libitem)
  (declare (special *lib-masterindex*))
  (let* ((value (gethash (libitem-name libitem) *lib-masterindex* nil))
	 (useful (mapcar 'namestring (mapcar 'cdr value))))
    (if (member (namestring (libitem-file libitem)) useful :test 'string=)
	;then we're modifying
	(modify-item-in-file (libitem-name libitem) (libitem-file libitem) libitem)
      ;otherwise we're adding
      (progn (push (cons (libitem-type libitem) (libitem-file libitem))
		   (gethash (libitem-name libitem) *lib-masterindex* nil))
	     (insert-item-in-file libitem))))
  t)
      
(defvar *retrieve-stack* nil)

(defun retrieve-item (name &key (type nil) (multiple show-all-libobjects) (preferred-dir nil) (writeable nil)
			   (fail-gently nil))
  (let* ((load-warn-p nil)
	 (found (locate-item name :type type :multiple multiple :preferred-dir preferred-dir :writeable writeable))
	 (count 0))
    (push (cons name (or type 'daffy-duck)) *retrieve-stack*)
    (when (null (car found)) (if fail-gently (return-from retrieve-item nil)
			       (throwfail "I can't locate " name " of type " type ".")))
    (when (cdr found)
          (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-msg))
	  (complain (length found) " items called " name " found. They are as follows:" t)
	  (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-options))
	  (dolist (elt found)
		  (incf count) 
		  (complain count ") " (car elt) " in file " (pfile (namestring (cdr elt)))))
	  (unless show-all-libobjects
		  (throwfail t "Please either specify the type of the item or set SHOW-ALL-LIBOBJECTS to T.")) ; cebrown: changed NIL to T. 7/26/99
	  (incf count) 
	  (complain count ") None of these.")
	  (setq found (list (nth (1- (get-a-number count)) found))))
    (when (car found)
	  (let ((input-file (cdar found))
		(type (caar found)))
	    (with-open-file
	     (*instream* (complete-lib-filename input-file) :direction :input)
	     (do ((item nil (read *instream* nil control-d-char)))
		 ((or (eq item control-d-char) 
		      (and (eq name (cadr item))
			   (eq type (nth 3 item))))
		  (if (eq item control-d-char)
		      (return (complain "I can't locate " name " of type " type "..."))
		    (progn
		      (close *instream*)
		      (let ((load-warn-p nil))
			(dolist (needed (cadr (memq :needed-objects item)))
				(when (assoc needed *retrieve-stack*)
				      (complain "Recursive definitions are not allowed! This object will be badly defined in TPS.")
				      (return nil))
				(unless (and (not (eq 'theory (cadr (memq :type item)))) (object-present needed nil))
					(setq found (or (locate-item needed :type 'abbr :multiple nil 
								     :preferred-dir (directory-namestring input-file))
							(locate-item needed :type 'lib-const :multiple nil 
								     :preferred-dir (directory-namestring input-file))
							(locate-item needed :type 'gwff :multiple nil 
								     :preferred-dir (directory-namestring input-file))
							(locate-item needed :type 'rrule :multiple nil 
								     :preferred-dir (directory-namestring input-file))
							(locate-item needed :type 'theory :multiple nil 
								     :preferred-dir (directory-namestring input-file))))
					(if (cdr found)
					    (unless 
					     (or (retrieve-libobject needed :type 'abbr :multiple nil 
								     :fail-gently fail-gently
								     :preferred-dir (directory-namestring input-file))
						 (retrieve-libobject needed :type 'lib-const :multiple nil
								     :fail-gently fail-gently
								     :preferred-dir (directory-namestring input-file))
						 (retrieve-libobject needed :type 'gwff :multiple nil
								     :fail-gently fail-gently
								     :preferred-dir (directory-namestring input-file))
						 (retrieve-libobject needed :type 'rrule :multiple nil 
								     :fail-gently fail-gently
								     :preferred-dir (directory-namestring input-file))
						 (retrieve-libobject needed :type 'theory :multiple nil 
								     :fail-gently fail-gently
								     :preferred-dir (directory-namestring input-file)))
					     (return-from retrieve-item nil))
					  (unless 
					   (retrieve-libobject needed :type (caar found) :multiple nil 
							       :fail-gently fail-gently
							       :preferred-dir (directory-namestring input-file))
					   (return-from retrieve-item nil))))))
		      (if fail-gently
			  (%catch%
			   (progn
			     (define-permanent-objects item)
			     (pop *retrieve-stack*)
			     (parse-libitem item input-file nil))
			   (fail nil))
			(progn
			  (define-permanent-objects item)
			  (pop *retrieve-stack*)
			  (parse-libitem item input-file nil))))))))))))

(defun retrieve-item-from-file-without-needed-objs (name type input-file)
  (with-open-file
   (*instream* (complete-lib-filename input-file) :direction :input)
   (do ((item nil (read *instream* nil control-d-char)))
       ((or (eq item control-d-char) 
	    (and (eq name (cadr item))
		 (eq type (nth 3 item))))
	(if (eq item control-d-char)
	    (return (complain "I can't locate " name " of type " type "..."))
	  (parse-libitem item input-file nil))))))

(defun object-present (name type)
  (or (and (memq type '(abbr gwff)) (and (abbrev-q name) (gwff-q name)))
      (and (eq type 'dpairset) (fboundp name))
      (and (eq type 'lib-const) (logconst-p name))
      (and (eq type 'rrule) (and (fboundp 'rewrule-p) (rewrule-p name))) 
      (and (eq type 'theory) (and (fboundp 'theory-p) (theory-p name)))
      (and (memq type '(mode mode1)) (member name global-modelist))
      (and (eq type 'slist) (find-package 'auto) (auto::find-searchlist name auto::*global-searchlist*))
      (and (null type) (or (and (abbrev-q name) (gwff-q name)) (logconst-p name) 
			   (and (fboundp 'rewrule-p) (rewrule-p name)) 
			   (and (fboundp 'theory-p) (theory-p name))
			   (member name global-modelist) (and (find-package 'auto) 
							      (auto::find-searchlist name auto::*global-searchlist*))))))

(defun define-permanent-objects (item)
  (when (or (eq (cadr (memq :type item)) 'gwff)
	    (eq (cadr (memq :type item)) 'abbr))
    (when expertflag
      (let ((name (cadr (memq :name item)))
	    (string (cadr (memq :description item)))
	    (helpstring (cadr (memq :mhelp item))))
	(unless (and (theorem-p name) (not (library-theorem-p name)))
	  (when (and load-warn-p (theorem-p name)) 
	    (msg t "Overwriting existing library theorem..." t))
	  (eval `(deftheorem ,name (assertion ,string) 
		   (thm-type library) (allowed-cmd-p allow-all) 
		   (allowed-lemma-p allow-no-lemmas) 
		   (mhelp ,helpstring)))
	  (setf (get name 'allowed-lemma-p) 'allow-all))))
    (when (and expertflag (eq (cadr (memq :type item)) 'gwff))
      (let* ((name (cadr (memq :name item)))
	     (string (cadr (memq :description item)))
	     (helpstring (cadr (memq :mhelp item)))
	     (tp (princ-to-string (type (getrwff string)))))
	(unless (and (abbrev-q name) (not (library-theorem-p name)))
	  (when (and load-warn-p (abbrev-q name)) 
	    (msg t "Overwriting existing abbreviation..." t))
	  (eval `(def-abbrev ,name (defn ,string) (type ,tp) 
		   (fo-single-symbol t) 
		   (printnotype t) (mhelp ,helpstring))))))))

;don't have to do this for modes, as it's already done by parse-libitem.
;we also don't have to def-abbrev for abbreviations, for the same reason.

(defun parse-libitem (item filename &optional(needed t) (reload nil))
  (if needed (dolist (needed (cadr (memq :needed-objects item)))
		     (if (not (and (not reload) (eq (get needed 'thm-type) 'library)))
			 (if (locate-item needed :type 'abbr :multiple nil)
			     (retrieve-libobject needed :type 'abbr :multiple nil 
						 :preferred-dir (directory-namestring filename))
			   (if (locate-item needed :type 'lib-const :multiple nil 
					    :preferred-dir (directory-namestring filename))
			       (retrieve-libobject needed :type 'lib-const :multiple nil 
						   :preferred-dir (directory-namestring filename))
			     (if (locate-item needed :type 'gwff :multiple nil 
					      :preferred-dir (directory-namestring filename))
				 (retrieve-libobject needed :type 'gwff :multiple nil 
						     :preferred-dir (directory-namestring filename))
			       (retrieve-libobject needed :multiple nil 
						   :preferred-dir (directory-namestring filename))))))))
;we assume needed-objects are gwffs if they aren't abbrs; if we can't find either,
;then we hope it's either one or the other but not both in the backup directory
  (let ((type (cadr (memq :type item))))
    (make-libitem :name (cadr (memq :name item))
		  :type type
		  :description (funcall (get-lib-descr-readfn type)
					(cadr (memq :description item)))
		  :mhelp (cadr (memq :mhelp item))
		  :provability (cadr (memq :provability item))
		  :keywords (cadr (memq :keywords item))
		  :proof-date (cadr (memq :proof-date item))
		  :context (cadr (memq :context item))
		  :other-attributes
		  (funcall (get-lib-attr-readfn type) 
			   (cadr (memq :other-attributes item)))
		  :other-remarks (cadr (memq :other-remarks item))
		  :needed-objects (cadr (memq :needed-objects item))
		  :file filename)))

(defun delete-libobject (name type &optional (silent nil) (dir nil))
  (let ((found (locate-item name :type type :writeable t :multiple (if dir nil t) :preferred-dir dir))
	(count 0)
	(load-warn-p nil))
    (when (null (car found)) (throwfail "I can't locate " name " of type " type ".")) 
    (when (cdr found)
          (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-msg))
	  (complain (length found) " items called " name " found. They are as follows:" t)
	  (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-options))
	  (dolist (elt found)
		  (incf count) 
		  (complain count ") " (car elt) " in file " (pfile (namestring (cdr elt)))))
	  (unless show-all-libobjects
		  (throwfail t "Please either specify the type of the item or set SHOW-ALL-LIBOBJECTS to T.")) ; cebrown: changed NIL to T.
	  (incf count) 
	  (complain count ") None of these.")
	  (setq found (list (nth (1- (get-a-number count)) found))))
    (when (car found)
	  (delete-item-from-masterindex name (car found))
	  (delete-item-from-file name (cdar found) type silent))))

; this is useful for when we already have the parsed formula, but
; need to reconstruct type information. - 8/14/01
(defun def-abbr-quick (name formula &optional (help ""))
  (unless (symbolp name)
    (throwfail "Abbreviation " name " should be a symbol"))
  (intern name :CL-USER)
  (setf (get name 'defn) formula)
  (setf (get name 'mhelp) (acons 'abbrev help (get name 'mhelp)))
  (setf (get name 'abbrev) t)
  (setf (get name 'fo-single-symbol) t)
  (setf (get name 'type) (type formula))
  (setf (get name 'typelist)
	(setdiff (auto::find-prim-types formula) '(I O S)))
  (push name global-abbrevlist)
  name)

(defun princ-tex-verbatim (str)
  (dotimes (i (length str))
    (let ((c (aref str i)))
      (if (eq c #\space)
	  (princ "\\enskip ")
	(if (tex-verbatim-char-p c)
	    (princ (format nil "\\char~d " (char-code c)))
	  (princ c))))))

(defun tex-verbatim-char-p (c)
  (and (not (member c '(#\- #\=)))
       (let ((a (char-code c)))
	 (or (and (> a 32) (< a 48))
	     (and (> a 57) (< a 65))
	     (and (> a 90) (< a 97))
	     (> a 122)))))

      
