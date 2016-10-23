; this file contains code to read a .bdf font file and generate
; .bmp versions of each character.  The .bmp versions can
; then be converted to .ppm's and finally to .gif's

; Examples:
; (font-to-bmp813 "/home/theorem/tps/fonts/vtsingle.bdf" "vtsingle.") ; for the small 8x13 fonts
; (font-to-bmp1220 "/home/theorem/tps/fonts/galsymbold.bdf" "galsymbold.") ; for the big 12x20 fonts
; (bmptogif (list "vtsingle.1" . . .  "galsymbold.96" . . . )) ; to convert a list of .bmp files to .gif files

(defvar *img-directory* "/home/theorem/fontimages/")

; header for .bmp file for a character 12 x 20
(defvar header1220 '(66 77 142
 0 0 0 0 0 0 0 62 0 0 0 40 0 0 0 12 0 0 0 20 0 0 0 1 0 1 0 0 0
 0 0 80 0 0 0 109 11 0 0 109 11 0 0 2 0 0 0 2 0 0 0 255 255 255
 0 0 0 0
 0 0 0 0))

; header for .bmp file for a character 8 x 13
(defvar header813 '(66 77 114 0 0 0 0 0 0 0 62 0 0 0 40 0 0 0 8 0 0 0 13 0 0 0 1 0 1 0 0 0
 0 0 52 0 0 0 109 11 0 0 109 11 0 0 2 0 0 0 2 0 0 0 255 255 255 0 0 0 0 0 0 0 0))


(defun font-to-bmp1220 (fontfile imgname)
  (let ((f (open fontfile :direction :input))
	(imf nil)
	(bytestack nil)
	(i 0))
    (do ((l (read-line f nil nil) (read-line f nil nil)))
	((null l)
	 (close f))
	(when (stringp l)
	  (if (and (> (length l) 7)
		   (string= l "ENCODING" :end1 8))
	      (let* ((r (read-from-string (format nil "(~d)" l)))
		     (c (cadr r)))
		(if (numberp c)
		    (setq i c)))
	    (if (string= l "ENDCHAR")
		(progn
		  (do ((bs bytestack (cddr bs)))
		      ((null bs)
		       (close imf))
		      (let ((byte1 (car bs))
			    (byte2 (cadr bs)))
			(write-byte 0 imf)
			(write-byte byte1 imf)
			(write-byte byte2 imf)
			(write-byte 0 imf)

;			(write-byte 0 imf)
;			(write-byte 0 imf)
;			(write-byte byte1 imf)
;			(write-byte byte2 imf)
			))
		  (setq imf nil))
	      (if (string= l "BITMAP")
		  (progn
		    (setq imf (open (format nil "~d/~d~d.bmp" *img-directory* imgname i)
				    :direction :output :if-exists :supersede
				    :if-does-not-exist :create
				    :element-type 'unsigned-byte))
		    (setq bytestack nil)
		    (dolist (h header1220)
			    (write-byte h imf)))
		(when imf
		  (let* ((hex (read-from-string (format nil "#x~d" l)))
			 (byte2 (mod hex 256))
			 (byte1 (/ (- hex byte2) 256)))
		    (push byte2 bytestack)
		    (push byte1 bytestack)
		    )))))))))


(defun font-to-bmp813 (fontfile imgname)
  (let ((f (open fontfile :direction :input))
	(imf nil)
	(bytestack nil)
	(i 0))
    (do ((l (read-line f nil nil) (read-line f nil nil)))
	((null l)
	 (close f))
	(when (stringp l)
	  (if (and (> (length l) 7)
		   (string= l "ENCODING" :end1 8))
	      (let* ((r (read-from-string (format nil "(~d)" l)))
		     (c (cadr r)))
		(if (numberp c)
		    (setq i c)))
	    (if (string= l "ENDCHAR")
		(progn
		  (do ((bs bytestack (cdr bs)))
		      ((null bs)
		       (close imf))
		      (let ((byte1 (car bs)))
			(write-byte 0 imf)
			(write-byte byte1 imf)
			(write-byte 0 imf)
			(write-byte 0 imf)

			))
		  (setq imf nil))
	      (if (string= l "BITMAP")
		  (progn
		    (setq imf (open (format nil "~d/~d~d.bmp" *img-directory* imgname i)
				    :direction :output :if-exists :supersede
				    :if-does-not-exist :create
				    :element-type 'unsigned-byte))
		    (setq bytestack nil)
		    (dolist (h header813)
			    (write-byte h imf)))
		(when imf
		  (let* ((hex (read-from-string (format nil "#x~d" l))))
		    (push hex bytestack)
		    )))))))))

(defun bmptogif (bmpfiles)
  (dolist (b bmpfiles)
	  (excl:run-shell-command (format nil "bmptoppm ~d/~d.bmp > ~d/~d.ppm"
					  *img-directory* b *img-directory* b))
	  (excl:run-shell-command (format nil "ppmtogif ~d/~d.ppm > ~d/~d.gif"
					  *img-directory* b *img-directory* b))))
