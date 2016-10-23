; the following function reads a file and creates (or overwrites)
; a new file with the contents except #\return characters.
; This is useful because MS Windows uses #\return to end lines
; where Unix uses #\Newline.  So, a file edited in MS, then viewed
; in Unix has a lot of ^M's at the end of lines.  Basically, filtering
; a file through cleanms deletes these ^M's.

(defun cleanms (infile outfile)
  (let ((inf (open infile :direction :input))
	(outf (open outfile :direction :output :if-exists :supersede
		    :if-does-not-exist :create)))
    (do ((ch (read-char inf nil 'eof) (read-char inf nil 'eof)))
	((eq ch 'eof)
	 (close inf)
	 (close outf))
	(unless (equal ch #\return)
	  (write-char ch outf)))))
