;;; file-utils.lisp Do things with files

(in-package :mizar)

(defgeneric replace-extension (path old-extension new-extension))

(defmethod replace-extension ((path string) old-extension new-extension)
  (pathname (concatenate 'string
			 (subseq path 0 (- (length path)
					   (length old-extension)))
			 new-extension)))

(defmethod replace-extension ((path pathname) old-extension new-extension)
  (replace-extension (namestring path) old-extension new-extension))

(defun lines-in-file (path)
  (let (lines)
    (with-open-file (stream path)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(push line lines)))
    (reverse lines)))

(defun file-size (filename)
  ;; stolen from http://www.gigamonkeys.com/book/files-and-file-io.html
  (with-open-file (in filename :element-type '(unsigned-byte 8))
    (file-length in)))

;;; file-utils.lisp ends here