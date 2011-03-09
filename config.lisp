
(in-package :mizar)

(defparameter *mizar-items-root* "/Users/alama/sources/mizar/mizar-items")

(defun file-in-mizar-items-dir (filename)
  (concatenate 'string *mizar-items-root* "/" filename))

(defparameter *mizar-items-config* (make-hash-table))

(defun update-mizar-items-config (key value)
  (setf (gethash key *mizar-items-config*) value))

(update-mizar-items-config 'sandbox-location 
			   "/Volumes/ramdisk/")
(update-mizar-items-config 'exec-in-dir-script-path
			   (file-in-mizar-items-dir "exec-in-dir.sh"))
(update-mizar-items-config 'fix-by-and-from-script-path
			   (file-in-mizar-items-dir "fix-by-and-from.sh"))
(update-mizar-items-config 'expand-canceled-script-path
			   (file-in-mizar-items-dir "expand-canceled.pl"))
(update-mizar-items-config 'xsl4mizar-path
			   "/Users/alama/sources/mizar/xsl4mizar")
(update-mizar-items-config 'listvoc-script-path
			   (file-in-mizar-items-dir "listvoc.sh"))

(defun mizar-items-config (key)
  (gethash key *mizar-items-config*))