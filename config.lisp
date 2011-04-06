
(in-package :mizar)

(defparameter *mizar-items-root* "/Users/alama/sources/mizar/mizar-items")

(defun file-in-mizar-items-dir (filename)
  (concatenate 'string *mizar-items-root* "/" filename))

(defparameter *mizar-items-xsl4mizar-root* "/Users/alama/sources/mizar/xsl4mizar")

(defun file-in-xsl4mizar-dir (filename)
  (concatenate 'string *mizar-items-xsl4mizar-root* "/" filename))

(defvar *mizar-items-config* (make-hash-table))

(defun update-mizar-items-config (key value)
  (setf (gethash key *mizar-items-config*) value))

(update-mizar-items-config 'sandbox-location 
			   "/dev/shm/")
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
(update-mizar-items-config 'mhtml-css-path
			   (file-in-xsl4mizar-dir "MHTML/mhtml.css"))
(update-mizar-items-config 'screen-css-path
			   (file-in-xsl4mizar-dir "MHTML/screen.css"))
(update-mizar-items-config 'mhtml-js-path
			   (file-in-xsl4mizar-dir "MHTML/mhtml.js"))
(update-mizar-items-config 'fragment-depdenency-graph
			   (file-in-mizar-items-dir "ckb-ckb-depgraph"))
(update-mizar-items-config 'item-to-fragment-path
			   (file-in-mizar-items-dir "mizar-item-ckb-table"))
(update-mizar-items-config 'full-item-dependency-graph
			   (file-in-mizar-items-dir "full-item-depgraph"))
(update-mizar-items-config 'vertex-neighbors-forward-graph-path
			   (file-in-mizar-items-dir "full-vertex-neighbors-forward-depgraph"))
(update-mizar-items-config 'vertex-neighbors-backward-graph-path
			   (file-in-mizar-items-dir "full-vertex-neighbors-backward-depgraph"))
(update-mizar-items-config 'favicon-path
			   (file-in-mizar-items-dir "favicon.ico"))
(update-mizar-items-config 'server-access-log-file
			   "/tmp/mizar-items-access")
(update-mizar-items-config 'server-messages-log-file
			   "/tmp/mizar-items-messages")
(update-mizar-items-config 'itemization-source
			   "/Users/alama/sources/mizar/mizar-items/itemization-brutalized")
(update-mizar-items-config 'html-source
			   "/Users/alama/sources/mizar/mizar-items/itemization-non-brutalized")

(defun mizar-items-config (key)
  (gethash key *mizar-items-config*))
