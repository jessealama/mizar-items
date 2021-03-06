
(in-package :mizar)

(defparameter *mizar-items-root* "/Users/alama/sources/mizar/mizar-items/lisp")

(defun file-in-mizar-items-dir (filename)
  (concatenate 'string *mizar-items-root* "/" filename))

(defparameter *mizar-items-xsl4mizar-root* "/Users/alama/sources/mizar/mizar-items/src/xslt")

(defun file-in-xsl4mizar-dir (filename)
  (concatenate 'string *mizar-items-xsl4mizar-root* "/" filename))

(defparameter *mizar-items-data-root*
  (concatenate 'string *mizar-items-root* "/" "data"))

(defun file-in-data-dir (filename)
  (concatenate 'string *mizar-items-data-root* "/" filename))

(defun file-in-data-dir-for-mml (filename mml-version)
  (format nil "~a/~a/~a" *mizar-items-data-root* mml-version filename))

(defun mml-lar-path (mml-version)
  (file-in-data-dir-for-mml "mml.lar" mml-version))

(defun mml-lar ()
  (lines-of-file (merge-pathnames "mml.lar" (mizfiles))))

(defun mml-data-dir ()
  (merge-pathnames "mml/" (mizfiles)))

(defun bib-directory-for-mml (mml-version)
  (format nil "~a/bib" (mml-data-dir mml-version)))

(defun bib-file-for-article (mml-version article-name)
  (format nil "~a/~a.bib" (bib-directory-for-mml mml-version) article-name))

(defun item-to-fragment-table-for-mml (mml-version)
  (format nil "~a/item-to-fragment-table" (mml-data-dir mml-version)))

(defun item-dependency-table-for-mml (mml-version)
  (format nil "~a/item-dependency-table" (mml-data-dir mml-version)))

(defun property-directory-for-mml (mml-version)
  (format nil "~a/properties" (mml-data-dir mml-version)))

(defun needed-property-file-for-mml (mml-version)
  (format nil "~a/needed" (property-directory-for-mml mml-version)))

(defvar *mizar-items-config* (make-hash-table))

(defun update-mizar-items-config (key value)
  (setf (gethash key *mizar-items-config*) value))

(update-mizar-items-config 'sandbox-location
                           (pathname "/dev/shm/"))
(update-mizar-items-config 'exec-in-dir-script-path
                           (file-in-mizar-items-dir "exec-in-dir.sh"))
(update-mizar-items-config 'fix-by-and-from-script-path
                           (file-in-mizar-items-dir "fix-by-and-from.sh"))
(update-mizar-items-config 'expand-canceled-script-path
                           (file-in-mizar-items-dir "expand-canceled.pl"))
(update-mizar-items-config 'xsl4mizar-path
                           "/Users/alama/sources/mizar/mizar-items/xsl4mizar/")
(update-mizar-items-config 'listvoc-script-path
                           (file-in-mizar-items-dir "listvoc.sh"))
(update-mizar-items-config 'author-title-script
                           (file-in-mizar-items-dir "author-title.pl"))
(update-mizar-items-config 'mhtml-css-path
                           (file-in-xsl4mizar-dir "MHTML/mhtml.css"))
(update-mizar-items-config 'screen-css-path
                           (file-in-xsl4mizar-dir "MHTML/screen.css"))
(update-mizar-items-config 'mhtml-js-path
                           (file-in-xsl4mizar-dir "MHTML/mhtml.js"))
(update-mizar-items-config 'absrefs-stylesheet
                           (file-in-xsl4mizar-dir "addabsrefs.xsl"))
(update-mizar-items-config 'update-requirements-stylesheet
                           (file-in-xsl4mizar-dir "update-requirements.xsl"))
(update-mizar-items-config 'propertied-constructors-stylesheet
                           (file-in-xsl4mizar-dir "propertied-constructors.xsl"))
(update-mizar-items-config 'list-properties-stylesheet
                           (file-in-xsl4mizar-dir "list-properties.xsl"))
(update-mizar-items-config 'strip-prop-stylesheet
                           (file-in-xsl4mizar-dir "strip-prop.xsl"))
(update-mizar-items-config 'mhtml-stylesheet
                           (file-in-xsl4mizar-dir "MHTML/mhtml_main.xsl"))
(update-mizar-items-config 'env-stylesheet
                           (file-in-xsl4mizar-dir "env.xsl"))
(update-mizar-items-config 'split-stylesheet
                           (file-in-xsl4mizar-dir "split.xsl"))
(update-mizar-items-config 'itemize-stylesheet
                           (file-in-xsl4mizar-dir "itemize.xsl"))
(update-mizar-items-config 'wsm-stylesheet
                           (file-in-xsl4mizar-dir "wsm.xsl"))
(update-mizar-items-config 'evl2environ-stylesheet
                           (file-in-xsl4mizar-dir "evl2environ.xsl"))
(update-mizar-items-config 'extend-evl-stylesheet
                           (file-in-xsl4mizar-dir "extend-evl.xsl"))
(update-mizar-items-config 'free-variables-stylesheet
                           (file-in-xsl4mizar-dir "free-variables.xsl"))
(update-mizar-items-config 'toplevel-dellink-stylesheet
                           (file-in-xsl4mizar-dir "toplevel-dellink.xsl"))
(update-mizar-items-config 'toplevel-private-functions-stylesheet
                           (file-in-xsl4mizar-dir "toplevel-private-functions.xsl"))
(update-mizar-items-config 'toplevel-constant-definition-stylesheet
                           (file-in-xsl4mizar-dir "toplevel-constant-definition.xsl"))
(update-mizar-items-config 'toplevel-type-changing-statements-stylesheet
                           (file-in-xsl4mizar-dir "toplevel-type-changing-statements.xsl"))
(update-mizar-items-config 'toplevel-private-predicates-and-functions-stylesheet
                           (file-in-xsl4mizar-dir "toplevel-private-predicates-and-functions.xsl"))
(update-mizar-items-config 'toplevel-choice-stylesheet
                           (file-in-xsl4mizar-dir "toplevel-choice.xsl"))
(update-mizar-items-config 'item-to-fragment-path
                           (file-in-mizar-items-dir "item-to-fragment-table"))
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
                           "/local/data/alama/brutalized-itemizations")
(update-mizar-items-config 'html-source
                           "/local/data/alama/non-brutalized-itemizations")

(defun mizar-items-config (key)
  (gethash key *mizar-items-config*))
