
(in-package :mizar)

(defsuite mizar-environment nil)

(deftest mizfiles-exists (mizar-environment)
  (assert-true (directory-exists-p (mizfiles)))
  (assert-true (directory-exists-p (mml-data-dir))))

(defsuite miz2lisp (mizar-environment))

(deftest xboole_0 ()
  (let ((xboole_0-path (merge-pathnames "xboole_0.miz" (mml-data-dir))))
    (assert-true (file-exists-p xboole_0-path))
    (let ((xboole_0-as-string (file-as-string xboole_0-path)))
      (assert-true (stringp xboole_0-as-string))
      (assert-false (string= xboole_0-as-string ""))
      (let ((xboole_0-text-proper (parse xboole_0-as-string)))
	(let ((temp (temporary-file :base "article" :extension "miz")))
	  (let ((rendered-xboole_0 (write-article xboole_0-text-proper temp)))
	    (assert-true (stringp rendered-xboole_0))
	    (assert-false (string= rendered-xboole_0 ""))
	    (assert-true (write-string-into-file rendered-xboole_0 temp))
	    (assert-true (verifiable temp))))))))
