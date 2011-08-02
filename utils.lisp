;;; utils.lisp

(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun concat (&rest strings)
  (apply 'concatenate 'string strings))

(defun lines-as-array (str)
  "Treat the newline character in STR as a delimiter and make an array of strings."
  (let ((split (split "\\n" str)))
    (make-array (list (length split)) :initial-contents split)))

(defun array->newline-delimited-string (array)
  (let ((newline (make-string 1 :initial-element #\Newline)))
    (reduce #'(lambda (s1 s2)
		(concat s1 newline s2))
	    array)))

(defun uppercase (str)
  (format nil "~@:(~A~)" str))

(defun lowercase (str)
  (format nil "~(~A~)" str))

(defun colon-separated-list (list)
  (format nil "~{~a~^:~}" list))

(defun pad-with-newline (str)
  (format nil "~A~%" str))

(defun ensure-final-semicolon (str)
  (if (string= str "")
      ""
      (let* ((len (length str))
	     (final-char (aref str (1- len))))
	(if (eq final-char #\;)
	    str
	    (format nil "~A;" str)))))

(defun maybe-strip-semicolon (string)
  (if (string= string "")
      ""
      (if (char= (aref string 0) #\;)
	  (subseq string 1)
	  string)))

(defun chomp (string)
  (let ((len (length string)))
    (if (zerop len)
	""
	(if (char= (aref string (1- len)) #\Newline)
	    (if (= len 1)
		""
		(subseq string 0 (- len 2)))
	    string))))

(defun non-empty-stringp (thing)
  (and (stringp thing)
       (not (string= thing ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lists and sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun first-n (lst n)
  (loop 
     for i from 1 upto n
     for elt in lst
     collecting elt into items
     finally (return items)))

(defun last-n (lst n)
  (nthcdr (- (length lst) n) lst))

(defun tuple-lex-less (tuple-1 tuple-2)
  "Determine whether TUPLE-1 is lexicographically less than TUPLE-2,
  ignoring all but the first and second components of both tuples."
  (let ((first-1 (first tuple-1))
	(first-2 (first tuple-2))
	(second-1 (second tuple-1))
	(second-2 (second tuple-2)))
    (or (< first-1 first-2)
	(and (= first-1 first-2)
	     (< second-1 second-2)))))

(defun minimal-sublist-satisfying (list predicate)
  "Find a minimal sublist of LIST that satisfies PREDICATE, which is
assumed to take a single argument of type list.

The computed sublist will be built by successively removing elements
from the beginning of the list."
  (if (null list)
      nil
      (let ((head (car list))
	    (tail (cdr list)))
	(if (funcall predicate tail)
	    (minimal-sublist-satisfying tail predicate)
	    (cons head (minimal-sublist-satisfying tail predicate))))))

(defun all-but-last (lst)
  (reverse (cdr (reverse lst))))

(defun remove-nth-element (lst n)
  (append (first-n lst n)
	  (nthcdr (1+ n) lst)))

(defun chunkify (list chunk-size)
  "A list (CHUNK-1 CHUNK-2 ... CHUNK-n) of sublists of LIST all having
  size equal to CHUNK-SIZE, except CHUNK-n, which will be of size at
  least 1 but at most CHUNK-SIZE.  The list (CHUNK-1 CHUNK-2
  ... CHUNK-n) is such that appending each of them together yields a
  list equal to LIST.  If CHUNK-SIZE is less than 1, the value is NIl."
  (unless (< chunk-size 1)
    (loop
       with chunks = nil
       with chunk = nil
       with len = (length list)
       with i = 0
       for elt in list
       for counter from 1 upto len
       do
	 (push elt chunk)
	 (incf i)
	 (when (or (= i chunk-size)
		   (= counter len))
	   (push (reverse chunk) chunks)
	   (setf chunk nil
		 i 0))
       finally
	 (return (reverse chunks)))))

(defun shortest-admissible-final-segment (list pred)
  (labels ((iteratively-remove-from-end (list pred)
	     (loop
		with len = (length list)
		for i from 0 upto len
		for trimmed = (last-n list i)
		do
		  (when (funcall pred trimmed)
		    (return trimmed))))
	   (iteratively-with-chunks (chunks pred)
	     (let ((pred-on-chunked-list #'(lambda (chunks)
					     (funcall pred (apply #'append chunks)))))
	       (iteratively-remove-from-end chunks pred-on-chunked-list)))
	   (shortest-with-chunks (list pred)
	     (let* ((len (length list))
		    (chunks (chunkify list (floor (sqrt len))))
		    (initial-chunk-segment (iteratively-with-chunks chunks pred)))
	       (iteratively-remove-from-end (apply #'append initial-chunk-segment) pred))))
    (shortest-with-chunks list pred)))

(defun last-removable-element (list pred)
  (loop
     with len = (length list)
     for i from (1- len) downto 0
     for trimmed = (remove-nth-element list i)
     do
       (when (funcall pred trimmed)
	 (return i))
     finally
       (return nil)))

;; (defun remove-unneeded (list pred)
;;   (loop
;;      with left-limit = 0
;;      with right-limit = (length list)
;;      for half = (ceiling (/ (- right-limit left-limit) 2))
;;      for lst = nil then (first-n list right-limit)
;;      do
;;        (break "left limit = ~d and right limit = ~d and half = ~d and lst = ~A" left-limit right-limit half lst)
;;        (if (= left-limit right-limit)
;; 	   (return lst)
;; 	   (if (funcall pred lst)
;; 	       (setf right-limit (ceiling (/ right-limit 2)))
;; 	       (setf left-limit half)))))

(defun remove-unneeded (list pred)
  (let ((index-of-last-unneeded (last-removable-element list pred)))
    (if index-of-last-unneeded
	(remove-unneeded (remove-nth-element list
					     index-of-last-unneeded)
			 pred)
	list)))

(defun remove-unneeded-chunked (list pred)
  (let* ((len (length list))
	 (chunk-size (floor (sqrt len))))
    (let ((first-pass (loop
			 with chunks = (chunkify list chunk-size)
			 with needed-chunks = chunks
			 for chunk in chunks
			 do
			   (let* ((sans-chunk (remove chunk needed-chunks))
				  (stripped-list (apply #'append sans-chunk)))
			     (when (funcall pred stripped-list)
			       (setf needed-chunks sans-chunk)))
			 finally
			   (return (apply #'append needed-chunks)))))
      (remove-unneeded first-pass pred))))

(defun subsequence-from-indices (seq indices)
  (loop
     with num-indices = (length indices)
     with sorted-indices = (sort indices #'<)
     with new-seq = (make-array (list num-indices))
     for i from 0 upto num-indices
     for index in sorted-indices
     do (setf (aref new-seq i) (aref seq index))
     finally (return new-seq)))

(defun numbers-from-to (start end)
  (loop
     for i from start upto end 
     collecting i into nums
     finally (return nums)))

(defun minimal-subsequence-satisfying (seq predicate)
  (loop
     with len = (length seq)
     with needed-indices = (numbers-from-to 0 (1- len))
     for i from len downto 1
     for elt = (aref seq (1- i))
     for candidate = (subsequence-from-indices seq needed-indices)
     do
       (unless (funcall predicate candidate)
	 (delete (1- i) needed-indices))
     finally
       (return (subsequence-from-indices seq needed-indices))))

(defun every-with-falsifying-witness (list pred)
  "Determine whether every member of LIST satisfies the unary
predicate PRED.  Rreturns two values: if there is a member of LIST
that fails to satisfy PRED, return NIL and the first such member of
LIST; otherwise, return T and NIL."
  (loop
     for elt in list
     do
       (unless (funcall pred elt)
	 (return (values nil elt)))
     finally
       (return (values t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Iteration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro until (condition &body body)
  `(do nil (,condition) ,@body))

(defmacro while (condition &body body)
  `(do nil ((not ,condition)) ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun values-for-keys-less-than (table key &key (predicate #'<))
  (let (result)
    (maphash #'(lambda (k v)
		 (when (funcall predicate k key)
		   (push v result)))
	     table)
    result))

(defun keys (table)
  (loop 
     for k being the hash-keys in table
     collecting k into keys
     finally (return keys)))

(defun values-of-table (table)
  (loop
     for v being the hash-values in table
     collecting v into vals
     finally (return vals)))

(defun key-for-value (val table)
  (loop
     for k being the hash-keys in table
     for v being the hash-values in table
     do
       (when (eq v val)
	 (return k))
     finally
       (return nil)))

(defun keys-with-rest->hash-table (keys)
  "Given a list of lists of the form (KEY1 . REST1), make a hash table whose keys are the KEYs, and whose values are the associated RESTs."
  (loop
     with table = (make-hash-table :test #'equal)
     for (key . val) in keys
     do
       (setf (gethash key table) val)
     finally 
       (return table)))

(defun count-hash-table-keys (table)
  "Assuming that all values of the hash table TABLE are sequences, sum the length of all of them."
  (loop
     for v being the hash-values in table
     summing (length v) into num-edges
     finally (return num-edges)))

(defun hash-table-from-keys-and-values (test &rest keys-and-values)
  (loop
     with table = (make-hash-table :test test)
     for (key . value) in keys-and-values
     do
       (setf (gethash key table) value)
     finally
       (return table)))

(defun random-key-with-value (table)
  (let ((random-key (random-elt (hash-table-keys table))))
    (multiple-value-bind (value we-already-know-that-its-present)
	(gethash random-key table)
      (declare (ignore we-already-know-that-its-present))
      (values random-key value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files and streams
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lines-of-file (path)
  (let (lines)
    (with-open-file (file path :direction :input
			       :if-does-not-exist :error)
      (symbol-macrolet
	  (($line (read-line file nil nil)))
	(do ((line $line $line))
	    ((null line))
	  (push line lines))))
    (reverse lines)))

(defun file-as-string (path)
  (let ((newline (make-string 1 :initial-element #\Newline))
	(lines (lines-of-file path)))
    (if lines
	(reduce #'(lambda (s1 s2)
		    (concat s1 newline s2))
		(lines-of-file path))
	"")))

(defun stream-lines (stream)
  (when stream
    (let (lines)
      (symbol-macrolet
	  (($line (read-line stream nil nil)))
	(do ((line $line $line))
	    ((null line))
	  (push line lines)))
      (reverse lines))))

(defun empty-file-p (path)
  (with-open-file (s path :direction :input
		          :if-does-not-exist :error)
    (let ((c (peek-char t s nil :end)))
      (when (eq c :end)
	t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ensure-directory (directory)
  (namestring (pathname-as-directory directory)))

(defun file-in-directory (directory filename &optional extension)
  (concat (ensure-directory directory) filename (if (null extension)
						    ""
						    (concat "." extension))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Regular expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun regexp-disjoin (&optional str-1 str-2)
  (if (and str-1 str-2)
      (concat str-1 "|" str-2)
      ""))

(defun exact-regexp (str)
  (concatenate 'string "^" str "$"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Running programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-program (program args &key search input output error wait)
  #+sbcl
  (sb-ext:run-program program
		      args
		      :search t
		      :input nil
		      :output nil
		      :error nil
		      :wait wait)
  #+ccl
  (ccl:run-program program
		   args
		   :input input
		   :output output
		   :error error
		   :wait wait))

(defun process-exit-code (process)
  #+sbcl
  (sb-ext:process-exit-code process)
  #+ccl
  (multiple-value-bind (status exit-code)
      (ccl:external-process-status process)
    (declare (ignore status))
    exit-code))

(defun process-output (process)
  #+sbcl
  (sb-ext:process-output process)
  #+ccl
  (ccl:external-process-output-stream process))

(defun process-error (process)
  #+sbcl
  (sb-ext:process-error process)
  #+ccl
  (ccl:external-process-error-stream process))

(defun temporary-file (&key (base "") (extension "") (tmp-dir "/tmp"))
  (if (or (stringp tmp-dir)
	  (pathnamep tmp-dir))
      (if (stringp extension)
	  (if (scan "^\.?[a-zA-Z0-9]*$" extension)
	      (if (stringp base)
		  (if (scan "^[A-Za-z]*$" base)
		      (register-groups-bind (real-ext)
			  ("^\.?([a-zA-Z0-9]*)$" extension)
			(let ((real-tmp-dir (pathname-as-directory tmp-dir)))
			  (if (directory-p real-tmp-dir)
			      (loop
				 with real-tmp-dir-name = (namestring real-tmp-dir)
				 for i from 1 upto 1000
				 for tmp-path = (if (string= real-ext "")
						    (format nil "~a/~a~d" real-tmp-dir-name base i)
						    (format nil "~a/~a~d.~a" real-tmp-dir-name base i real-ext))
				 do
				   (unless (probe-file tmp-path)
				     (return (pathname tmp-path)))
				 finally
				   (if (string= base "")
				       (error "We have run out of temporary file names in ~a!" tmp-dir)
				       (error "We have run out of temporary file names in ~a! with the base name ~a" tmp-dir base)))
			      (error "We cannot understand '~a' as a directory" tmp-dir))))
		      (error "BASE must consist of alphanumeric characters only; '~a' is not a suitable argument" base))
		  (error "BASE must be a string; '~a' is not a suitable argument" base))
	      (error "EXTENSION must be a string consisting of alphanumeric characters, possibly beginning with a period '.'; '~a' is not a suitable argument" extension))
	  (error "EXTENSION must be a string (possibly the empty string); '~a' is not a suitable value" extension))
      (error "TMP-DIR must be either a string or a pathname; '~a' is not a suitable argument" tmp-dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; XSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-stylesheet (stylesheet xml-document)
  (:documentation "Apply the XSL stylesheet STYLESHEET to XML-DOCUMENT."))

(defmethod apply-stylesheet ((stylesheet string) xml-document)
  "Apply the stylesheet indicated by STYLESHEET to XML-DOCUMENT.

If STYLESHEET is the empty string, nothing will be done, and XML-DOCUMENT will be returned.  If STYLESHEET is not the empty string, its first character will be consulted.  If it is a forward slash '/', then STYLESHEET will be understood as a path to an XSL file.  If STYLESHEET is not empty and its first character is not a forward slash '/', then STYLESHEET will be understood as a string representation of an XSL stylesheet."
  (if (string= stylesheet "")
      xml-document
      (let ((first-char (char stylesheet 0)))
	(if (char= first-char #\/)
	    (apply-stylesheet (pathname stylesheet) xml-document)
	    (let ((tmp-xsl-path (temporary-file)))
	      (with-open-file (xsl tmp-xsl-path
				   :direction :output
				   :if-exists :error
				   :if-does-not-exist :create)
		(format xsl "~a" stylesheet))
	      (apply-stylesheet tmp-xsl-path xml-document)
	      (delete-file tmp-xsl-path))))))

(defmethod apply-stylesheet (stylesheet (xml-document string))
  "Apply the stylesheet indicated by STYLESHEET to XML-DOCUMENT.

If XML-DOCUMENT is the empty string, nothing will be done, and XML-DOCUMENT (viz, the empty string) will be returned.  If XML-DOCUMENT is not the empty string, its first character will be consulted.  If it is a forward slash '/', then XML-DOCUMENT will be understood as a path to an XML file.  If XML-DOCUMENT is not empty and its first character is not a forward slash '/', then XML-DOCUMENT will be understood as a string representation of an XML document."
  (if (string= xml-document "")
      xml-document
      (let ((first-char (char xml-document 0)))
	(if (char= first-char #\/)
	    (apply-stylesheet stylesheet (pathname xml-document))
	    (let ((tmp-xml-path (temporary-file)))
	      (with-open-file (xml tmp-xml-path
				   :direction :output
				   :if-exists :error
				   :if-does-not-exist :create)
		(format xml "~a" xml-document))
	      (apply-stylesheet stylesheet tmp-xml-path)
	      (delete-file tmp-xml-path))))))

(defmethod apply-stylesheet :around ((stylesheet pathname) xml-document)
  (declare (ignore xml-document))
  (if (file-exists-p stylesheet)
      (call-next-method)
      (error "There is no stylesheet at ~a" (namestring stylesheet))))

(defmethod apply-stylesheet :around (stylesheet (xml-document pathname))
  (declare (ignore stylesheet))
  (if (file-exists-p xml-document)
      (call-next-method)
      (error "There is no XML document at ~a" (namestring xml-document))))

(defmethod apply-stylesheet ((stylesheet pathname) (xml-document pathname))
  (let ((xsltproc (run-program "xsltproc"
			       (list (namestring stylesheet)
				     (namestring xml-document))
			       :search t
			       :output :stream)))
      (if (zerop (process-exit-code xsltproc))
	  (format nil "~{~a~%~}" (stream-lines (process-output xsltproc)))
	  (error "xsltproc did not exit cleanly when applying~%~%  ~a~%~%to~%~%  ~a" (namestring stylesheet) (namestring xml-document)))))

;;; utils.lisp ends here