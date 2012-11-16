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
  (format nil "~@:(~a~)" str))

(defun lowercase (str)
  (format nil "~(~a~)" str))

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

(defun delete-space (string)
  (delete #\Space string))

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

(defun minimal-sublist-satisfying (list predicate)
  (let ((needed-table (make-hash-table :test #'eql))
	(length (length list)))
    ;; initially, everything is needed
    (loop
       for i from 0 upto (1- length)
       do
	 (setf (gethash i needed-table) t))
    (let ((needed (minimal-sublist-satisfying-1 list
				  predicate
				  needed-table
				  0
				  (1- length))))
      (hash-table-keys needed))))

(defun list-terms-from-table (list needed-table)
  (loop
     with new-list = nil
     for i from 0 upto (1- (length list))
     for item in list
     do
       (when (present-in-table? i needed-table)
	 (push item new-list))
     finally
       (return (reverse new-list))))

(defun minimal-sublist-satisfying-1 (list predicate needed-table begin end)
  (flet ((clear-range (a b)
	   (loop for i from a upto b do (remhash i needed-table)))
	 (restore-range (a b)
	   (loop for i from a upto b do (setf (gethash i needed-table) t)))
	 (testing ()
	   (loop
	      for i from 0 upto (1- (length list))
	      initially (format t "testing [")
	      do
		(if (present-in-table? i needed-table)
		    (format t "+")
		    (format t "-"))
	      finally (format t "] (~d,~d)~%" begin end))))
    (cond ((< end begin)
	   needed-table)
	  ((= end begin)
	   (clear-range begin end)
	   (if (funcall predicate (hash-table-keys needed-table))
	       (progn
		 ;; (format t "good~%")
		 ;; (format t "Element ~d can be dumped.~%" begin)
		 )
	       (progn
		 ;; (format t "bad~%")
		 ;; (format t "Element ~d cannot be dumped.~%" begin)
		 (restore-range begin end)))
	   needed-table)
	  ((= end (1+ begin))
	   (clear-range begin begin)
	   (if (funcall predicate (hash-table-keys needed-table))
	       (progn
		 (clear-range end end)
		 (unless (funcall predicate (hash-table-keys needed-table))
		   (restore-range end end)))
	       (progn
		 (restore-range begin begin)
		 (clear-range end end)
		 (unless (funcall predicate (hash-table-keys needed-table))
		   (restore-range end end))))
	   needed-table)
	  ((< begin end)
	   (let* ((width (- end begin))
		  (half (floor (/ width 2)))
		  (midpoint (+ begin half)))
	     (clear-range begin midpoint)
	     (if (funcall predicate (hash-table-keys needed-table))
		 (progn
		   ;; (format t "good~%")
		   ;; (format t "Every element from ~d to ~d can be dumped.~%" begin end)
		   (minimal-sublist-satisfying-1 list
						 predicate
						 needed-table
						 (1+ midpoint)
						 end))
		 (progn
		   ;; (format t "bad~%")
		   ;; (format t "Not every element from ~d to ~d can be dumped.~%" begin end)
		   (restore-range begin midpoint)
		   (let ((needed-bottom-half
			  (minimal-sublist-satisfying-1 list
							predicate
							needed-table
							begin
							midpoint)))
		     (minimal-sublist-satisfying-1 list
						   predicate
						   needed-bottom-half
						   (1+ midpoint)
						   end)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Iteration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro until (condition &body body)
  `(do nil (,condition) ,@body))

(defmacro while (condition &body body)
  `(do nil ((not ,condition)) ,@body))

(defun expand-forms (forms)
  (if forms
      (if (rest forms)
	  (list 'if (first forms) (expand-forms (cdr forms)) (list 'error "The form~%~%  ~a~%~%evaluated to NIL." (list 'quote (first forms))))
	  (list 'unless (first forms) (list 'error "The form~%~%  ~a~%~%evaluated to NIL." (list 'quote (first forms)))))
      (list t)))

(defmacro stop-if-nil (&body body)
  `,(expand-forms body))

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

(defun present-in-table? (key table)
  (multiple-value-bind (dummy present?)
      (gethash key table)
    (declare (ignore dummy))
    present?))

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
			  (if (directory-pathname-p real-tmp-dir)
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
;;; Paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ensure-directory (directory)
  (namestring (pathname-as-directory directory)))

(defun file-in-directory (directory filename &optional extension)
  (concat (ensure-directory directory) filename (if (null extension)
						    ""
						    (concat "." extension))))

(defgeneric file-has-extension (file extension)
  (:documentation "Determine whether FILE has the extension EXTENSION.
EXTENSION can optionally begin with a full-stop '.'.  This utility does not check whether FILE actually exists; it only checks whether the extension of FILE (whether it exists or not) is equal to the specified extension.  If the file has no extension at all, then this function will return NIL no matter what EXTENSION is.  If the name of FILE ends with a full-stop, then the empty string is the only value of EXTENSION for which this utility will return T."))

(defmethod file-has-extension ((file-string string) extension)
  (file-has-extension (pathname file-string) extension))

(defmethod file-has-extension ((file pathname) (extension string))
  (register-groups-bind (extension-sans-period)
      ("^\\.?(.*)$" extension)
    (let ((file-ns (file-namestring file)))
      (register-groups-bind (ext)
	  ("\\.(.*)$" file-ns)
	(string= ext extension-sans-period)))))

(defun files-in-directory-with-extension (directory extension)
  (let ((dir (pathname (pathname-as-directory directory))))
    (let ((files (directory (format nil "~a*.~a" dir extension))))
      (let ((sorted-files (sort files #'< :key #'file-write-date)))
	sorted-files))))

(defun native-namestring (path)
  #+ccl
  (ccl:native-translated-namestring xml-path-1)
  #+sbcl
  (sb-ext:native-namestring path)
  #-(or ccl sbcl)
  (error "We can compute native pathnames only for CCL and SBCL."))

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
;;; Token lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tokenize (list)
  (format nil ",~{~a~^,~}," list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; XML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-xml-file (path)
  (let ((doc (cxml:parse-file path (cxml-dom:make-dom-builder))))
    (dom:child-nodes (dom:document-element doc))))

;;; utils.lisp ends here
