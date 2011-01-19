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
  (let* ((len (length str))
	 (final-char (aref str (1- len))))
    (if (eq final-char #\;)
	str
	(format nil "~A;" str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lists and sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun first-n (lst n)
  (loop 
     for i from 1 upto n
     for elt in lst
     collecting elt into items
     finally (return items)))

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

(defun remove-unneeded (list pred)
  (let ((index-of-last-unneeded (last-removable-element list pred)))
    (if index-of-last-unneeded
	(remove-unneeded (remove-nth-element list
					     index-of-last-unneeded)
			 pred)
	list)))

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
  (let ((newline (make-string 1 :initial-element #\Newline)))
    (reduce #'(lambda (s1 s2)
		(concat s1 newline s2))
	    (lines-of-file path))))

(defun stream-lines (stream)
  (let (lines)
    (symbol-macrolet
	(($line (read-line stream nil nil)))
      (do ((line $line $line))
	  ((null line))
	(push line lines)))
    (reverse lines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ensure-directory (directory)
  (namestring (pathname-as-directory directory)))

(defun file-in-directory (directory filename &optional extension)
  (concat (ensure-directory directory) filename (if (null extension)
						    ""
						    (concat "." extension))))

;;; utils.lisp ends here