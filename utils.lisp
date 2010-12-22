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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lists
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

;;; utils.lisp ends here