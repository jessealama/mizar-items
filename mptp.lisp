
(in-package :mizar)

(defun data-directory-for-mml (mml-version)
  (format nil "~a/~a"
	  *mizar-items-data-root*
	  mml-version))

(defun mptp-axiom-file-for-mml-version (mml-version)
  (let ((data-directory (data-directory-for-mml mml-version)))
    (if (file-exists-p data-directory)
	(format nil "~a/mptp-axioms" data-directory)
	(error "There is no data directory in the expected location '~a'" data-directory))))

(define-constant +mptp-axiom-line-regexp+
    "^fof\\(([^,]+), axiom, (.+)\\)\.$"
  :test #'string=
  :documentation "A regular expression that matches lines contained in
the result of exporting the MML to MPTP.")

(defun load-mptp-axioms-for-mml (mml-version)
  (let ((axiom-file (mptp-axiom-file-for-mml-version mml-version)))
    (if (file-exists-p axiom-file)
	(let ((mptp-table (make-hash-table :test #'equal)))
	  (with-open-file (axioms axiom-file
				  :direction :input
				  :if-does-not-exist :error)
	    (loop
	       for line = (read-line axioms nil)
	       do
		 (if line
		     (register-groups-bind (formula-name formula)
			 (+mptp-axiom-line-regexp+ line)
		       (setf (gethash formula-name mptp-table) formula))   
		     (return mptp-table))))))))

(defun article-of-mptp-formula-name (mptp-formula-name)
  (register-groups-bind (article)
      ("_([a-z0-9_]+)$" mptp-formula-name)
    article))

(defun item-of-mptp-formula-name (mptp-formula-name)
  (let* ((article (article-of-mptp-formula-name mptp-formula-name))
	 (article-len (length article))
	 (len (length mptp-formula-name)))
    (subseq mptp-formula-name 0 (1- (- len article-len)))))

(defgeneric mptp-name-of-item (item))

(defmethod mptp-name-of-item ((item symbol))
  (mptp-name-of-item (symbol-name item)))

(defmethod mptp-name-of-item ((item string))
  (destructuring-bind (article kind number)
      (split ":" item)
    (cond ((string= kind "theorem") (format nil "t~a_~a" number article))
	  ((string= kind "lemma") nil)
	  ((string= kind "deftheorem") (format nil "d~a_~a" number article))
	  ((string= kind "scheme") (format nil "s~a_~a" number article))
	  ((scan ".pattern" kind) nil) ; no semantic content
	  ((scan ".constructor" kind) nil) ;; not directly accounted for
	  ((string= kind "definiens")
	   (let* ((corresponding-deftheorem (deftheorem-from-definiens item))
		  (corresponding-article (item-article corresponding-deftheorem))
		  (corresponding-number (item-number corresponding-deftheorem)))
	     (format nil "d~a_~a" corresponding-number corresponding-article)))
	  ((string= kind "ccluster") (format nil "cc~a_~a" number article))
	  ((string= kind "fcluster") (format nil "fc~a_~a" number article))
	  ((string= kind "rcluster") (format nil "rc~a_~a" number article))

	  (t (error "Don't know how to map '~a' into the MPTP namespace" item)))))

(defgeneric constructor-item? (item))

(defmethod constructor-item? ((item symbol))
  (constructor-item? (symbol-name item)))

(defmethod constructor-item? ((item string))
  (scan "[^:]+:.constructor:[0-9]+" item))

(defun constructor-kind (constructor-item)
  (register-groups-bind (kind)
      ("^[^:]+:(.)constructor:[0-9]+$" constructor-item)
    kind))

(defun constructor-number (constructor-item)
  (register-groups-bind (number)
      ("^[^:]+:.constructor:([0-9]+)$" constructor-item)
    number))

(defgeneric auxiliary-mptp-items-for-item (item mptp-table))

(let ((table (make-hash-table :test #'equal)))
  (defmethod auxiliary-mptp-items-for-item :around (item mptp-table)
    (let ((key (cons item mptp-table)))
      (multiple-value-bind (stored known?)
	  (gethash key table)
	(if known?
	    stored
	    (let ((val (call-next-method)))
	      (setf (gethash key table) val)))))))

(defmethod auxiliary-mptp-items-for-item ((item symbol) mptp-table)
  (auxiliary-mptp-items-for-item (symbol-name item) mptp-table))

(defmethod auxiliary-mptp-items-for-item ((item string) mptp-table)
  (cond ((constructor-item? item)
	 (let ((kind (constructor-kind item))
	       (number (constructor-number item))
	       (article (item-article item)))
	   (loop
	      with extras = nil
	      for item being the hash-keys in mptp-table using (hash-value formula)
	      do
		(when (scan (format nil "_~a~a_~a$" kind number article) item)
		  (push (cons item formula) extras))
	      finally
		(return extras))))
	;; ((definiens-item? item)
	;;  (let* ((corresponding-deftheorem (deftheorem-from-definiens item))
	;; 	(corresponding-article (item-article corresponding-deftheorem))
	;; 	(corresponding-number (item-number corresponding-deftheorem))
	;; 	(deftheorem-mptp-name (format nil "d~a_~a" corresponding-number corresponding-article))
	;; 	(deftheorem-mptp-formula (gethash deftheorem-mptp-name mptp-table)))
	;;    (list (cons deftheorem-mptp-name deftheorem-mptp-formula))))
	(t nil)))

(defun mptp-formulas-for-item (item mptp-table)
  (let ((mptp-name (mptp-name-of-item item)))
    (if mptp-name
	(multiple-value-bind (formula present?)
	    (gethash mptp-name mptp-table)
	  (if present?
	      (cons (list mptp-name formula) 
		    (auxiliary-mptp-items-for-item item mptp-table))
	      (error "An entry for the key '~a' is not present in the given MPTP table" mptp-name)))
	(auxiliary-mptp-items-for-item item mptp-table))))

(define-constant +system-on-tptp-form-uri+
  "http://www.cs.miami.edu/~tptp/cgi-bin/SystemOnTPTPFormReply"
  :test #'string=
  :documentation "The URI of the SystemOnTPTP resource to which we post problems.")

(defun system-on-tptp (formula)
  (let ((tptp-formula (format nil "fof(dummy, conjecture, ~a)." formula)))
    (drakma:http-request +system-on-tptp-form-uri+
			 :method :post
			 :parameters (list
				      '("QuietFlag" . "-q01")
				      '("SubmitButton" . "RunSelectedSystems")
				      '("AutoModeTimeLimit" . "300")
				      '("ProblemSource" . "FORMULAE")
				      (cons "FORMULAEProblem"  tptp-formula)
				      '("System___Paradox---4.0" . "Paradox---4.0")))))

(defun named-formula-as-tptp-axiom (name formula)
  (format nil "fof(~a, axiom, ~a)." name formula))

(defun named-formula-as-tptp-conjecture (name formula)
  (format nil "fof(~a, conjecture, ~a)." name formula))

(defun dependence-problem (item needed-items mptp-table)
  (flet ((mptp-formula (item)
	   (mptp-formulas-for-item item mptp-table))
	 (formula-as-axiom (name-formula-pair)
	   (destructuring-bind (name . formula)
	       name-formula-pair
	     (named-formula-as-tptp-axiom name formula))))
    (let* ((deps-named-formulas (remove-duplicates (reduce #'append (mapcar #'mptp-formula
									    needed-items))
						   :test #'string=
						   :key #'car))
 	   (deps-axioms (mapcar #'formula-as-axiom deps-named-formulas))
	   (conjecture-name (mptp-name-of-item item))
	   (conjecture-formula (gethash conjecture-name mptp-table))
	   (conjecture (named-formula-as-tptp-conjecture conjecture-name conjecture-formula)))
      (cons conjecture deps-axioms))))

(defgeneric item-dependencies (item))

(defmethod item-dependencies ((item symbol))
  (gethash item *item-dependency-graph-forward*))

(defmethod item-dependencies ((item string))
  (item-dependencies (get-and-maybe-set-item-name item)))

(defgeneric item-supports (item))

(defmethod item-supports ((item symbol))
  (gethash item *item-dependency-graph-backward*))

(defmethod item-supports ((item string))
  (item-supports (get-and-maybe-set-item-name item)))

(defgeneric verify-immediate-dependence-problem (item mptp-table))

(defmethod verify-immediate-dependence-problem ((item symbol) mptp-table)
  (dependence-problem item
		      (item-dependencies item)
		      mptp-table))

(defmethod verify-immediate-dependence-problem ((item string) mptp-table)
  (verify-immediate-dependence-problem (get-and-maybe-set-item-name item)
				       mptp-table))

(defgeneric verify-transitive-dependence-problem (item mptp-table))

(defmethod verify-transitive-dependence-problem ((item string) mptp-table)
  (verify-transitive-dependence-problem (get-and-maybe-set-item-name item)
					mptp-table))

(defmethod verify-transitive-dependence-problem ((item symbol) mptp-table)
  (let ((all-deps-table (dependencies-generated-by-item item)))
    (dependence-problem item
			(remove item (hash-table-keys all-deps-table))
			mptp-table)))

(defgeneric necessity-of-item-for-item (item needed-item mptp-table)
  (:documentation "Generate an ATP problem in which we try to show that NEEDED-ITEM is
necessary for ITEM.  We do this by simply removing NEEDED-ITEM from
the list of items on which ITEM immediately depends."))

(defmethod necessity-of-item-for-item ((item string) needed-item mptp-table)
  (necessity-of-item-for-item (get-and-maybe-set-item-name item) needed-item mptp-table))

(defmethod necessity-of-item-for-item (item (needed-item string) mptp-table)
  (necessity-of-item-for-item item (get-and-maybe-set-item-name needed-item) mptp-table))

(defmethod necessity-of-item-for-item ((item symbol) (needed-item symbol) mptp-table)
  (dependence-problem item
		      (remove needed-item
			      (item-dependencies item)
			      :test #'string=)
		      mptp-table))

(defun necessity-of-items-for-item (item needed-items mptp-table)
  (dependence-problem item
		      (remove-if #'(lambda (thing)
				     (member thing needed-items :test #'string=))
				 (item-dependencies item))
		      mptp-table))
