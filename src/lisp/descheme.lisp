
(in-package :mizar)

(defgeneric replace-placeholders (x args))

(defmethod replace-placeholders ((x t) arguments)
  (error "How to replace placeholders for '~a'?" x))

(defmethod replace-placeholders ((x dom:text) arguments)
  (declare (ignore arguments))
  x)

(defmethod replace-placeholders ((x dom:comment) arguments)
  (declare (ignore arguments))
  x)

(defmethod replace-placeholders ((x dom:processing-instruction) arguments)
  (declare (ignore arguments))
  x)

(defmethod replace-placeholders ((x dom:element) arguments)
  (if (placeholder-term-p x)
      (let ((nr (nr x))
            (num-args (length arguments)))
        (unless nr
          (error "Placeholder term lacks an nr attribute."))
        (setf nr (parse-integer nr))
        (when (zerop nr)
          (error "Illegal zero value of the nr attribute of a placeholder term."))
        (when (< nr 1)
          (error "Illegal negative value for the nr attribute of a placeholder term."))
        (when (> nr num-args)
          (error "Illegal value ~d for the nr attribute of a placeholder term (we found ~a arguments)" nr num-args))
        (dom:clone-node (nth (1- nr) arguments) t))
      (let ((n (name x))
            (attributes (xpath:all-nodes (xpath:evaluate "@*" x))))
        (let ((new (dom:create-element (owner-document x) n)))
          (dolist (attr attributes)
            (dom:set-attribute new (dom:node-name attr) (dom:node-value attr)))
          (dolist (child (children x))
            (dom:append-child new (replace-placeholders child arguments)))
          new))))

(defgeneric resolve-local-predicates (x))

(defmethod resolve-local-predicates ((x t))
  (error "how to deal with '~a'?" x))

(defmethod resolve-local-predicates ((x dom:text))
  x)

(defmethod resolve-local-predicates ((x dom:comment))
  x)

(defmethod resolve-local-predicates ((x dom:processing-instruction))
  x)

(defmethod resolve-local-predicates ((x dom:element))
  (if (private-predicate-formula-p x)
      (let ((idnr (idnr x))
            (args (children x)))
        (let ((xpath (format nil "preceding::Item[@kind = \"Private-Predicate-Definition\" and Variable[@idnr = \"~a\"]]" idnr)))
          (break "xpath:~%~%~a" xpath)
          (let ((definition (xpath:first-node (xpath:evaluate xpath x))))
            (unless definition
              (error "Unable to find the definition for private predicate~%~%  ~a~%" x))
            (let ((definition-formula (last-element-child definition)))
              (replace-placeholders definition-formula args)))))
      (let ((new (dom:create-element (owner-document x) (name x)))
            (children (children x))
            (attributes (xpath:all-nodes (xpath:evaluate "@*" x))))
        (dolist (attr attributes)
          (dom:set-attribute new (dom:node-name attr) (dom:node-value attr)))
        (loop
           :for child :in children
           :do (dom:append-child new (resolve-local-predicates child)))
        new)))

(defgeneric descheme (thing)
  (:documentation "Eliminate all schemes from THING."))

(defmethod descheme :before ((article t))
  (without-reservations article))

(defmethod descheme :after ((article t))
  (verifier article))

(defgeneric free-variables (x))

(defmethod free-variables ((x t))
  (error "How to compute free variables for '~a'?" x))

(defmethod free-variables ((x dom:element))
  (cond ()))

(defun term-depends-on-term (term-1 term-2)
  t)

(defgeneric universally-generalize (x))

(defmethod universally-generalize ((x dom:element))
  (let ((free (free-variables x))
        (owner (owner-document x)))
    (let ((sorted-free (tsort free #'term-depends-on-term)))
      (loop
         :with final-generalization = x
         :for var :in sorted-free
         :do
         (let ((gen (make-element "Universal-Quantifier-Formula" owner))
               (segment (make-element "Explicitly-Qualified-Segment" owner))
               (variables (make-element "Variables" owner))
               (variable (make-element "Variable" owner))
               (type (lookup-type var)))
           (dom:set-attribute variable "spelling" (spelling var))
           (dom:append-child variables variable)
           (dom:append-child segment variables)
           (dom:append-child segment (dom:clone-node type t))
           (dom:append-child gen segment)
           (dom:append-child gen (dom:clone-node final-generalization t))
           (setf final-generalization gen))
         :finally (return final-generalization)))))

(defgeneric immediate-ground (x))

(defmethod immediate-ground ((x t))
  (error "How to compute the immediate grounds of '~a'?" x))

(defmethod immediate-ground ((x proposition))
  x)

(defmethod immediate-ground ((x choice-statement))
  (let ((owner (owner-document x)))
    (let ((new-existential (dom:create-element owner "Existential-Quantifier-Formula")))
      (let ((explicit-segments (xpath:all-nodes (xpath:evaluate "Explicitly-Qualified-Segment" parent)))
            (propositions (xpath:all-nodes (xpath:evaluate "Conditions/Proposition/*[position() = last()]" parent))))
        (dolist (explicit-segment explicit-segments)
          (dom:append-child new-existential
                            (dom:clone-node explicit-segment t)))
        (let ((new-conjunction (dom:create-element owner "Conjunctive-Formula")))
          (dolist (formula propositions)
            (dom:append-child new-conjunction (resolve-local-predicates formula)))
          (dom:append-child new-existential new-conjunction)))
      new-existential)))

;; Steps:
;;
;; + make the formula
;; + generate the proof of this formula
;; + make the theorem whose content is this formula
;; + replace the old scheme justification by a straightforward
;;   justification to the new theorem

(defmethod make-scheme-justified-proposition ((x scheme-justification))
  (let ((justification (justification x)))
    (universally-generalize
     (expand-private-predicates
      (immediate-ground (justification x)))))
  (let ((prev-sibling (previous-element-sibling x))
        (parent (parent x))
        (owner (owner-document x)))
    (cond ((and prev-sibling
                (string= (name prev-sibling) "Proposition"))
           (clone (last-element-child prev-sibling)))
          ((and parent
                (string= (name parent) "Item")
                (has-attribute parent "kind")
                (string= (kind parent) "Choice-Statement"))
           (let ((new-existential (dom:create-element owner "Existential-Quantifier-Formula")))
             (let ((explicit-segments (xpath:all-nodes (xpath:evaluate "Explicitly-Qualified-Segment" parent)))
                   (propositions (xpath:all-nodes (xpath:evaluate "Conditions/Proposition/*[position() = last()]" parent))))
               (dolist (explicit-segment explicit-segments)
                 (dom:append-child new-existential
                                   (dom:clone-node explicit-segment t)))
               (let ((new-conjunction (dom:create-element owner "Conjunctive-Formula")))
                 (dolist (formula propositions)
                   (dom:append-child new-conjunction (resolve-local-predicates formula)))
                 (dom:append-child new-existential new-conjunction)))
             new-existential))
          (t
           (error "Don't know how to handle this scheme justification node.")))))

(defmethod make-scheme-justified-proposition ((x dom:element))
  (let ((prev-sibling (previous-element-sibling x))
        (parent (parent x))
        (owner (owner-document x)))
    (cond ((and prev-sibling
                (string= (name prev-sibling) "Proposition"))
           (clone (last-element-child prev-sibling)))
          ((and parent
                (string= (name parent) "Item")
                (has-attribute parent "kind")
                (string= (kind parent) "Choice-Statement"))
           (let ((new-existential (dom:create-element owner "Existential-Quantifier-Formula")))
             (let ((explicit-segments (xpath:all-nodes (xpath:evaluate "Explicitly-Qualified-Segment" parent)))
                   (propositions (xpath:all-nodes (xpath:evaluate "Conditions/Proposition/*[position() = last()]" parent))))
               (dolist (explicit-segment explicit-segments)
                 (dom:append-child new-existential
                                   (dom:clone-node explicit-segment t)))
               (let ((new-conjunction (dom:create-element owner "Conjunctive-Formula")))
                 (dolist (formula propositions)
                   (dom:append-child new-conjunction (resolve-local-predicates formula)))
                 (dom:append-child new-existential new-conjunction)))
             new-existential))
          (t
           (error "Don't know how to handle this scheme justification node.")))))

(defmethod make-scheme-justified-theorem :around ((x dom:element))
  (if (scheme-justification-p x)
      (call-next-method)
      (error "Don't know how to make scheme justifications for nodes of type '~a'." (dom:node-name x))))

(defmethod make-scheme-justified-theorem ((x dom:element))
  (let ((content (make-scheme-justified-proposition x))
        (owner (owner-document x)))
    (let ((proof-block-node (dom:create-element owner "Block")))
      (dom:set-attribute proof-block-node "kind" "Proof")
      (make-unexported-theorem-node (make-proposition-node content "SchemeInstance")
                                    proof-block-node))))

(defmethod descheme ((article pathname))
  (let ((msx (file-with-extension article "msx"))
        (tpr (file-with-extension article "tpr")))
    (loop
       :with label = "SchemeInstance"
       :with xpath = "descendant::Scheme-Justification[not(ancestor::Item[@kind = \"Scheme-Block-Item\"]) and not(ancestor::*[parent::Text-Proper and preceding-sibling::Item[@kind = \"Pragma\" and @spelling = \"$C scheme instance\"]])][1]"
       :for msx-root = (parse-xml-file msx)
       :for msx-doc = (dom:owner-document msx-root)
       :for first-scheme = (xpath:first-node (xpath:evaluate xpath msx-root))
       :until (null first-scheme)
       :do
       (let ((proposition (previous-element-sibling first-scheme))
             (toplevel-item (toplevel-ancestor first-scheme)))
         (unless proposition
           (error "Missing previous sibling of the scheme justification!"))
         (unless toplevel-item
           (error "Missing toplevel item!"))
         (let ((new-pragma-node (make-pragma "$C scheme instance" msx-doc)))
           (dom:insert-before msx-root new-pragma-node toplevel-item)
           (let ((new-theorem (make-scheme-justified-theorem first-scheme)))
             (dom:insert-before msx-root new-theorem toplevel-item)
             (dom:replace-child (parent first-scheme)
                                (make-straightforward-justification-node msx-doc (make-local-reference-node label msx-doc))
                                first-scheme)
             ;; write the new .msx to disk
             (write-string-into-file (with-output-to-string (s)
                                       (dom:map-document (cxml:make-character-stream-sink s) msx-doc)) msx :if-exists :supersede)
             (break "did one round!")
             (apply-stylesheet *pp-stylesheet* msx nil tpr)
             (mglue article)))))))
