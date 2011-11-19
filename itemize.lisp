;;; itemize.lisp Break up mizar articles into bits

(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Itemization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric xsl-split-article (article)
  (:documentation "Divide any 'multi-part' elements of ARTICLE.  This means:

* divide definition blocks that define multiple things into several 'singleton' definition blocks,
* likewise for notation blocks,
* divide reservations that reserve multiple variables into 'singleton' reservations"))

(defmethod xsl-split-article ((article article))
  (error "We haven't yet defined XSL-SPLIT-ARTICLE for objects of class ARTICLE.  Sorry."))

(defmethod xsl-split-article ((article string))
  "Split an article given as a string.

If ARTICLE is the empty string, signal an error.  If ARTICLE is not the empty string, look at its first character.  If the first character is a forward slash '/', then interpret ARTICLE as a path to an article file on disk, and proceed accordingly.  Otherwise, interpret ARTICLE as the string representation of a MIZAR article, save the article to a temporary location on disk, and proceed as if ARTICLE were that file on disk."
  (if (string= article "")
      (error "We cannot split an empty article!")
      (let ((first-char (char article 0)))
        (if (char= first-char #\/)
            (xsl-split-article (pathname article))
            (let ((temp-article-path (temporary-file :extension ".miz")))
              (with-open-file (temp-article temp-article-path
                                            :direction :output
                                            :if-exists :error
                                            :if-does-not-exist :create)
                (format temp-article "~a" article))
              (xsl-split-article temp-article-path)
              (delete-file temp-article-path))))))

(defmethod xsl-split-article ((article pathname))
  (loop
     with toplevel-dellink-stylesheet = (mizar-items-config 'toplevel-dellink-stylesheet)
     with split-stylesheet = (mizar-items-config 'split-stylesheet)
     with schedule = (list toplevel-dellink-stylesheet
                           toplevel-dellink-stylesheet ;; need to do
                                                       ;; this twice,
                                                       ;; if there are
                                                       ;; toplevel
                                                       ;; links
                           split-stylesheet)
     with xml = (replace-extension article "miz" "wsx")
     for sheet in schedule
     do
       (format t "Applying ~a...~%" sheet)
       (setf xml (apply-stylesheet sheet xml nil nil))
     finally
       (return xml)))

(defun xsl-itemize-article (article)
  (let ((itemize-stylesheet (mizar-items-config 'itemize-stylesheet)))
    (apply-stylesheet itemize-stylesheet
                      (xsl-split-article article)
                      nil
                      nil)))

(defun ckb-< (ckb-path-1 ckb-path-2)
  (let ((ckb-pattern "^ckb([0-9]+)$"))
    (register-groups-bind (ckb-num-1-as-str)
        (ckb-pattern (pathname-name ckb-path-1))
      (register-groups-bind (ckb-num-2-as-str)
          (ckb-pattern (pathname-name ckb-path-2))
        (let ((ckb-num-1 (parse-integer ckb-num-1-as-str))
              (ckb-num-2 (parse-integer ckb-num-2-as-str)))
          (< ckb-num-1 ckb-num-2))))))

(defgeneric extend-evl (evl-file prel-dir dict-dir)
  (:documentation "Extend the .evl file EVL-FILE with whatever the contents of PREL-DIR and DICT-DIR are.  If, for example, there is a file 'foo.sch' in PREL-DIR, then EVL-FILE will be extended so that, in its Schemes directives, we find 'FOO' as an Ident.  If there is a .voc file in DICT-DIR, it will be added to the Vocabularies directive as well."))

(defmethod extend-evl ((evl-file string) prel-dir dict-dir)
  (extend-evl (pathname evl-file) prel-dir dict-dir))

(defmethod extend-evl (evl-file (prel-dir string) dict-dir)
  (extend-evl evl-file (pathname prel-dir) dict-dir))

(defmethod extend-evl (evl-file prel-dir (dict-dir string))
  (extend-evl evl-file prel-dir (pathname dict-dir)))

(defmethod extend-evl :around ((evl-file pathname) (prel-dir pathname) (dict-dir pathname))
  (if (file-exists-p evl-file)
      (when (file-exists-p prel-dir)
        (if (directory-p prel-dir)
            (when (file-exists-p prel-dir)
              (if (directory-p prel-dir)
                  (call-next-method)
                  (error "The specified dict directory, '~a', isn't actually a directory" (namestring dict-dir))))
            (error "The specified prel DB, '~a', isn't actually a directory" (namestring prel-dir)))
        (call-next-method))
      (error "The specified .evl file, '~a', doesn't exist" (namestring evl-file))))

(defmethod extend-evl ((evl-file pathname) (prel-dir pathname) (dict-dir pathname))
  (if (file-exists-p prel-dir)
      (let ((more-vocabularies "")
            (more-notations "")
            (more-definitions "")
            (more-theorems "")
            (more-schemes "")
            (more-registrations "")
            (more-constructors "")
            (more-requirements ""))
        (flet ((pad-string (string new-bit)
                 (format nil "~a~a," string (uppercase new-bit))))
          (flet ((add-to-vocabularies (article)
                   (setf more-vocabularies (pad-string more-vocabularies article)))
                 (add-to-notations (article)
                   (setf more-notations (pad-string more-notations article)))
                 (add-to-definitions (article)
                   (setf more-definitions (pad-string more-definitions article)))
                 (add-to-theorems (article)
                   (setf more-theorems (pad-string more-theorems article)))
                 (add-to-schemes (article)
                   (setf more-schemes (pad-string more-schemes article)))
                 (add-to-registrations (article)
                   (setf more-registrations (pad-string more-registrations article)))
                 (add-to-constructors (article)
                   (setf more-constructors (pad-string more-constructors article)))
                 (add-to-requirements (article)
                   (setf more-requirements (pad-string more-requirements article))))
            (flet ((dispatch-exported-file (path)
                     (cond ((voc-file-p path) (add-to-vocabularies (pathname-name path)))
                           ((dno-file-p path) (add-to-notations (pathname-name path)))
                           ((drd-file-p path) (add-to-registrations (pathname-name path)))
                           ((dcl-file-p path) (add-to-registrations (pathname-name path)))
                           ((eid-file-p path) (add-to-registrations (pathname-name path)))
                           ((did-file-p path) (add-to-registrations (pathname-name path)))
                           ((sch-file-p path) (add-to-schemes (pathname-name path)))
                           ((dco-file-p path) (add-to-constructors (pathname-name path)))
                           ((def-file-p path) (add-to-definitions (pathname-name path)))
                           ((the-file-p path) (add-to-theorems (pathname-name path)))
                           (t
                            (error "Don't know how to deal with the file '~a'" (namestring path))))))
              ;; prel
              (loop
                 for extension in (list "dno" "dcl" "eid" "did" "sch" "def" "dco" "the")
                 do
                   (loop
                      with files = (files-in-directory-with-extension prel-dir extension)
                      with sorted-files = (sort files #'ckb-<)
                      for path in sorted-files
                      do (dispatch-exported-file path)))
              ;; voc
              (loop
                 with files = (files-in-directory-with-extension dict-dir "voc")
                 with sorted-files = (sort files #'ckb-<)
                 for path in sorted-files
                 do (dispatch-exported-file path)))))
        (apply-stylesheet (mizar-items-config 'extend-evl-stylesheet)
                          evl-file
                          (list (cons "vocabularies" more-vocabularies)
                                (cons "notations" more-notations)
                                (cons "definitions" more-definitions)
                                (cons "theorems" more-theorems)
                                (cons "schemes" more-schemes)
                                (cons "registrations" more-registrations)
                                (cons "constructors" more-constructors)
                                (cons "requirements" more-requirements))
                          nil))
      (apply-stylesheet (mizar-items-config 'extend-evl-stylesheet)
                      evl-file
                      nil
                      nil)))

(defun write-xml-node-to-file (xml-node path)
  (with-open-file (xml-out path
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :supersede
                           :element-type '(unsigned-byte 8))
;;                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;                         Watch out: omitting this key can lead to trouble
    (dom:map-document (cxml:make-octet-stream-sink xml-out)
                      xml-node)))

(defgeneric itemize (article))

(defmethod itemize :around ((article-path pathname))
  (if (file-exists-p article-path)
      (call-next-method)
      (error "There is no article at ~a" article-path)))

(defmethod itemize ((article-string string))
  (if (string= article-string "")
      0
      (let ((first-char (char article-string 0)))
        (if (char= first-char #\/)
            (itemize (pathname article-string))
            (let ((temp-article (temporary-file :base "a" :extension "miz")))
              (write-string-into-file article-string temp-article
                                      :if-exists :error
                                      :if-does-not-exist :create)
              (prog1
                  (itemize temp-article)
                (delete-file temp-article)))))))

(defmethod itemize :before ((article-path pathname))
  (let* ((article-name (pathname-name article-path))
         (items-dir (format nil "/~{~a/~}~a/"
                            (cdr (pathname-directory article-path))
                            article-name))
         (prel-dir (format nil "~aprel/" items-dir))
         (dict-dir (format nil "~adict/" items-dir))
         (text-dir (format nil "~atext/" items-dir))
         (evl2environ-stylesheet (mizar-items-config 'evl2environ-stylesheet))
         (article-in-items-dir (format nil "~a~a.miz" items-dir article-name))
         (evl-file (format nil "~a~a.evl" items-dir article-name))
         (msm-article (format nil "~a~a.msm" items-dir article-name)))
    (handler-case (progn (ensure-directories-exist items-dir)
                         (ensure-directories-exist prel-dir)
                         (ensure-directories-exist dict-dir)
                         (ensure-directories-exist text-dir))
      (file-error () (error "We cannot ensure that the directories~%
* ~a~%  * ~a~%  * ~a~%  * ~a~%~%exist,
so we cannot proceed." items-dir prel-dir dict-dir text-dir)))
    (copy-file article-path article-in-items-dir)
    (accom article-in-items-dir :flags '("-q" "-l"))
    (let ((environ (apply-stylesheet evl2environ-stylesheet
                                    evl-file
                                    nil
                                    nil)))
      (wsmparser article-in-items-dir :flags '("-q" "-l"))
      (msmprocessor article-in-items-dir :flags '("-q" "-l"))
      (with-open-file (new-miz article-in-items-dir
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :error)
        (format new-miz "~a" environ)
        (terpri new-miz)
        (with-open-file (msm-miz msm-article
                                 :direction :input
                                 :if-does-not-exist :error)
          (loop
             for line = (read-line msm-miz nil nil)
             do
               (if line
                   (format new-miz "~a~%" line)
                   (return)))))
      (accom article-in-items-dir :flags '("-q" "-l"))
      (wsmparser article-in-items-dir :flags '("-q" "-l")))))

(defmethod itemize ((article-path pathname))
  (let* ((article-name (pathname-name article-path))
         (items-dir (format nil "/~{~a/~}~a/"
                            (cdr (pathname-directory article-path))
                            article-name))
         (article-in-items-dir (format nil "~a~a.miz" items-dir article-name))
         (itemized-article (xsl-itemize-article article-in-items-dir))
         (xml-doc (handler-case (cxml:parse itemized-article
                                            (cxml-dom:make-dom-builder))
                    (error (err) (error "There was an error parsing the
result of itemizing the article at~%~%  ~a;~%~%The error was: ~a" article-in-items-dir err))))
         (evl-file (replace-extension article-in-items-dir "miz" "evl"))
         (wsm-stylesheet (mizar-items-config 'wsm-stylesheet))
         (evl2environ-stylesheet (mizar-items-config 'evl2environ-stylesheet))
         (prel-dir (format nil "~aprel/" items-dir))
         (dict-dir (format nil "~adict/" items-dir))
         (text-dir (format nil "~atext/" items-dir)))
    (loop
       with all-nodes = (xpath:all-nodes (xpath:evaluate "Fragments/Text-Proper" xml-doc))
       with context-nodes = nil
       for i from 1
       for needed-context = (copy-list context-nodes)
       for item-node in all-nodes
       for doc = (rune-dom:create-document item-node)
       for item-path = (format nil "~a~d.wsi" items-dir i)
       for fragment-miz-path = (format nil "~ackb~d.miz" text-dir i)
       for fragment-evl-path = (format nil "~a~d.evl" items-dir i)
       for extended-evl = (extend-evl evl-file prel-dir dict-dir)
       do
         ;; first record whether this is a "context" node that might
         ;; need to be prepended to some later item
         (when (not (xpath:node-set-empty-p (xpath:evaluate "Item[@kind = \"Reservation\" or @kind = \"Constant-Definition\" or @kind = \"Regular-Statement\" or @kind = \"Type-Changing-Statement\" or @kind = \"Choice-Statement\" or @kind = \"Private-Predicate-Definition\" or @kind = \"Private-Functor-Definition\"]" item-node)))
           (setf context-nodes (append context-nodes (list item-node))))
         (write-xml-node-to-file doc item-path)
         (write-string-into-file extended-evl fragment-evl-path
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
         (let ((bundle-miz-environ (apply-stylesheet evl2environ-stylesheet
                                                     fragment-evl-path
                                                     nil
                                                     nil))
               (bundle-miz-text (apply-stylesheet wsm-stylesheet
                                                  item-path
                                                  (list
                                                   (cons "suppress-environment" "1")
                                                   (cons "evl" (namestring fragment-evl-path)))
                                                  nil)))

           (with-open-file (bundle-miz fragment-miz-path
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
             (format bundle-miz "~a" bundle-miz-environ)
             (terpri bundle-miz)
             (loop
                for context-node in context-nodes
                for pos = (position context-node all-nodes)
                for previous-fragment-path = (format nil "~a~d.wsi" items-dir (1+ pos))
                for previous-fragment-text = (apply-stylesheet wsm-stylesheet
                                                               previous-fragment-path
                                                               (list (cons "suppress-environment" "1"))
                                                               nil)
                do
                  (format bundle-miz "~a" previous-fragment-text)
                  (terpri bundle-miz))
             (format bundle-miz "~a" bundle-miz-text))
           (accom fragment-miz-path :working-directory items-dir :flags '("-q" "-l"))
           (verifier fragment-miz-path :working-directory items-dir :flags '("-q" "-l"))
           (handler-case (progn (exporter fragment-miz-path :working-directory items-dir :flags '("-q" "-l"))
                                (transfer fragment-miz-path :working-directory items-dir :flags '("-q" "-l")))
             (mizar-error ()
               (setf context-nodes (append context-nodes (list item-node)))))
           ;; now minimize the set of context nodes.  trivial
           ;; algorithm: delete items from the end
           (loop
              ;; initially (format t "Minimizing item ~d...~%" i)
              for context-node in (reverse (remove item-node context-nodes))
              do
                (let ((trimmed (remove context-node needed-context)))
                  (with-open-file (bundle-miz fragment-miz-path
                                              :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create)
                    (format bundle-miz "~a" bundle-miz-environ)
                    (terpri bundle-miz)
                    (loop
                       for context-node in trimmed
                       for pos = (position context-node all-nodes)
                       for previous-fragment-path = (format nil "~a~d.wsi" items-dir (1+ pos))
                       for previous-fragment-text = (apply-stylesheet wsm-stylesheet
                                                                      previous-fragment-path
                                                                      (list (cons "suppress-environment" "1"))
                                                                      nil)
                       do
                         (format bundle-miz "~a" previous-fragment-text)
                         (terpri bundle-miz))
                    (format bundle-miz "~a" bundle-miz-text))
                  (handler-case (progn (verifier fragment-miz-path :working-directory items-dir :flags '("-q" "-l"))
                                       (setf needed-context trimmed))
                    (mizar-error () nil)))
              finally
                ;; ensure that at the end we write the minimized
                ;; context
                (with-open-file (bundle-miz fragment-miz-path
                                              :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create)
                    (format bundle-miz "~a" bundle-miz-environ)
                    (terpri bundle-miz)
                    (loop
                       for context-node in needed-context
                       for pos = (position context-node all-nodes)
                       for previous-fragment-path = (format nil "~a~d.wsi" items-dir (1+ pos))
                       for previous-fragment-text = (apply-stylesheet wsm-stylesheet
                                                                      previous-fragment-path
                                                                      (list (cons "suppress-environment" "1"))
                                                                      nil)
                       do
                         (format bundle-miz "~a" previous-fragment-text)
                         (terpri bundle-miz))
                    (format bundle-miz "~a" bundle-miz-text)))))
    t))

(defun itemize-no-errors (article)
  (handler-case
      (progn
        (handler-bind ((warning #'muffle-warning))
          (itemize article))
        (format t "~a: success~%" article)
        t)
      (error ()
        (format *error-output* "~a: failure~%" article)
        nil)))

;;; itemize.lisp ends here