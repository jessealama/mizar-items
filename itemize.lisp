;;; itemize.lisp Break up mizar articles into bits

(in-package :mizar)

(defun article-uses-article (article-1 article-2)
  (member article-1 (complete-environment article-2)))

(defun itemize (&rest articles)
  (let ((articles (sort articles #'article-uses-article)))
    articles))

;;; itemize.lisp ends here