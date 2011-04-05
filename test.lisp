
(in-package :mizar)

(defun mml-lar-included-in-bib-data ()
  "Test whether all articles mentioned in *MML-LAR* have bibliographic
information (that is, are mentioned in *ARTICLES*)."
  (let ((difference (set-difference *mml-lar*
				    (mapcar #'first *articles*))))
    (if difference
	(values nil difference)
	(values t t))))
