;;; item.lisp Bits of mizar articles

(in-package :mizar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass item ()
  ((source-article
    :initarg :source-article
    :accessor source-article)
   (begin-line-number
    :initarg :begin-line-number
    :accessor begin-line-number)
   (end-line-number
    :initarg :end-line-number
    :accessor end-line-number)
   (begin-column-number
    :initarg :begin-column-number
    :accessor begin-column-number)
   (end-column-number
    :initarg :end-column-number
    :accessor end-column-number)
   (text
    :initarg :text
    :accessor text))
  (:documentation "A mizar item represents a free-standing piece of a
  mizar article, such as a definition, a theorem, a notation, or a registration."))

(defmethod initialize-instance :after ((item item) &key)
  "If we know the bounds and the soruce article, compute the text (if not already set)."
  (when (and (slot-boundp item 'source-article)
	     (slot-boundp item 'begin-line-number)
	     (slot-boundp item 'begin-column-number)
	     (slot-boundp item 'end-line-number)
	     (slot-boundp item 'end-column-number)
	     (not (slot-boundp item 'text)))
    (setf (text item) (region (source-article item)
			      (begin-line-number item)
			      (begin-column-number item)
			      (end-line-number item)
			      (end-column-number item)))))

(defclass pseudo-item (item)
  ()
  (:documentation "A pseudo-item represents a non-exportable part of a
  mizar article.  This means: reserve, set,
  consider, reconsider, deffunc, and defpred statements.  These are
  statements that cannot usefully comprise a stand-alone article
  fragment, since the mizar exporter produces nothing from them,
  whereas items that occur later in the article from which a
  pseudo-item is taken can refer to this item.  Such items need to be
  'carried along', prefixed to an exportable item."))

(defclass reservation-item (pseudo-item)
  ())

(defclass set-item (pseudo-item)
  ())

(defclass consider-item (pseudo-item)
  ())

(defclass reconsider-item (pseudo-item)
  ())

(defclass deffunc-item (pseudo-item)
  ())

(defclass defpred-item (pseudo-item)
  ())

(defclass exportable-item (item)
  ()
  (:documentation "An exportable-item represents an exportable piece
  of a mizar article.  This means: theorem, definition, notation, and
  registration statements (and their contents)."))

(defclass iterequality-item (item)
  ())

(defclass theorem-item (exportable-item)
  ())

(defclass scheme-item (exportable-item)
  ())

(defclass proposition-item (exportable-item)
  ())

(defclass definition-item (exportable-item)
  ())

(defclass notation-item (exportable-item)
  ())

(defclass registration-item (exportable-item)
  ())

;;; item.lisp ends here