(in-package #:cl-cc/optimize/partial-evaluation)

(defstruct (opt-lattice-value (:conc-name opt-lattice-value-))
  "Three-point SCCP/IPSCCP lattice value."
  (kind :bottom :type keyword)
  value)

(defun opt-lattice-bottom ()
  "Return the unknown-bottom lattice element."
  (make-opt-lattice-value :kind :bottom))

(defun opt-lattice-constant (value)
  "Return a constant lattice element for VALUE."
  (make-opt-lattice-value :kind :constant :value value))

(defun opt-lattice-overdefined ()
  "Return the overdefined lattice element."
  (make-opt-lattice-value :kind :overdefined))

(defun opt-lattice-meet (left right)
  "Conservatively meet LEFT and RIGHT in the SCCP lattice."
  (let ((left-kind (opt-lattice-value-kind left))
        (right-kind (opt-lattice-value-kind right)))
    (cond
      ((eq left-kind :bottom) right)
      ((eq right-kind :bottom) left)
      ((or (eq left-kind :overdefined)
           (eq right-kind :overdefined))
       (opt-lattice-overdefined))
      ((and (eq left-kind :constant)
            (eq right-kind :constant)
            (equal (opt-lattice-value-value left)
                   (opt-lattice-value-value right)))
       left)
      (t (opt-lattice-overdefined)))))
