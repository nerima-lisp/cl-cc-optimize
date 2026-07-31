;;;; optimizer-inline-devirt.lisp — sealed generic-function devirtualization
;;;;
;;;; Forward dataflow over opt-devirt-facts (optimizer-inline-devirt-data.lisp):
;;;; tracks which register holds which class, constant, or closure, then
;;;; rewrites a vm-generic-call to a direct call when the receiver's class is
;;;; known and its generic function has exactly one applicable, sealed
;;;; primary method. Register renaming for the inlined method body comes from
;;;; optimizer-register-rename.lisp.

(in-package :cl-cc/optimize)

(defun %opt-last-emitted-func-ref-p (result reg label)
  "Return T when RESULT already starts with REG's direct reference to LABEL."
  (let ((last (first result)))
    (and (typep last 'vm-func-ref)
         (eq (vm-dst last) reg)
         (equal (vm-label-name last) label))))

(defun %opt-devirt-call (inst reg-track result)
  "Emit a direct callee reference before INST when its callee register is known."
  (let* ((func-reg (vm-func-reg inst))
         (label (gethash func-reg reg-track)))
    (when (and label
               (not (%opt-last-emitted-func-ref-p result func-reg label)))
      (push (make-vm-func-ref :dst func-reg :label label) result))
    (push inst result)
    (let ((dst (opt-inst-dst inst)))
      (when dst
        (remhash dst reg-track)))
    result))

(defun %opt-clear-reg-facts (reg &rest tables)
  "Remove REG from each hash-table in TABLES."
  (when reg
    (dolist (table tables)
      (remhash reg table))))

(defun %opt-set-reg-fact (reg table value &rest other-tables)
  "Set REG's fact in TABLE and clear REG from OTHER-TABLES."
  (when reg
    (setf (gethash reg table) value)
    (dolist (other other-tables)
      (remhash reg other))))

(defun %opt-eql-specializer-p (specializer)
  "Return T when SPECIALIZER contains an EQL specializer."
  (cond
    ((and (consp specializer) (eq (car specializer) 'eql)) t)
    ((consp specializer) (some #'%opt-eql-specializer-p specializer))
    (t nil)))

(defun %opt-standard-combination-p (combination)
  "Return T when COMBINATION denotes the standard method combination."
  (or (null combination) (eq combination 'standard)))

(defun %opt-gf-info (gf-name gf-infos)
  "Return mutable compile-time metadata for GF-NAME."
  (or (gethash gf-name gf-infos)
      (setf (gethash gf-name gf-infos)
            (list :methods nil
                  :satiated nil
                  :combination nil
                  :unsafe nil))))

(defun %opt-hash-gf-info (gf-ht)
  "Build GF metadata from a literal GF hash table when possible."
  (when (and (hash-table-p gf-ht) (gethash :__methods__ gf-ht))
    (let ((methods nil)
          (methods-ht (gethash :__methods__ gf-ht)))
      (when (hash-table-p methods-ht)
        (maphash (lambda (specializer method)
                   (let* ((fn (and (hash-table-p method)
                                   (gethash :function method)))
                          (label (cond
                                   ((stringp fn) fn)
                                   ((typep fn 'vm-closure-object)
                                    (vm-closure-entry-label fn))
                                   (t nil))))
                     (push (list :specializer specializer
                                 :qualifier (and (hash-table-p method)
                                                 (first (gethash :qualifiers method)))
                                 :label label)
                           methods)))
                 methods-ht))
      (list :methods methods
            :satiated (and (gethash :__satiated__ gf-ht) t)
            :combination (gethash :__method-combination__ gf-ht)
            :unsafe nil))))

(defun %opt-track-sealed-gf-facts (inst class-sealed facts gf-infos)
  "Update compile-time facts used by sealed generic-function devirtualization.
FACTS is an opt-devirt-facts struct bundling all 6 per-register hash tables."
  (let ((dst (opt-inst-dst inst))
        (reg-name          (opt-devirt-facts-reg-name facts))
        (reg-class         (opt-devirt-facts-reg-class facts))
        (reg-object-class  (opt-devirt-facts-reg-object-class facts))
        (reg-const         (opt-devirt-facts-reg-const facts))
        (reg-closure-label (opt-devirt-facts-reg-closure-label facts))
        (reg-gf-literal    (opt-devirt-facts-reg-gf-literal facts)))
    (typecase inst
      (vm-class-def
       (%opt-set-reg-fact (vm-dst inst) reg-class (vm-class-name-sym inst)
                           reg-name reg-object-class reg-const reg-closure-label reg-gf-literal))
      (vm-get-global
       (%opt-set-reg-fact (vm-dst inst) reg-name (vm-global-name inst)
                           reg-class reg-object-class reg-const reg-closure-label reg-gf-literal))
      (vm-const
       (%opt-set-reg-fact (vm-dst inst) reg-const (vm-value inst)
                           reg-name reg-class reg-object-class reg-closure-label reg-gf-literal)
       (let ((literal-info (%opt-hash-gf-info (vm-value inst))))
         (when literal-info
           (setf (gethash (vm-dst inst) reg-gf-literal) literal-info))))
      ((or vm-closure vm-func-ref)
       (%opt-set-reg-fact (vm-dst inst) reg-closure-label (vm-label-name inst)
                           reg-name reg-class reg-object-class reg-const reg-gf-literal))
      (vm-move
       (let ((move-dst (vm-dst inst))
             (src (vm-move-src inst)))
         (%opt-clear-reg-facts move-dst reg-name reg-class reg-object-class
                               reg-const reg-closure-label reg-gf-literal)
         (dolist (accessor *devirt-fact-slot-accessors*)
           (let ((table (funcall accessor facts)))
             (multiple-value-bind (value found-p) (gethash src table)
               (when found-p
                 (setf (gethash move-dst table) value)))))))
      (vm-make-obj
       (let ((class-name (or (gethash (vm-class-reg inst) reg-name)
                             (gethash (vm-class-reg inst) reg-class))))
         (%opt-clear-reg-facts dst reg-name reg-class reg-object-class
                               reg-const reg-closure-label reg-gf-literal)
         (when (and class-name (gethash class-name class-sealed))
           (setf (gethash dst reg-object-class) class-name))))
      (vm-register-method
       (let ((gf-name (gethash (vm-gf-reg inst) reg-name))
             (label (gethash (vm-method-reg inst) reg-closure-label)))
         (when gf-name
           (let ((info (%opt-gf-info gf-name gf-infos)))
             ;; Runtime registration re-opens a satiated GF.  The optimizer only
             ;; treats a later explicit :__satiated__ write as a closed-world seal.
             (setf (getf info :satiated) nil)
             (if (vm-method-qualifier inst)
                 (setf (getf info :unsafe) t)
                 (push (list :specializer (vm-method-specializer inst)
                             :qualifier nil
                             :label label)
                       (getf info :methods)))))))
      (vm-sethash
       (let ((table-name (gethash (vm-hash-table-reg inst) reg-name)))
         (when table-name
           (multiple-value-bind (key key-known-p)
               (gethash (vm-hash-key inst) reg-const)
             (multiple-value-bind (value value-known-p)
                 (gethash (vm-hash-value inst) reg-const)
               (let ((info (%opt-gf-info table-name gf-infos)))
                 (cond
                   ((and key-known-p (eq key :__satiated__))
                    (if value-known-p
                        (setf (getf info :satiated) (not (opt-falsep value)))
                        (setf (getf info :unsafe) t)))
                   ((and key-known-p (eq key :__method-combination__))
                    (if value-known-p
                        (setf (getf info :combination) value)
                        (setf (getf info :unsafe) t)))
                   ((not key-known-p)
                    (setf (getf info :unsafe) t)))))))))
      (t
       (%opt-clear-reg-facts dst reg-name reg-class reg-object-class
                             reg-const reg-closure-label reg-gf-literal)))))

(defun %opt-single-sealed-primary-method-label (inst class-sealed facts gf-infos)
  "Return direct method label for safe sealed+satiated generic call INST, or NIL.
FACTS is an opt-devirt-facts struct bundling all per-register fact tables."
  (when (= (length (vm-args inst)) 1)
    (let* ((reg-name         (opt-devirt-facts-reg-name facts))
           (reg-object-class (opt-devirt-facts-reg-object-class facts))
           (reg-gf-literal   (opt-devirt-facts-reg-gf-literal facts))
           (arg-class        (gethash (first (vm-args inst)) reg-object-class))
           (gf-name          (gethash (vm-gf-reg inst) reg-name))
           (info             (or (and gf-name (gethash gf-name gf-infos))
                                 (gethash (vm-gf-reg inst) reg-gf-literal))))
      (when (and arg-class
                 (gethash arg-class class-sealed)
                 info
                 (getf info :satiated)
                 (not (getf info :unsafe))
                 (%opt-standard-combination-p (getf info :combination)))
        (let ((primary-methods
                (remove-if (lambda (method)
                             (or (getf method :qualifier)
                                 (%opt-eql-specializer-p (getf method :specializer))))
                           (copy-list (getf info :methods)))))
          (when (and (= (length primary-methods) 1)
                     (notany (lambda (method)
                               (%opt-eql-specializer-p (getf method :specializer)))
                             (getf info :methods)))
            (let ((method (first primary-methods)))
              (when (equal (getf method :specializer) arg-class)
                (getf method :label)))))))))

(defun %opt-fresh-register-generator (instructions)
  "Return a closure producing fresh VM register keywords for INSTRUCTIONS."
  (let ((next (1+ (opt-max-reg-index instructions))))
    (lambda ()
      (prog1 (intern (format nil "R~A" next) :keyword)
        (incf next)))))

(defun %opt-devirt-generic-call (inst class-sealed facts gf-infos fresh-reg result)
  "Replace INST with direct vm-call when sealed GF dispatch is statically unique.
FACTS is an opt-devirt-facts struct bundling all per-register fact tables."
  (let ((label (%opt-single-sealed-primary-method-label inst class-sealed facts gf-infos)))
    (if label
        (let ((func-reg (funcall fresh-reg)))
          (push (make-vm-func-ref :dst func-reg :label label) result)
          (push (make-vm-call :dst (vm-dst inst)
                              :func func-reg
                              :args (copy-list (vm-args inst)))
                result))
        (push inst result)))
  result)

(defun %opt-pass-devirtualize-local (instructions)
  "Insert direct function references before calls whose callee register is known.
This implements a conservative called-value propagation slice: closure,
function-reference, registered symbol, and move designators are tracked through a
single forward scan, and `vm-call`/`vm-tail-call`/`vm-apply` sites with a known
callee receive a `vm-func-ref` immediately before the call.  The call itself is
left in place so existing VM semantics and the inline pass remain unchanged.

For sealed and explicitly satiated generic functions, this pass also performs a
more aggressive static-dispatch optimization: a `vm-generic-call` whose receiver
object is known to be an instance of a sealed class is rewritten to a direct
`vm-call` when the GF has exactly one applicable unqualified primary method,
uses the standard method combination, and has no EQL-specializer ambiguity."
  (let ((name-to-label (opt-build-function-name-map instructions))
        (reg-track    (make-hash-table :test #'eq))
        (class-sealed (make-hash-table :test #'equal))
        (facts        (%make-devirt-facts))
        (gf-infos     (make-hash-table :test #'equal))
        (fresh-reg    (%opt-fresh-register-generator instructions))
        (result       nil))
    (dolist (inst instructions)
      (when (typep inst 'vm-class-def)
        (setf (gethash (vm-class-name-sym inst) class-sealed)
              (and (cl-cc/vm:vm-sealed-p inst) t))))
    (dolist (inst instructions (nreverse result))
      (typecase inst
        ((or vm-call vm-tail-call vm-apply)
         (setf result (%opt-devirt-call inst reg-track result)))
        (vm-generic-call
         (when *opt-enable-sealed-gf-devirtualization*
           (setf result (%opt-devirt-generic-call inst class-sealed facts
                                                  gf-infos fresh-reg result)))
         (unless *opt-enable-sealed-gf-devirtualization*
           (push inst result))
         (let ((dst (opt-inst-dst inst)))
           (%opt-clear-reg-facts dst reg-track
                                 (opt-devirt-facts-reg-name facts)
                                 (opt-devirt-facts-reg-class facts)
                                 (opt-devirt-facts-reg-object-class facts)
                                 (opt-devirt-facts-reg-const facts)
                                 (opt-devirt-facts-reg-closure-label facts)
                                 (opt-devirt-facts-reg-gf-literal facts))))
        (t
         (%opt-track-known-callee-label inst name-to-label reg-track)
          (%opt-track-sealed-gf-facts inst class-sealed facts gf-infos)
          (push inst result))))))
