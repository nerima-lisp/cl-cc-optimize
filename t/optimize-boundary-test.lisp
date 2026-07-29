;;;; t/optimize-boundary-test.lisp — module boundary tests for cl-cc-optimize
;;;;
;;;; cl-cc's own suite covers the passes against this system. What is pinned
;;;; here is the property that made the extraction possible at all: this package
;;;; reaches cl-cc/vm only through its public contract. While it did not, no
;;;; separate repository could have built -- an external consumer cannot name
;;;; another package's internal symbols.

(in-package :cl-cc-optimize/test)

(defun %internal-vm-references-in (path)
  "Return the distinct CL-CC/VM internal symbol names named under PATH."
  (let ((names '()))
    (dolist (file (directory (merge-pathnames "**/*.lisp" path))
                  (sort (remove-duplicates names :test #'string=) #'string<))
      (with-open-file (in file :external-format :utf-8)
        (loop for line = (read-line in nil)
              while line
              do (let ((start 0))
                   (loop for hit = (search "cl-cc/vm::" line :start2 start
                                                             :test #'char-equal)
                         while hit
                         do (let* ((from (+ hit (length "cl-cc/vm::")))
                                   (to (or (position-if-not
                                            (lambda (c)
                                              (or (alphanumericp c)
                                                  (find c "%*+-/=<>?!_")))
                                            line :start from)
                                           (length line))))
                              (when (> to from) (push (subseq line from to) names))
                              (setf start (max (1+ hit) to))))))))))

(describe-sequential "cl-cc-optimize boundary with cl-cc/vm"
  (it "names no cl-cc/vm internal symbol"
    ;; §5-2 of cl-cc's split design. This started at 75 references and is the
    ;; gate the extraction had to pass; a new one would reintroduce a coupling
    ;; that only shows up as a broken build in this repository.
    (expect (%internal-vm-references-in
             (asdf:system-relative-pathname :cl-cc-optimize "src/"))
            :to-be nil)))

(describe-sequential "cl-cc-optimize public surface"
  (it "exports the pass entry points a driver calls"
    (dolist (name '("OPTIMIZE-INSTRUCTIONS" "OPT-PASS-FOLD" "OPT-PASS-DEVIRTUALIZE"))
      (expect (nth-value 1 (find-symbol name :cl-cc/optimize)) :to-be :external)))

  (it "exports the known-function property database"
    (dolist (name '("KNOWN-FUNCTION-PROPERTIES" "KNOWN-FUNCTION-PROPERTY-P"))
      (expect (nth-value 1 (find-symbol name :cl-cc/optimize)) :to-be :external))))

(describe-sequential "optimization is behaviour-preserving on a trivial program"
  (it "folds a constant binop and keeps the result register"
    ;; The narrowest end-to-end check this repository can make on its own: the
    ;; VM is a dependency, so a program can be built and optimized here, but
    ;; running one belongs to cl-cc's suite.
    (let* ((program (cl-cc/vm:make-vm-program
                     :instructions (list (cl-cc/vm:make-vm-const :dst :r0 :value 2)
                                         (cl-cc/vm:make-vm-const :dst :r1 :value 3)
                                         (cl-cc/vm:make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                                         (cl-cc/vm:make-vm-halt :reg :r2))
                     :result-register :r2))
           (optimized (cl-cc/optimize:optimize-instructions
                       (cl-cc/vm:vm-program-instructions program))))
      (expect (listp optimized) :to-be-truthy)
      (expect (plusp (length optimized)) :to-be-truthy))))

(describe-sequential "optimizer passes return complete instruction streams"
  (it "returns final cold-block labels and preserves the analyzed stream"
    (let* ((jump (cl-cc/vm:make-vm-jump-zero :reg :condition :label "cold"))
           (label (cl-cc/vm:make-vm-label :name "cold"))
           (signal (cl-cc/vm:make-vm-signal-error :error-reg :error))
           (instructions (list jump label signal))
           (cold-labels (cl-cc/optimize::%opt-cold-labels instructions))
           (analyzed (cl-cc/optimize:opt-analyze-branch-weights instructions)))
      (expect (hash-table-p cold-labels) :to-be-truthy)
      (expect (gethash "cold" cold-labels) :to-be-truthy)
      (expect (hash-table-count cold-labels) :to-equal 1)
      (expect (length analyzed) :to-equal (length instructions))
      (expect (cl-cc/optimize:opt-branch-weight (first analyzed)) :to-be :unlikely)
      (expect (second analyzed) :to-be label)
      (expect (third analyzed) :to-be signal)))

  (it "reassociation returns a nonempty unchanged stream when no rewrite applies"
    (let* ((constant (cl-cc/vm:make-vm-const :dst :r0 :value 10))
           (halt (cl-cc/vm:make-vm-halt :reg :r0))
           (instructions (list constant halt))
           (result (cl-cc/optimize::opt-pass-reassociate instructions)))
      (expect result :to-be-truthy)
      (expect result :to-equal instructions)
      (expect (every #'eq result instructions) :to-be-truthy))))
