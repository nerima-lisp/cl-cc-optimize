(require :asdf)

(progn
  (defparameter *root*
    (make-pathname :name nil :type nil :defaults *load-truename*))
  (asdf:initialize-source-registry
   `(:source-registry (:tree ,*root*) :inherit-configuration))
  (defun lisp-sources-under (directory)
    (append
     (remove-if-not
      (lambda (path)
        (string-equal (pathname-type path) "lisp"))
      (uiop:directory-files directory))
     (mapcan (function lisp-sources-under) (uiop:subdirectories directory))))
  (defun component-lisp-sources (component)
    (if (typep component (find-class (find-symbol "SOURCE-FILE" "ASDF")))
        (list (truename (asdf:component-pathname component)))
        (mapcan (function component-lisp-sources)
                (asdf:component-children component))))
  (let* ((test-root (merge-pathnames "t/" *root*))
         (test-sources
          (mapcar (function truename) (lisp-sources-under test-root)))
         (test-system (asdf:find-system "cl-cc-optimize/test"))
         (component-sources (component-lisp-sources test-system))
         (orphans
          (set-difference test-sources
                          component-sources
                          :test
                          (lambda (test-source component-source)
                            (equal (namestring test-source)
                                   (namestring component-source))))))
    (unless test-sources
      (error "No Lisp test sources found under ~A" test-root))
    (unless component-sources
      (error "ASDF test system has no Lisp source components"))
    (when orphans
      (format *error-output* "Found test source files not loaded by ASDF:~%")
      (dolist (path orphans)
        (format *error-output* "  ~A~%" (enough-namestring path *root*)))
      (uiop:quit 1))
    (format t
            "Checked ~D test source files against ~D ASDF components.~%"
            (length test-sources)
            (length component-sources)))
  (uiop:quit 0))
