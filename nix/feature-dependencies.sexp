((:feature "analysis"
           :source-dirs
           ("src/analysis")
           :test-dir
           "t/analysis"
           :facade-packages
           ("cl-cc/optimize")
           :domain-package
           "cl-cc/optimize/analysis"
           :infra-package
           nil
           :depends-on
           ())
 (:feature "scalar"
           :source-dirs
           ("src/scalar")
           :test-dir
           "t/scalar"
           :facade-packages
           ("cl-cc/optimize")
           :domain-package
           "cl-cc/optimize/scalar"
           :infra-package
           nil
           :depends-on
           ("analysis" "reporting"))
 (:feature "control-flow"
           :source-dirs
           ("src/control-flow")
           :test-dir
           "t/control-flow"
           :facade-packages
           ("cl-cc/optimize")
           :domain-package
           "cl-cc/optimize/control-flow"
           :infra-package
           nil
           :depends-on
           ("analysis" "reporting" "scalar"))
 (:feature "loops"
           :source-dirs
           ("src/loops")
           :test-dir
           "t/loops"
           :facade-packages
           ("cl-cc/optimize")
           :domain-package
           "cl-cc/optimize/loops"
           :infra-package
           nil
           :depends-on
           ("analysis" "control-flow" "reporting" "scalar"))
 (:feature "memory"
           :source-dirs
           ("src/memory")
           :test-dir
           "t/memory"
           :facade-packages
           ("cl-cc/optimize")
           :domain-package
           "cl-cc/optimize/memory"
           :infra-package
           nil
           :depends-on
           ("analysis" "loops"))
 (:feature "inlining"
           :source-dirs
           ("src/inlining")
           :test-dir
           "t/inlining"
           :facade-packages
           ("cl-cc/optimize")
           :domain-package
           "cl-cc/optimize/inlining"
           :infra-package
           nil
           :depends-on
           ("analysis" "reporting" "rewriting"))
 (:feature "partial-evaluation"
           :source-dirs
           ("src/partial-evaluation")
           :test-dir
           "t/partial-evaluation"
           :facade-packages
           ("cl-cc/optimize")
           :domain-package
           "cl-cc/optimize/partial-evaluation"
           :infra-package
           nil
           :depends-on
           ("analysis"))
 (:feature "speculation"
           :source-dirs
           ("src/speculation")
           :test-dir
           "t/speculation"
           :facade-packages
           ("cl-cc/optimize")
           :domain-package
           "cl-cc/optimize/speculation"
           :infra-package
           nil
           :depends-on
           ("analysis" "inlining" "loops" "partial-evaluation"))
 (:feature "rewriting"
           :source-dirs
           ("src/rewriting")
           :test-dir
           "t/rewriting"
           :facade-packages
           ("cl-cc/optimize")
           :domain-package
           "cl-cc/optimize/rewriting"
           :infra-package
           nil
           :depends-on
           ("analysis"))
 (:feature "pipeline"
           :source-dirs
           ("src/pipeline")
           :test-dir
           "t/pipeline"
           :facade-packages
           ("cl-cc/optimize")
           :domain-package
           "cl-cc/optimize/pipeline"
           :infra-package
           nil
           :depends-on
           ("analysis" "control-flow" "inlining" "loops" "memory" "rewriting" "reporting" "scalar" "speculation" "verification"))
 (:feature "reporting"
           :source-dirs
           ("src/reporting")
           :test-dir
           "t/reporting"
           :facade-packages
           ("cl-cc/optimize")
           :domain-package
           "cl-cc/optimize/reporting"
           :infra-package
           nil
           :depends-on
           ("analysis"))
 (:feature "verification"
           :source-dirs
           ("src/verification")
           :test-dir
           "t/verification"
           :facade-packages
           ("cl-cc/optimize")
           :domain-package
           "cl-cc/optimize/verification"
           :infra-package
           nil
           :depends-on
           ("analysis")))
