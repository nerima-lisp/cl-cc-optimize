(in-package :cl-cc/optimize/partial-evaluation/test)

(it-sequential "opt-lattice-meet-conservative-cases"
  (let* ((bottom (cl-cc/optimize/partial-evaluation:opt-lattice-bottom))
         (seven-a (cl-cc/optimize/partial-evaluation:opt-lattice-constant 7))
         (seven-b (cl-cc/optimize/partial-evaluation:opt-lattice-constant 7))
         (eight (cl-cc/optimize/partial-evaluation:opt-lattice-constant 8))
         (overdefined (cl-cc/optimize/partial-evaluation:opt-lattice-overdefined)))
    (expect (cl-cc/optimize/partial-evaluation:opt-lattice-value-kind
             (cl-cc/optimize/partial-evaluation:opt-lattice-meet bottom seven-a))
            :to-be :constant)
    (expect (= 7 (cl-cc/optimize/partial-evaluation:opt-lattice-value-value
                  (cl-cc/optimize/partial-evaluation:opt-lattice-meet seven-a seven-b)))
            :to-be-truthy)
    (expect (cl-cc/optimize/partial-evaluation:opt-lattice-value-kind
             (cl-cc/optimize/partial-evaluation:opt-lattice-meet seven-a eight))
            :to-be :overdefined)
    (expect (cl-cc/optimize/partial-evaluation:opt-lattice-value-kind
             (cl-cc/optimize/partial-evaluation:opt-lattice-meet seven-a overdefined))
            :to-be :overdefined)))
