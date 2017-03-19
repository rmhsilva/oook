;;;; Test OOOK

(asdf:defsystem :oook-test
  :depends-on   (#:oook
                 #:2am                  ; Test framework
                 )

  ;; TODO: reorganise the test stuff. Each test should use 'in-package' to
  ;; access the internals of the package under test. There should probably be
  ;; standard setup / tear down. Each package gets a 'suite'?
  :serial t
  :components   ((:module "test"
                  :components
                  ((:file "fixtures")
                   (:file "test" :depends-on ("fixtures")))))

  :perform (test-op (op system)
                    (uiop:symbol-call :2am :run)))
