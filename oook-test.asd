;;;; Test OOOK

(asdf:defsystem :oook-test
  :depends-on   (#:oook
                 #:2am                  ; Test framework
                 )

  :serial t
  :components   ((:module "test"
                  :components
                  ((:file "fixtures")
                   (:file "test" :depends-on ("fixtures")))))

  :perform (test-op (op system)
                    (uiop:symbol-call :2am :run))
  )
