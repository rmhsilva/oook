;;;; OOOOOK!

(asdf:defsystem :oook
  :version      "0.1.0"
  :description  "An ORM for Common Lisp"
  :author       "Ric da Silva <ric@rmhsilva.com>"
  :license      "MIT"

  :depends-on   (#:alexandria           ; Utilities
                 #:anaphora             ; WHhoooo anaphoraaaa
                 #:cl-inflector         ; Pluralise stuff
                 #:sqlite               ; SQLite DB lib
                 #:jonathan             ; JSON
                 #:clsql                ; SQL ORM
                 )

  :serial       t
  :components   ((:module "src"
                  :components
                  ((:file "package")
                   (:file "utils")
                   ;; (:file "errors")
                   (:file "methods")
                   (:file "macro")
                   )))

  :in-order-to ((test-op (test-op #:oook-test))))
