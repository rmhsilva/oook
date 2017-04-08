;;;; OOOOOK!

(asdf:defsystem :oook
  :version      "0.1.0"
  :description  "Some magic on the shoulders of CLSQL"
  :author       "Ric da Silva <ric@rmhsilva.com>"
  :license      "MIT"

  :depends-on   (#:alexandria           ; Utilities
                 #:anaphora             ; WHhoooo anaphoraaaa
                 #:cl-inflector         ; Pluralise stuff
                 #:sqlite               ; SQLite DB lib
                 #:jonathan             ; JSON
                 #:clsql                ; SQL ORM
                 #:parse-number         ; General number parsing
                 #:closer-mop           ; MOP compat layer
                 )

  :serial       t
  :components   ((:module "src"
                  :components
                  ((:file "package")
                   (:file "utils")
                   ;; (:file "errors")
                   (:file "serialise")
                   (:file "methods")
                   (:file "macro")
                   )))

  :in-order-to ((test-op (test-op #:oook-test))))
