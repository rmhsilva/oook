;;;; OOOOOK!

(asdf:defsystem :oook
  :version      "0.1.0"
  :description  "Some magic on the shoulders of CLSQL"
  :author       "Ric da Silva <ric@rmhsilva.com>"
  :license      "MIT"

  :depends-on   (#:alexandria           ; Utilities
                 #:anaphora             ; WHhoooo anaphoraaaa
                 #:cl-inflector         ; Pluralise stuff
                 #:closer-mop           ; MOP compat layer
                 #:clsql                ; SQL ORM
                 #:jonathan             ; JSON
                 #:parse-number         ; General number parsing
                 #:spinneret            ; HTML generator
                 )

  :serial       t
  :components   ((:module "src"
                  :components
                  ((:file "package")
                   (:file "utils")
                   (:file "serialise")
                   (:file "html")
                   (:file "methods")
                   (:file "macro")
                   )))

  :in-order-to ((test-op (test-op #:oook-test))))
