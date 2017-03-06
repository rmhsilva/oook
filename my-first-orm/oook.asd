;;;; OOOOOK!

(asdf:defsystem :oook
  :version      "0.1.0"
  :description  "An ORM for Common Lisp"
  :author       "Ric da Silva <ric@rmhsilva.com>"
  :license      "MIT"

  :depends-on   (#:alexandria           ; Utilities
                 #:anaphora             ; WHhoooo anaphoraaaa
                 #:cl-inflector         ; Pluralise stuff
                 ;; #:dbi                  ; Database interface
                 #:sqlite               ; SQLite DB lib
                 #:sxql                 ; SQL generator
                 #:envy                 ; Config TODO remove
                 #:2am                  ; Test framework
                 )

  :serial       t
  :components   ((:file "package")
                 (:module "src"
                  :depends-on ("package")
                  :components
                  ((:file "macro" :depends-on ("model"))
                   ;; (:file "error")
                   (:file "db")
                   (:file "model")))

                 (:module "test"
                  :depends-on ("package" "src")
                  :components
                  ((:file "fixtures")
                   (:file "test-all" :depends-on ("fixtures"))))))
