;;;; Testing OOOK

(defpackage :oook-test (:use :cl :2am :oook) (:export :clear-tests))
(in-package :oook-test)

(defun clear-tests ()
  "Remove all tests"
  ;; NOTE: this line just clears the NIL suite
  (setf (suite nil) (values nil nil)))


(defvar name-slot '(name))
(defvar amount-slot '(amount :type :integer))


(test parse-slot-defs
  (is (equal (macro::parse-slot-def name-slot)
             '(:name name))
      "Single name")
  (is (equal (macro::parse-slot-def amount-slot)
             '(:name amount
               :type :integer))
      "Name with type"))

(test make-base-slot
  (is (equal (macro::make-clsql-base-slot name-slot)
             '(name
               :initarg :name
               :accessor name))
      "Single name")
  (is (equal (macro::make-clsql-base-slot amount-slot)
             '(amount
               :initarg :amount
               :accessor amount
               :type :integer))
      "Name with type"))

(test make-key-slot
  (is (equal (macro::make-clsql-key-slot 'name)
             '(name
               :initarg :name
               :accessor name
               :type :integer
               :db-constraints :not-null
               :db-kind :key))
      "Foreign key slot"))


(test make-join-slot
  (is (equal (macro::make-clsql-join-slot 'thing :home 'has-one-id :foreign 'id)
             '(thing
               :accessor thing
               :db-kind :join
               :db-info (:join-class thing
                         :home-key has-one-id
                         :foreign-key id
                         :set nil)))
      "A single join")
  (is (equal (macro::make-clsql-join-slot 'things :model 'thing
                                                  :home 'id
                                                  :foreign 'this-id
                                                  :set t)
             '(things
               :accessor things
               :db-kind :join
               :db-info (:join-class thing
                         :home-key id
                         :foreign-key this-id
                         :set t)))
      "Multiple join"))

