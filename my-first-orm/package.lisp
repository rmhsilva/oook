;;;; Package definitions for OOOK

(defpackage :oook
  (:use :cl)
  ;; (:import-from)
  (:export
   :defmodel
   :find!
   :find-one!
   :save!
   :destroy!))


(defpackage :oook.db
  (:nicknames :db)
  (:use :cl :sxql)
  (:export
   :set-db
   :with-db
   :atomic
   :get-one
   :get-all
   :exec
   :insert-one))


(defpackage :oook.util
  (:use :cl)
  (:import-from :alexandria :make-keyword)
  (:export
   :ensure-slot-type
   :ensure-integer
   :sql-field-keyword
   :sql-field))


(defpackage :oook.model
  (:use :cl :sxql :oook.util)
  (:export
   :id
   :created-at
   :last-modified
   :make-model-slot
   :model-slot-name
   :submodel-id
   :make-submodel
   :model-slot-default
   :<model>
   :find!
   :find-one!
   :save!
   :destroy!))


(defpackage :oook.macro
  (:use :cl :oook.model :oook.util)
  (:import-from :cl-inflector :symbol-plural-of :symbol-singular-of)
  (:import-from :alexandria   :ensure-list      :make-keyword)
  (:import-from :anaphora     :aif              :it)
  (:export
   :defmodel))


(defpackage :oook.helpers
  (:use :cl)
  (:export
   :from-alist
   :to-alist))

;; NOTE: no jonathan deps. To-alist lets jonathan be used later on easily.
