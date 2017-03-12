;;;; Package definitions for OOOK


(defpackage :oook.utils
  (:use :cl)
  (:export
   :from-alist
   :search-like
   :to-alist))


(defpackage :oook.macro
  (:use :cl)
  (:nicknames :macro)
  (:import-from :cl-inflector :symbol-plural-of :symbol-singular-of)
  (:import-from :alexandria   :ensure-list      :make-keyword :compose)
  (:import-from :anaphora     :aif              :it)
  (:export
   :*default-slot-type*
   :defmodel
   :def-enhanced-printer
   :foreign-key
   :join-fks
   :id
   :owns-one
   :owns-many
   :created-at
   :last-modified
   :make-foreign-key))


(defpackage :oook.methods
  (:use :cl :oook.utils)
  (:import-from
   :oook.macro
   :id
   :created-at
   :last-modified
   :owns-one
   :owns-many
   :make-foreign-key)
  (:export
   :filter
   :find-by-id
   :save
   :destroy))


(defpackage :oook
  (:use :cl)
  (:import-from
   :oook.utils
   :search-like
   :to-alist
   :from-alist)
  (:import-from
   :oook.macro
   :*default-slot-type*
   :defmodel
   :def-enhanced-printer
   :id
   :created-at
   :last-modified)
  (:import-from
   :oook.methods
   :find-by-id
   :filter
   :save
   :destroy)
  (:export
   :*default-slot-type*
   :defmodel
   :search-like
   :def-enhanced-printer
   :to-alist
   :from-alist
   :id
   :created-at
   :last-modified
   :find-by-id
   ;; :filter
   :save
   :destroy))
