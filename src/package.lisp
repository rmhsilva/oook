;;;; Package definitions for OOOK


(defpackage :oook.utils
  (:use :cl)
  (:import-from :parse-number :parse-number)
  (:export
   :parse-as-type
   :def-enhanced-printer
   :sql-field
   :search-like))

(defpackage :oook.macro
  (:documentation
   "Defines the `defmodel' macro, and the associated functions")
  (:use :cl)
  (:nicknames :macro)
  (:import-from :alexandria   :ensure-list      :make-keyword :compose)
  (:import-from :cl-inflector :symbol-plural-of :symbol-singular-of)
  (:import-from :anaphora     :aif              :it)
  (:import-from :oook.utils   :sql-field)
  (:export
   :*default-slot-type*
   :defmodel
   :owns-one
   :owns-many
   :foreign-key
   :join-fks
   :serialisable-fields
   :deserialisable-fields
   :serialisable-joins
   :id
   :created-at
   :last-modified))

(defpackage :oook.serialise
  (:documentation
   "A set of serialisation utilities, including to/from alists, to json (using
   jonathan)")
  (:use :cl)
  (:import-from :anaphora :aif :it)
  (:import-from :oook.utils :sql-field)
  (:import-from :spinneret :deftag)
  (:import-from
   :oook.macro
   :serialisable-fields
   :deserialisable-fields
   :serialisable-joins
   :owns-many)
  (:export
   :*serialisation-options*
   :with-serialisation-options
   :pprint-model
   :from-alist
   :to-alist))


(defpackage :oook.methods
  (:use :cl)
  (:import-from
   :oook.macro
   :id
   :created-at
   :last-modified
   :foreign-key
   :join-fks
   :owns-one
   :owns-many)
  (:export
   :filter
   :find-by-id
   :save
   :destroy))


(defpackage :oook
  (:use :cl)
  (:import-from
   :oook.utils
   :def-enhanced-printer
   :search-like)
  (:import-from
   :oook.serialise
   :*serialisation-options*
   :with-serialisation-options
   :to-alist
   :pprint-model
   :from-alist)
  (:import-from
   :oook.macro
   :*default-slot-type*
   :defmodel
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
   :*serialisation-options*
   :*default-slot-type*
   :defmodel
   :search-like
   :def-enhanced-printer
   :with-serialisation-options
   :to-alist
   :from-alist
   :pprint-model
   :id
   :created-at
   :last-modified
   :find-by-id
   ;; :filter
   :save
   :destroy))
