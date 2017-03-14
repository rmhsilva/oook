;;;; Big macros

(in-package :oook.macro)

(defvar *default-slot-type* 'string)


;;
;; Helpers

;; TODO pluralise
;; (defun sql-table-name (symbol)
;;   "Make `symbol' suitable for SQL table name"
;;   (sql-field-keyword symbol))

(defun make-foreign-key (symbol)
  "Create a foreign key (append _id) for symbol"
  (intern (concatenate 'string
                       (symbol-name symbol)
                       "-ID")))


(defun make-clsql-base-slot (slot)
  "Create a clsql base slot form from `slot'"
  (destructuring-bind (name &rest args &key &allow-other-keys)
      (ensure-list slot)
    `(,name
      ,@(unless (getf args :initarg)
          (list :initarg (make-keyword name)))
      ,@(unless (getf args :accessor)
          (list :accessor name))
      ,@(unless (getf args :type)
          (list :type *default-slot-type*))
      ,@(unless (getf args :initform)
          (list :initform nil))
      ,@args)))


(defun make-clsql-key-slot (name)
  "Create a clsql foreign key slot form from `slot'"
  (make-clsql-base-slot
   `(,name
     :type integer
     :db-constraints :not-null)))

(defun make-clsql-join-slot (name &key model home foreign set)
  (symbol-macrolet ((the-model (aif model it name)))
    `(,name
      :accessor ,name
      :initarg ,(make-keyword name)
      :db-kind :join
      :db-info (:join-class ,the-model
                :home-key ,home
                :foreign-key ,foreign
                :set ,set))))


;;
;; Main macro definition

(defmacro defmodel (model-name (&key
                                  (table nil)
                                  (timestamped t)
                                  (has-one nil)
                                  (owns-one nil)
                                  ;; (has-many nil) ; TODO many-to-many
                                  (owns-many nil)
                                  (belongs-to nil))
                    &body fields)
  "Define a class that models DB structure"
  (let* ((has-one (ensure-list has-one))
         (owns-one (ensure-list owns-one))
         (owns-many (ensure-list owns-many))
         (belongs-to (ensure-list belongs-to))
         (got-doc (stringp (car fields)))
         (sql-table (if table table model-name))
         (doc (if got-doc (car fields) ""))
         (slot-forms (if got-doc (cdr fields) fields))
         (owns-many-map (loop for right in owns-many
                              collecting
                              (cons right (symbol-plural-of right))))
         (id-symbol (intern "ID")))
    `(clsql:def-view-class ,model-name ()
       ((foreign-key
         :db-kind :virtual
         :reader foreign-key
         :initform ',(make-foreign-key model-name))
        (owns-one
         :db-kind :virtual
         :reader owns-one
         :initform ',owns-one)
        (owns-many
         :db-kind :virtual
         :reader owns-many
         :initform ',owns-many-map)
        (join-fks
         :db-kind :virtual
         :reader join-fks
         :initform ',(loop for right in (append has-one belongs-to)
                           collect (cons right (make-foreign-key right))))
        (serialisable-fields
         :db-kind :virtual
         :reader serialisable-fields
         :initform ',(append (if timestamped '(id created-at last-modified))
                             (mapcar #'(lambda (slot)
                                         (first (ensure-list slot)))
                                     slot-forms)))
        (serialisable-joins
         :db-kind :virtual
         :reader serialisable-joins
         :initform ',(append has-one owns-one (mapcar #'symbol-plural-of
                                                      owns-many)))
        (id
         :accessor id
         :type integer
         :initarg :id
         :db-kind :key
         :db-constraints :not-null)
        ,@(if timestamped
              `((created-at
                 :type clsql:wall-time
                 :accessor created-at
                 :initform nil)
                (last-modified
                 :type clsql:wall-time
                 :accessor last-modified
                 :initform nil)))
        ,@(mapcar #'make-clsql-base-slot
                  slot-forms)
        ,@(mapcar (compose #'make-clsql-key-slot #'make-foreign-key)
                  (append has-one belongs-to))
        ,@(mapcar (lambda (right)
                    (make-clsql-join-slot
                     right
                     :model right
                     :home (make-foreign-key right)
                     :foreign id-symbol))
                  has-one)
        ,@(mapcar (lambda (right)
                    (make-clsql-join-slot
                     right
                     :model right
                     :home id-symbol
                     :foreign (make-foreign-key model-name)))
                  owns-one)
        ,@(mapcar (lambda (right)
                    (make-clsql-join-slot
                     (symbol-plural-of right)
                     :model right
                     :home id-symbol
                     :foreign (make-foreign-key model-name)
                     :set t))
                  owns-many))
       (:base-table ,sql-table)
       (:documentation ,doc))))


(defgeneric id (inst)
  (:documentation "Read the `id' attribute"))

(defgeneric serialisable-fields (inst)
  (:documentation
   "Return the slots in `inst' that will be included when `to-alist' is called"))

(defgeneric serialisable-joins (inst))

(defgeneric join-fks (inst)
  (:documentation
   "Return the foreign keys for models in has-one and belongs-to"))

(defgeneric foreign-key (inst))

(defgeneric owns-one (inst))

(defgeneric owns-many (inst))

(defgeneric created-at (inst))

(defgeneric last-modified (inst))

