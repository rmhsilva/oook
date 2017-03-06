;;;; Big macros

(in-package :oook.macro)


;;
;; Helpers

;; TODO pluralise
(defun sql-table-name (symbol)
  "Make `symbol' suitable for SQL table name"
  (sql-field-keyword symbol))

(defun parse-slot-def (form)
  "Extract model slot info from `form'"
  (destructuring-bind (name &key
                              (type :string)
                              field
                              default)
      (ensure-list form)
    ;; (make-instance 'model-slot
    (make-model-slot
     :name name
     :type type
     :field (aif field
                 it
                 (sql-field name))
     :default default)))

(defun make-foreign-key (symbol)
  "Create a foreign key (append _id) for symbol"
  (intern (concatenate 'string
                       (symbol-name symbol)
                       "-ID")))


;;
;; Main macro definition

(defmacro defmodel (model-name (&key
                                  (table nil)
                                  (has-one nil)
                                  (owns-one nil)
                                  ;; (has-many nil) ; TODO many-to-many
                                  (owns-many nil)
                                  (belongs-to nil))
                    &body fields)
  "Define a class that models DB structure"
  (let* ((got-doc (stringp (car fields)))
         (doc (if got-doc (car fields) ""))
         (sql-table (if table table (sql-table-name model-name)))
         (slot-forms (if got-doc (cdr fields) fields))
         ;; Names that correspond to columns in the table
         (custom-slots (mapcar #'parse-slot-def slot-forms))
         (custom-slot-names (mapcar #'(lambda (s) (model-slot-name s))
                                    custom-slots))
         ;; initforms for the fields (default values in defmodel)
         (init-forms (mapcar #'(lambda (s) (model-slot-default s))
                             custom-slots))
         ;; Slots for owns-many relations
         (owns-many-slots (mapcar #'symbol-plural-of owns-many))
         (slots
           ;; Slots for the custom model fields
           (loop for slot in custom-slot-names
                 ;; for type in field-types
                 for init in init-forms
                 collecting `(,slot :initform ,init
                                    :initarg ,(make-keyword slot)
                                    :accessor ,slot)))
         ;; Definitions of all associated models
         (submodels (append
                     (loop for foreign in has-one
                           collect (make-submodel :type foreign
                                                       :assoc :has-one))
                     (loop for foreign in belongs-to
                           collect (make-submodel :type foreign
                                                       :assoc :belongs-to))))
         ;; Slots for all associated models
         (assocs (loop for right in (append has-one
                                            owns-one
                                            belongs-to
                                            (mapcar #'symbol-plural-of owns-many))
                       collecting
                       `(,right :accessor ,right :initform 'nil))))
    `(progn
       (defclass ,model-name (<model>)
         ((sql-table :reader sql-table :initform ,sql-table)
          (field-defs :initform ',custom-slots)
          (submodels :initform ',submodels)
          ,@assocs
          ,@slots)
         (:documentation ,doc))

       ;; Define getters/setters for each foreign model
       ,@(mapcar (lambda (right) (associate-owns-one model-name right))
                 owns-one)
       ,@(mapcar (lambda (right) (associate-has-one model-name right))
                 (append belongs-to has-one))
       ,@(mapcar (lambda (right) (associate-owns-many model-name right))
                 owns-many)
       )))


;;
;; Association helpers

(defun bla (finder right fk current id)
  (cond
    ((null current)
     (funcall #'finder right fk := id))
    ((integerp current)
     (funcall #'finder right fk := current))
    ((eql right (type-of current))
     current)
    (t (error "Something unexpected is in the slot!"))))

(defmacro associate-owns-one (model right)
  (let ((inst (gensym "INST")))
    `(progn
       (defmethod ,right ((,inst ,model))
         (setf (slot-value ,inst ',right)
               (finder find-one!
                       ',right
                       ,(make-keyword (make-foreign-key model))
                       (slot-value ,inst ',right)
                       (id ,inst)))))))

;; (associate-owns-one user config)

(defmacro associate-has-one (model right)
  (let ((inst (gensym "INST")))
    `(defmethod ,right ((,inst ,model))
       (unless (slot-value ,inst ',right)
         (setf (slot-value ,inst ',right)
               (find-one! ',right
                          :id := (submodel-id ,inst ',right)))))))

;; (associate-has-one ingredient unit)

(defmacro associate-owns-many (model right)
  (let ((inst (gensym "INST"))
        (slot (symbol-plural-of right)))
    `(defmethod ,right ((,inst ,model))
       (unless (slot-value ,inst ',slot)
         (setf (slot-value ,inst ',slot)
               (find! ',right
                      ,(make-keyword (make-foreign-key model))
                      := (id ,inst)))))))

;; (associate-owns-many recipe recipe-step)
