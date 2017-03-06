;;;; Definition of base model class, and associated methods

(in-package oook.model)


(defvar *builtin-fields* '(id created-at last-modified)
  "A list of fields automatically added and managed by OOOK in all models")


;;
;; Internally used structures

;; TODO maybe these should be classes that wrap the custom slot, providing
;; seamless set/get with type conversion / checking, etc
(defstruct model-slot
  "Structure for custom slot definitions"
  name type default field not-null)

(defstruct submodel
  "Structure for submodel data"
  type id assoc)


;;
;; Main model class definition

(defclass <model> ()
  ((id :accessor id :initarg :id :initform nil)
   (created-at :accessor created-at :initform nil)
   (last-modified :accessor last-modified :initform nil)
   (sql-table :reader sql-table :initform nil)
   (field-defs :reader field-defs :initform nil)
   (submodels :accessor submodels :initform nil))
  (:documentation "Base class for OOOK models"))


(defun submodel (inst type)
  "Find the submodel of type `type' in `inst'"
  (loop for sm in (submodels inst)
        when (eql (submodel-type sm) type)
          collect sm))

(defun submodel-id (inst type)
  "Get the ID of the submodel `type' in `inst'"
  (submodel-id (submodel inst type)))

(defmacro make-submodel-assoc-finder (assoc)
  "Make a function that finds all submodels of `assoc' in a model"
  (let ((inst (gensym "inst")))
    `(defun ,(intern (symbol-name assoc)) (,inst)
       (loop for sm in (submodels ,inst)
             when (eql (submodel-assoc sm) ,assoc)
               collect it))))

(make-submodel-assoc-finder :has-one)
(make-submodel-assoc-finder :belongs-to)
(make-submodel-assoc-finder :owns-one)
(make-submodel-assoc-finder :owns-many)

#|
(defun get-table-fields (inst)
  "Get all table field names in this model"
  (append (mapcar #'sql-field *builtin-fields*)
          (mapcar #'(lambda (field)
                      (aif (field-def-)))
           (field-defs inst))))

(defun get-table-slots (inst)
  "Get all slots that map to table fields in this model"
  (append *builtin-fields*
          (mapcar #'submodel-type (submodels inst))
          (custom-slot-names inst)))
|#


(defmethod initialize-instance :after ((inst <model>) &rest args)
  "Initialise a new model"
  (declare (ignore args))
  ;; Ensure all custom slots are the correct type
  (mapcar (lambda (slot) (ensure-slot-type inst
                                      (model-slot-name slot)
                                      (model-slot-type slot)))
          (field-defs inst))
  ;; Do the same with the builtin types
  (ensure-slot-type inst 'id :integer)
  ;; TODO set id to nil if it is zero
  ;; TODO ensure not-nils
  (mapcar (lambda (slot)
            (ensure-slot-type inst slot :timestamp))
          '(created-at last-modified)))


(defgeneric save! (inst)
  (:documentation "Either update or create a row for `inst', as appropriate")
  (:method ((inst <model>))
    ;; has-one: ensure the FK ID is correct
    (loop for right in (has-one inst) do
      (aif (slot-value inst right)
           (setf (submodel-id (submodel inst right))
                 (id it))))
    ;; TODO update last-modified
    ;; Save the model itself
    (if (id inst)
        (update! inst)
        (create! inst))
    ;; TODO: ensure the belongs-to reference has not changed
    ;; owns-one, owns-many: ensure the FK for this instance is correct in right
    (loop for right in (owns-one inst) do
      (update-owns-one inst right))
    (loop for right in (owns-many inst) do
      (update-owns-many inst right))))


(defgeneric destroy! (inst)
  (:documentation "Remove this model from the db")
  (:method ((inst <model>))
    (let ((id (id inst)))
      (when (not (or (null id)
                     (= 0 id)))
        (db:exec (delete-from (sql-table inst)
                   (where (:= :id id))))
        ;; Remove associated models
        ;; TODO
        ))))


(defun build-select-query (table selectors queries)
  "Build an sql query to run on `table'. `selectors' is a list of fields to
select and `queries' is a of where clauses"
  (assert (= 0 (mod (length queries) 3)))
  (let* ((fields (apply #'sxql:make-clause :fields selectors))
         (from (sxql:make-clause :from table))
         (where-clauses
           `(:and ,@(loop for (f op v) on queries by #'cdddr while v
                          collecting `(,op ,(sql-field-keyword f) ,v))))
         (where (list (sxql:make-clause :where where-clauses))))
    (apply #'make-statement :select fields from (if queries where))))


;; TODO there is a gotcha: if you pass nil as a query param, it is ignored. What
;; is equivalent to NULL?
;; TODO options (e.g. limit, sort, offset, etc)
;; TODO make queries a proper lambda list
;; TODO pass in the db:get function (e.g. get-one)
(defgeneric find! (type &rest queries)
  (:documentation
   "Find all rows in the table for model `type' that match `queries'")

  (:method (type &rest queries)
    (let* ((inst (make-instance type))
           (selectors (mapcar #'make-keyword (get-table-fields inst)))
           (slot-names (get-table-slots inst))
           (qry (build-select-query (sql-table inst) selectors queries))
           (results
             (db:get-all qry)))
      ;; Create an instance and fill slots with data from the db
      ;; NOTE: children (has-one, owns-one, owns-many) are retrieved lazily
      (mapcar #'(lambda (result)
                  (let ((inst (make-instance type)))
                    (loop for slot in slot-names
                          for value in result
                          do (setf (slot-value inst slot) value))
                    inst))
              results))))


;; TODO: use :around to wrap each method in an sql transaction
(defmethod save! :around ((inst <model>))
  (call-next-method))

;; TODO: use :around to wrap each method in an sql transaction
(defmethod destroy! :around ((inst <model>))
  (call-next-method))
