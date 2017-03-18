;;;; Methods

(in-package :oook.methods)

;; Enable CLSQL's neat query syntax
(clsql:file-enable-sql-reader-syntax)


(defun find-by-id (type id)
  "Find a model with `id'"
  (first (clsql:select type
                       :where [= [slot-value type 'id] id]
                       :flatp t
                       :refresh t)))

(defun filter ()
  (error "Probably won't be implemented..."))


(defun new-row-p (inst)
  "Return T if `inst' does not have a row in the DB"
  (or (not (slot-boundp inst 'id))
      (null (id inst))
      (null (find-by-id (type-of inst)
                        (id inst)))))

;; FIXME there are edge cases here. There needs to be a way to determine whether
;; an instance is new or already existed. Also, prevent users from arbitrarily
;; setting the ID of an object and saving it..
(defun update-timestamps (inst)
  "Update the `created-at' and `last-modified' timestamps in `inst'"
  (flet  ((update-ts (slot)
            (setf (slot-value inst slot)
                  (clsql:get-time))))
    (if (new-row-p inst)
        (update-ts 'created-at)
        ;; Ensure the old value won't be clobbered for old rows
        (slot-makunbound inst 'created-at))
    (update-ts 'last-modified)))


(defmacro do-each-owns-many ((inst right value) &body body)
  "For each model in the owns-many relation of `inst', execute `body' with the
symbol `right' bound to the model type, and the symbol `value' bound to the
current slot-value of the relation"
  (let ((slot (gensym "slot")))
    `(loop for (,right . ,slot) in (owns-many ,inst) do
      (let ((,value (slot-value ,inst ,slot)))
        ,@body))))


(defun update-owns-many (inst right new)
  "Remove all olds that are no longer in new, and add the new!"
  (flet ((test-same-id (a b)
           ;; False if either a or b is nil, or their IDs are unequal
           (and (slot-boundp a 'id)
                (slot-boundp b 'id)
                (= (id a) (id b)))))
    (let* ((left (type-of inst))
           (left-fk (foreign-key inst))
           (old (clsql:select right
                  :where [= left-fk (id inst)]
                  :flatp t
                  :refresh t))
           ;; Use the ID to find which models are old / new
           (remove-old (set-difference old new :test #'test-same-id))
           (add-new (set-difference new old :test #'test-same-id)))
      (mapc #'destroy remove-old)
      (mapc #'(lambda (new-record)
                ;; Ensure the FK of `left' in `right' is correct
                (let ((fk-slot (cdr (assoc left (join-fks new-record)))))
                  (setf (slot-value new-record fk-slot) (id inst))))
            add-new)
      ;; Save all new, both ones that were there already, and the add-new
      (mapc #'update-timestamps new)
      (mapc #'save new))))


(defun save (inst)
  "Save `inst' and associated models"
  (clsql:with-transaction ()
    (if (new-row-p inst)
        (slot-makunbound inst 'id)
        ;; This 'links' the object to the database:
        (clsql:update-slot-from-record inst 'id))
    (update-timestamps inst)
    (clsql:update-records-from-instance inst)
    (if (null (slot-value inst 'id))               ; New row, so...
        (clsql:update-instance-from-records inst)) ; Reload to get ID
    ;; Also update joined objects
    (mapcar #'(lambda (slot)
                (when (and (slot-boundp inst slot)
                           (slot-value inst slot))
                  (let ((subinst (slot-value inst slot)))
                    (update-timestamps subinst)
                    ;; Ensure the FK is correct
                    (setf (slot-value subinst (foreign-key inst))
                          (id inst))
                    ;; FIXME If the owns-one instance has changed, the old one
                    ;; should be deleted, but that will not happen here.
                    (clsql:update-records-from-instance subinst))))
            (owns-one inst))
    (do-each-owns-many (inst model new)
      (update-owns-many inst model new))))


(defun destroy (inst)
  "Delete `inst' and associated models"
  (clsql:with-transaction ()
    (clsql:update-objects-joins (list inst) :slots :all)
    (clsql:delete-instance-records inst)
    (mapcar #'(lambda (slot)
                (when (slot-boundp inst slot)
                  (clsql:delete-instance-records (slot-value inst slot))))
            (owns-one inst))
    (do-each-owns-many (inst model instances)
      (mapcar #'clsql:delete-instance-records instances))))
