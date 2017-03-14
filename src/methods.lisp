;;;; Methods

(in-package :oook.methods)

;; Enable CLSQL's neat query syntax
(clsql:file-enable-sql-reader-syntax)


(defun find-by-id (type id)
  "Find a model with `id'"
  (first (clsql:select type :where [= [slot-value type 'id] id] :flatp t)))

(defun filter ()
  (error "Probably won't be implemented..."))


;; FIXME there are edge cases here. There needs to be a way to determine whether
;; an instance is new or already existed. Also, prevent users from arbitrarily
;; setting the ID of an object and saving it..
(defun update-timestamps (inst)
  "Update the `created-at' and `last-modified' timestamps in `inst'"
  (flet  ((update-ts (slot)
            (setf (slot-value inst slot)
                  (clsql:get-time))))
    ;; id == null is currently the only indication that the instance is new..
    (when (null (id inst))
      (update-ts 'created-at)
      ;; Ensure the old value won't be clobbered
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
           (and (id a) (id b) (= (id a) (id b)))))
    (let* ((left (type-of inst))
           (left-fk (foreign-key inst))
           (old (clsql:select right
                  :where [= left-fk (id inst)]
                  :flatp t))
           ;; Use the ID to find which models are old / new
           (remove-old (set-difference old new :test #'test-same-id))
           (add-new (set-difference new old :test #'test-same-id)))
      (mapc #'destroy remove-old)
      (mapc #'(lambda (new)
                (update-timestamps new)
                ;; Ensure the FK of `left' in `right' is correct
                (let ((slot (cdr (assoc left (join-fks new)))))
                  (setf (slot-value new slot) (id inst))))
            add-new)
      ;; Save all new, both ones that were there already, and the add-new
      (mapc #'save new))))


(defun save (inst)
  "Save `inst' and associated models"
  (clsql:with-transaction ()
    (update-timestamps inst)
    (clsql:update-records-from-instance inst)
    (if (null (slot-value inst 'id))
        (clsql:update-instance-from-records inst)) ; Reload to get ID
    ;; FIXME If the owns-one instance has changed, the old one should be deleted,
    ;; but that will not happen here.
    (mapcar #'(lambda (slot)
                (when (slot-boundp inst slot)
                  (let ((subinst (slot-value inst slot)))
                    (update-timestamps subinst)
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
