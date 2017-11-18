;;;; HTML helpers

(in-package :oook.serialise)

(defun all-same-type-p (records)
  "Check everything in `records' is the same type"
  (let ((expected-type (type-of (first records))))
    (every #'identity (mapcar #'(lambda (current)
                                  (equal (type-of current) expected-type))
                              records))))



;;; Editing records


;; Create the form automatically

(defun slot-type-without-null (class slot)
  "If a slot type is something like (OR NULL BOOLEAN), return just BOOLEAN"
  (let ((the-type (get-slot-type class slot)))
    (if (listp the-type)
        (car (set-difference the-type
                             '(null and or)))
        the-type)))

(defun field-name-for (container-name slot)
  (format nil "~a[~(~a~)]"
          container-name (substitute #\_ #\- (symbol-name slot))))

(defun field-label-for (slot)
  (format nil "~:(~a~)" (substitute #\Space #\- (symbol-name slot))))

(defun field-for-normal-slot (class slot &optional value)
  (let* ((container-name (format nil "~(~a~)" (class-name (find-class class))))
         (label (field-label-for slot))
         (name (field-name-for container-name slot))
         (field-type (slot-type-without-null class slot)))
    (alexandria:eswitch (field-type)
      ;; TODO create more different fields depending on type
      ('boolean (spinneret:with-html
                  (:div.inline.field
                   (ss:checkbox :name name :label label :value value))))
      ('integer (ss:field :type "number" :name name :label label :value value))
      ('clsql:wall-time (ss:field :type "date" :name name :label label :value value))
      ('float (ss:field :type "number" :name name :label label :value (coerce value 'single-float)))
      ('number (ss:field :type "number" :name name :label label :value (coerce value 'single-float)))
      ('string (ss:field :type "text" :name name :label label :value value)))))

;; (defun field-for-join-slot)


(defmacro with-record-type ((class) &body body)
  ;; let joined-slots ...
  `(flet ((field-for-slot (slot &optional value)
            ;; if slot-is-for-other-model
            (field-for-normal-slot ,class slot value)))
     ,@body))

;; Test:
;; (with-record-type ('recipemaster.models:ingredient)
;;   (field-for-slot 'recipemaster.models:name)
;;   (field-for-slot 'recipemaster.models:optimise t))


;; TODO: link to the edit page (route somehow) with the product ID
(defun gen-html-table (records &key exclude-slots table-classes)
  "Print an HTML table listing the data in `records'"
  (let ((slots (set-difference (serialisable-fields (first records))
                               exclude-slots)))
    ;; TODO this is icky, doesn't provide a nice restart
    (unless (all-same-type-p records)
      (error "Multiple types found in RECORDS"))
    (spinneret:with-html
      (ss:table
        :trs (mapcar #'symbol-name slots)
        :class table-classes
        :tds (mapcar
              (lambda (current-rec)
                (loop for current-slot in slots
                      collect (slot-value current-rec current-slot)))
              records)))))


(defun get-edit-form (record action &key exclude-slots)
  (let ((slots (set-difference (oook.macro:deserialisable-fields record)
                               (append exclude-slots
                                       '(oook.macro:id)))))
    (spinneret:with-html
      (with-record-type ((type-of record))
        (:form.ui.form
         :action action
         :method "POST"
         (dolist (current-slot slots)
           (field-for-slot current-slot (when (slot-boundp record current-slot)
                                          (slot-value record current-slot))))
         (:button.ui.primary.button :type "submit" "Save"))))))

;; (get-edit-form (make-instance 'recipemaster.models:ingredient)
;;                "/save")
