;;;; HTML helpers

(in-package :oook.serialise)

;; I want to do things like:
;; (gen-html-table 'model records)

(defun all-same-type-p (records)
  "Check everything in `records' is the same type"
  (let ((expected-type (type-of (first records))))
    (every #'identity (mapcar #'(lambda (current)
                                  (equal (type-of current) expected-type))
                              records))))


(deftag ui/table (body attrs &key headings)
  (let ((current (gensym "current")))
    `(:table.ui.basic.table
      ,@attrs
      (:thead (:tr (dolist (,current ,headings)
                     (:th ,current))))
      (:tbody ,@body))))

(deftag ui/field (extra attrs &key label div-classes)
  "A form field"
  `(:div.field :class ,div-classes
    ,(if label `(:label ,label))
    (:input ,@attrs)
    ,@extra))

(deftag ui/checkbox (checked attrs &key label)
  "A checkbox"
  `(:div.ui.checkbox
    (:input.hidden :type "checkbox"
                   :name name
                   ,@attrs
     :checked ,(if checked "checked" nil))
    (:label ,label)))


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

(defun field-for (class slot &optional value)
  (let* ((container-name (format nil "~(~a~)" (class-name (find-class class))))
         (label (field-label-for slot))
         (name (field-name-for container-name slot))
         (field-type (slot-type-without-null class slot)))
    (alexandria:eswitch (field-type)
      ;; TODO create more different fields depending on type
      ('boolean (spinneret:with-html
                  (:div.inline.field
                   (ui/checkbox :name name :label label :value value))))
      ('integer (ui/field :type "number" :name name :label label :value value))
      ('string (ui/field :type "text" :name name :label label :value value)))))


(defmacro with-record-type ((class) &body body)
  `(flet ((field-for-slot (slot &optional value)
            (field-for ,class slot value)))
     ,@body))

;; Test:
;; (with-record-type ('recipemaster.models:ingredient)
;;   (field-for-slot 'recipemaster.models:name)
;;   (field-for-slot 'recipemaster.models:optimise))


(defun gen-html-table (records &key exclude-slots table-classes)
  "Print an HTML table listing the data in `records'"
  (let ((slots (set-difference (serialisable-fields (first records))
                               exclude-slots))
        (joined-tables (join-fks (first records))))
    ;; TODO this is icky, doesn't provide a nice restart
    (unless (all-same-type-p records)
      (error "Multiple types found in RECORDS"))
    (spinneret:with-html
      (table :headings '()
             (dolist (current record)
               (with-slots slots current)
               (td
                ))))))


(defun get-edit-form (record action &key exclude-slots)
  (let ((slots (set-difference (oook.macro:deserialisable-fields record)
                               (append exclude-slots
                                       '(oook.macro:id)))))
    (print slots)
    (spinneret:with-html
      (with-record-type ((type-of record))
        (:form.ui.form
         :action action
         :method "POST"
         (dolist (current slots)
           (field-for-slot current)))))))

;; (get-edit-form (make-instance 'recipemaster.models:ingredient)
;;                "/save")
