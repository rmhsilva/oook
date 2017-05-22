;;;; Serialisation things

(in-package :oook.serialise)


;; (defun count (model))
(defvar *serialisation-options*
  '(:include-joins t :slots nil)
  "Options that control how objects are serialised")

;; TODO: add an option to exclude timestamp data. Or exclude by default...


(defmacro with-serialisation-options ((&rest options) &body body)
  "Execute `body' with json options set to `options'"
  `(let ((*serialisation-options* ',options))
     ,@body))


(defun to-alist (inst &key include-joins)
  "Return an a-list representation of `inst'"
  (labels ((get-value (inst slot)
             (let ((value (slot-value inst slot)))
               (cond
                 ((listp value)
                  (mapcar #'to-alist value))
                 ((typep value 'clsql:standard-db-object)
                  (to-alist value))
                 (t value))))

           (get-assoc (slot)
             (let ((key (sql-field slot))
                   (value (get-value inst slot)))
               (cons key value))))

    (let* ((include-joins (or (getf *serialisation-options* :include-joins)
                              include-joins))
           (slots-to-include
             (append (serialisable-fields inst)
                     (if include-joins
                         (serialisable-joins inst)))))

      (if include-joins
          (clsql:update-objects-joins (list inst) :slots :all))

      (mapcar #'get-assoc slots-to-include))))


(defun get-slot-type (class slot)
  "Get the type of `slot' in `class'"
  (closer-mop:slot-definition-type
   (find slot
         (closer-mop:class-direct-slots (find-class class))
         :key #'(lambda (s) (closer-mop:slot-definition-name s)))))


;; from-list TODO:
;;
;; - provide a mechanism to set specific slots only
;; - handle missing slots (e.g. checkboxes don't POST anything)
(defun from-alist (class alist &key include-joins)
  "Create an instance of `class', given `alist' containing mapping of slots to
values, in caveman2 parsed params format."
  (let* ((inst (make-instance class))
         (slots (macro:deserialisable-fields inst))
         (include-joins (and (getf *serialisation-options* :include-joins)
                             include-joins)))

    (labels ((get-initform (slot type)
               (aif (if (eql type :join)
                        (get-join-slot-value slot)
                        (get-base-slot-value slot type))
                    (cons slot it)))
             (get-base-slot-value (slot type)
               (aif (cdr (assoc (sql-field slot) alist :test #'string=))
                    (oook.utils:parse-as-type it type)))
             (get-join-slot-value (slot)
               ;; hacky inverse alist lookup for owns-many class name:
               (let ((value (cdr (assoc (sql-field slot) alist :test #'string=)))
                     (owns-many-slots (owns-many inst)))
                 ;; Exclude slots that have no value in the alist
                 (unless (null value)
                   (aif (position slot (mapcar #'cdr owns-many-slots))
                        (mapcar (alexandria:curry #'from-alist
                                                  (car (nth it owns-many-slots)))
                                value)
                        ;; It's not in owns-many, value is a single thing
                        (from-alist slot value))))))

      (let ((init-args
              (append (mapcar #'(lambda (slot)
                                  ;; get the value of each 'base' slot
                                  (get-initform slot (get-slot-type class slot)))
                              slots)
                      (if include-joins
                          (mapcar (lambda (slot)
                                    ;; get the value of each 'join' slot
                                    (get-initform slot :join))
                                  (serialisable-joins inst))))))

        (dolist (slot (remove-if #'null init-args) inst)
          (setf (slot-value inst (car slot)) (cdr slot)))))))


;;
;; jonathan doesn't do nested alists very well...
;; Lets just define the to-json manually!

(defmethod jonathan:%to-json ((inst clsql:standard-db-object))
  "Return a JSON representation of `inst'"
  (let* ((include-joins (getf *serialisation-options* :include-joins))
         (slots (aif (getf *serialisation-options* :slots) it
                     (append (serialisable-fields inst)
                             (if include-joins
                                 (serialisable-joins inst))))))
    (if include-joins
        (clsql:update-objects-joins (list inst) :slots :all))
    (jonathan:with-object
      (loop for slot in slots
            for key = (alexandria:make-keyword (sql-field slot))
            do (jonathan:write-key-value key (slot-value inst slot))))))


;;
;; And converters for special types

(defmethod jonathan:%to-json ((time clsql:wall-time))
  "Convert a wall-time object to JSON"
  (jonathan:with-object
    (jonathan:write-key-value "date_str" (clsql:print-date time))
    (jonathan:write-key-value "mjd" (clsql:time-mjd time))))


;;
;; Pretty printing! TODO shouldn't really be in here...

(defun pprint-model (inst &key (stream t) (slots nil) include-joins)
  "Pretty print `inst'"
  (if (null inst)
      (progn
        (warn "`inst' given to `pprint-model' is null")
        (format stream "NIL"))
      (let* ((include-joins (or (getf *serialisation-options* :include-joins)
                                include-joins))
             (slots (aif slots it
                         (append (serialisable-fields inst)
                                 (if include-joins
                                     (serialisable-joins inst))))))
        ;; TODO: nested models
        (format stream
                "[~A]~%~{~{ - ~A: ~A~%~}~}"
                (type-of inst)
                (loop for slot in slots
                      when (slot-boundp inst slot)
                        collect (list slot (slot-value inst slot)))))))
