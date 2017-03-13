;;;; Serialisation things

(in-package :oook.serialise)


;; (defun count (model))
(defvar *serialisation-options*
  '(:include-joins t :slots nil)
  "Options that control how objects are serialised")


(defmacro with-serialisation-options ((&rest options) &body body)
  "Execute `body' with json options set to `options'"
  `(let ((*serialisation-options* ',options))
     ,@body))


(defun to-alist (inst)
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

    (let* ((include-joins (getf *serialisation-options* :include-joins))
           (slots-to-include
             (append (serialisable-fields inst)
                     (if include-joins
                         (serialisable-joins inst)))))

      (if include-joins
          (clsql:update-objects-joins (list inst) :slots :all))

      (mapcar #'get-assoc slots-to-include))))


;; TODO: include a mapping of string names to classes to enable submodel
;; population here
(defun from-alist (type alist)
  "Create an instance of `type', given `alist' containing mapping of slots to
values, in caveman2 parsed params format"
  (let ((inst (make-instance type))
        (include-joins (getf *serialisation-options* :include-joins)))

    (labels ((get-initform (slot fn)
               (list (alexandria:make-keyword slot)
                     (funcall fn slot)))
             (get-base-slot-value (slot)
               (cdr (assoc (sql-field slot) alist :test #'string=)))
             (get-join-slot-value (slot)
               ;; hacky inverse alist lookup for owns-many class name:
               (let ((value (cdr (assoc (sql-field slot) alist :test #'string=)))
                     (owns-many-slots (owns-many inst)))
                 (aif (position slot (mapcar #'cdr owns-many-slots))
                      (mapcar (alexandria:curry #'from-alist
                                                (car (nth it owns-many-slots)))
                              value)
                      (from-alist slot value)))))

      (let ((init-args
              (append (mapcan #'(lambda (slot)
                                  ;; get the value of each 'base' slot
                                  (get-initform slot #'get-base-slot-value))
                              (serialisable-fields inst))
                      (if include-joins
                          (mapcan (lambda (slot)
                                    ;; get the value of each 'join' slot
                                    (get-initform slot #'get-join-slot-value))
                                  (serialisable-joins inst))))))
        (apply #'make-instance type init-args)))))


;; jonathan doesn't do nested alists very well...

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
