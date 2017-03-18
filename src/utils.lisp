;;;; Shared utilities

(in-package :oook.utils)

(clsql:file-enable-sql-reader-syntax)


(defun sql-field (symbol)
  "Make `symbol' suitable for SQL name"
  (string-downcase (substitute #\_ #\- (symbol-name symbol))))

(defun sql-field-keyword (symbol)
  "Make `symbol' suitable for a field name as a keyword"
  (alexandria:make-keyword (string-upcase (sql-field symbol))))


;;
;; Type Helpers

(defun ensure-integer (thing)
  "Return `thing' as an integer"
  (if (integerp thing)
      thing
      (if (stringp thing)
          (if (> (length thing) 0)
              (parse-integer thing)
              nil)
          (error "Don't know how to get an integer from thing"))))


(defmacro ensure-slot-type (inst slot type)
  "Create form to ensure that `slot' in `inst' is of type `type'"
  (alexandria:once-only (slot type)
    (symbol-macrolet ((val `(slot-value ,inst ,slot)))
      `(setf ,val
             (cond
               ((eql :integer ,type) (ensure-integer ,val))
               ((eql :timestamp ,type) (ensure-integer ,val))
               ;; TODO add more types (real, number, etc)
               (t ,val))))))


;; TODO: reimpliment this with defgeneric and methods
(defun parse-as-type (value type)
  "Try to parse `type' from input `value'"
  (flet ((coerce-to (type)
           (alexandria:eswitch (type)
             ('null (etypecase value
                      (null nil)))
             ('string (etypecase value
                        (null nil)
                        (string value)))
             ('integer (etypecase value
                         (null nil)
                         (integer value)
                         (string (if (> (length value) 0)
                                     (parse-integer value)
                                     nil))))
             ('float (etypecase value
                       (null nil)
                       (float value)
                       (string (if (> (length value) 0)
                                   (float (parse-number value))
                                   nil))))
             ('boolean (etypecase value
                         (null nil)
                         (string (or (equal value "1")
                                     (equal value "t")
                                     (equal value "true")))
                         (boolean value)
                         (integer (/= 0 value))))
             ('clsql:wall-time nil))))

    (if (listp type)
        (progn (assert (or (eql (car type) 'and)
                           (eql (car type) 'or)))
               ;; TODO handle this properly
               ;; (dolist (current type)
               ;;   (handler-case (parse value type)))
               (coerce-to (find-if-not #'(lambda (el) (member el '(or and null)))
                                       type)))
        (coerce-to type))))


(defun search-like (model column &key like (singlep nil) (fmt "%~A%"))
  "Search the DB for `model' using LIKE to match column. If `singlep' is t, only
one result is returned"
  (assert (not (null like)))
  (let* ((qry (format nil fmt like))
         (results (clsql:select model
                    :where [like [slot-value model column] qry]
                    :limit (if singlep 1)
                    :flatp t)))
    (if singlep
        (car results)
        results)))


(defmacro def-enhanced-printer (type &key slot)
  "Enhances `print-object' for `type', adding the `slot' attribute to it"
  (alexandria:with-gensyms (inst stream name)
    `(defmethod print-object ((,inst ,type) ,stream)
       (let ((,name (if (slot-boundp ,inst ,slot)
                        (slot-value ,inst ,slot)
                        "-")))
         (print-unreadable-object (,inst ,stream :type t)
           (format ,stream "\"~A\"" ,name))))))
