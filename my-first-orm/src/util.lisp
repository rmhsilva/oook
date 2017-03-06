;;;; Shared utilities

(in-package :oook.util)


(defun sql-field (symbol)
  "Make `symbol' suitable for SQL name"
  (string-downcase (substitute #\_ #\- (symbol-name symbol))))

(defun sql-field-keyword (symbol)
  "Make `symbol' suitable for a field name as a keyword"
  (make-keyword (string-upcase (sql-field symbol))))


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
