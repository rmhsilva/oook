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


(defun search-like (model column &key like (singlep nil))
  "Search the DB for `model' using LIKE to match column. If `singlep' is t, only
one result is returned"
  (let* ((qry (util:s+ "%" like "%"))
         (results (clsql:select model
                    :where [like [slot-value model column] qry]
                    :limit (if singlep 1)
                    :flatp t)))
    (if singlep
        (car results)
        results)))

;; (defun count (model))
