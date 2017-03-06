;;;; Database interaction

(in-package :oook.db)

(defvar *open-db* nil "Handle of the currently open db")

(defun connection-settings (&optional (db :main))
  "Get config for database `db'"
  (cdr (assoc db (config :databases))))

(defvar *db-file* nil "The database to connect to")

(defun set-db (file)
  (setf *db-file* file))


(defmacro with-db ((name) &body body)
  "Execute `body' with a database connection bound"
  `(if *open-db*
       (let ((,name *open-db*))
         ,@body)
       (sqlite:with-open-database (,name *db-file*)
         ,@body)))


(defmacro atomic (&body body)
  "Evaluate `body' with the db wrapped in a transaction"
  (let ((db (gensym)))
    `(unwind-protect
          (sqlite:with-open-database (,db *db-file*)
            (setf *open-db* ,db)
            (sqlite:with-transaction ,db
              ,@body))
       (setf *open-db* nil))))


;; FIXME: these are not transactions, they are persistent connections...
(defun begin-transaction ()
  "Start a transaction"
  (setf *open-db* (sqlite:connect *db-file*)))

(defun end-transaction ()
  "End a transaction"
  (if *open-db*
      (progn
        (sqlite:disconnect *open-db*)
        (setf *open-db* nil))
      (error "Database not currently open!")))


;;;;;;;;

(defun get-one (qry &key as)
  "Execute sxql `qry' and return a single row of values"
  (with-db (db)
    (multiple-value-bind (qry-str vars) (yield qry)
      ;; (let ((qry-str ()))) ;; TODO: Remove superfluous parens for UPDATE
      (multiple-value-list
       (apply #'sqlite:execute-one-row-m-v (append (list db qry-str)
                                                   vars))))))

(defun get-all (qry &key as)
  "Execute sxql `qry' and return list of results"
  (with-db (db)
    (multiple-value-bind (qry-str vars) (yield qry)
      (apply #'sqlite:execute-to-list db qry-str vars))))

(defun exec (qry)
  "Execute sxql `qry' and don't return anything"
  (with-db (db)
    (multiple-value-bind (qry-str vars) (yield qry)
      (apply #'sqlite:execute-non-query db qry-str vars))))


(defun insert-one (qry)
  "Insert into the db and return id of last row inserted"
  (with-db (db)
    (multiple-value-bind (qry-str vars) (yield qry)
      (apply #'sqlite:execute-to-list db qry-str vars))
    (sqlite:execute-single db "SELECT last_insert_rowid()")))


;; TODO with-transaction
