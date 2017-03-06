;;;; Testing OOOK

(defpackage :test-oook (:use :cl :2am :oook :test-oook-fixtures))
(in-package :test-oook)


(test create-unit
#|
  (finishes
   (let ((inst (make-instance 'unit :name "g" :type "mass"))
         (new-id (save! inst)))

     (is (numberp new-id)
         "The new ID should be a number")

     (is (> new-id 0)
         "A valid ID is greater than zero")
     )
   "The call to save! should succeed")
  |#
  )


(test find!-unit
  #|
  (finishes
   (let* ((res (find! 'unit)))

     (is (listp res)
         "Result from find! should be a list")

     (is (= (length res) 2)
         "There are two in the DB")

     (is (eql 'unit (type-of (first res)))
         "The results should be instances of unit")))
  |#
  )

(suite 'model)

(test build-select-query-single
  (multiple-value-bind
        (qry-str vars)
      (sxql:yield (oook.model::build-select-query :table
                                                  '(:id :bla)
                                                  '(:id := 2)))

    (is (equal qry-str
               "SELECT id, bla FROM table WHERE (id = ?)")
        "The query should be well formed")

    (is (equal vars '(2))
        "The query vars should be registered")))


(test build-select-query-multiple
  (multiple-value-bind
        (qry-str vars)
      (sxql:yield (oook.model::build-select-query :table
                                                  '(:id :bla)
                                                  '(:id := 2
                                                    :bla := 5)))

    (is (equal qry-str
               "SELECT id, bla FROM table WHERE ((id = ?) AND (bla = ?))")
        "The query should be well formed")

    (is (equal vars '(2 5))
        "The query vars should be registered")))


