;;;; Fixtures

(defpackage :test-oook-fixtures (:use :cl :oook))
(in-package :test-oook-fixtures)

(defmodel unit ()
  "A measurement unit"
  name
  (unit-type :type :string :field "type"))

(defmodel ingredient (:has-one (unit) :owns-many (product))
  "An ingredient"
  (name :type :string)
  (optimise :type :boolean))

(defmodel product (:belongs-to (ingredient) :has-one (unit))
  "A product one can buy"
  name
  (tpnb :type :integer)
  (price :type :real)
  (amount :type :real)
  image-url)

(defmodel recipe-step (:belongs-to (recipe))
  "A step in a recipe"
  instructions
  (step-number :type :integer))

(defmodel recipe-ingredient (:belongs-to (recipe) :has-one (ingredient unit))
  "An ingredient in a recipe"
  (amount :type :integer))

(defmodel recipe (:owns-many (recipe-ingredient recipe-step))
  "A recipe"
  name
  info
  ingredients-text
  (preptime :type :integer))


;; (defvar *unit1* (make-instance))
