;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2012-2015, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see http://www.gnu.org/licenses/.

(ns ncl.karyotype.iscnexamples_subset_test
  (:use [clojure.test])
  (:require
   [ncl.karyotype.iscnexamples_subset :as i]
   [tawny.owl :as o]
   [tawny.reasoner :as r]))

(defn ontology-reasoner-fixture [tests]
  (r/reasoner-factory :hermit)
  (o/ontology-to-namespace i/iscnexamples_subset)
  (binding [r/*reasoner-progress-monitor*
            (atom
            r/reasoner-progress-monitor-silent)]
    (tests)))

(use-fixtures :once ontology-reasoner-fixture)

(deftest Basic
  (is (r/consistent?))
  (is (r/coherent?)))

(deftest FemaleKaryotype
  (is (r/isuperclass? i/k45_XX_-22 i/FemaleKaryotype))
  (is (not (r/isuperclass? i/k45_X i/FemaleKaryotype)))
  (is (not (r/isuperclass? i/k46_Xc_+21 i/FemaleKaryotype)))
  )

(deftest MaleKaryotype
  (is (r/isuperclass? i/k46_XY_+21c_-21 i/MaleKaryotype))
  (is (r/isuperclass? i/k46_XY_+21c_-21 i/MaleKaryotype))
  (is (not (r/isuperclass? i/k45_X i/MaleKaryotype)))
  (is (not (r/isuperclass? i/k46_Xc_+21 i/MaleKaryotype)))
  )
