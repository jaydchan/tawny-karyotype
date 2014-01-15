;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2013, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

(ns ncl.karyotype.random_test
  (:use [clojure.test])
  (:require
   [ncl.karyotype.random :as ran]
   [ncl.karyotype.base :as b]
   [ncl.karyotype.events :as e]

   [ncl.karyotype.human :as h]
   [ncl.karyotype.karyotype :as k]

   [tawny.owl :as o]
   [tawny.reasoner :as r]
   [tawny.render :as ren]))

(defn ontology-reasoner-fixture [tests]
  (r/reasoner-factory :hermit)
  (o/ontology-to-namespace ran/random)
  (binding [r/*reasoner-progress-monitor*
            (atom
            r/reasoner-progress-monitor-silent)]
    (tests)))

(use-fixtures :once ontology-reasoner-fixture)

;; to run: M-x 'lein' 'test'

;; TODO equivalency for owl classes

(deftest Basic
  (is (r/consistent?))
  (is (r/coherent?)))

(deftest Random-Sex
  (let [test_sex #{b/k46_XX b/k46_XY}
        random (ran/random-sex)]
    ;; check sex vector
    (is (every? test_sex ran/sex))

    ;; testing random-sex function
    (is (some #(= (o/iri-for-name random)
                  (o/iri-for-name %)) test_sex))
    (is (o/superclass? b/base random b/k46_XN))))

(deftest Random-Band
  (let [random (ran/random-band)]
    ;; testing get-band function
    (is (= (o/iri-for-name h/HumanChromosome1Bandp36.3)
           (o/iri-for-name (ran/get-band 1 "p36.3"))))

    ;; testing random-band function
    (is (r/isuperclass? h/human random h/HumanChromosome1Band))))

(deftest Random-Chromosome
  (let [values (merge (range 1 23) "X" "Y")
        strings (map #(str "HumanChromosome" %) values)
        list (map #(o/owl-class h/human %) strings)
        test_chrom (into #{} list)
        random (ran/random-chromosome)]
    ;; check chromosomes vector
    (is (every? test_chrom ran/chromosomes))

    ;; testing random-chromosome function
    (is (some #(= (o/iri-for-name random)
                  (o/iri-for-name %)) test_chrom))
    (is (r/isuperclass? h/human random h/HumanChromosome))))

;; TODO COMPLETE!!!
(deftest Random-Deletions
  (let [r-terminal (ren/form (ran/random-terminal-deletion))
        r-inter (ren/form (ran/random-interstitial-deletion))
        r-band (ren/form (ran/random-band-deletion-driver))
        r-chromosome (ran/random-chromosome-deletion)]

    ;; terminal

    ;; chromosomal deletion
    (let [chrom (re-find #"HumanChromosome[\dXY]+" (str r-chromosome))]
      (println r-chromosome)
      (println chrom)

      (is (re-find #"Deletion" (str r-chromosome)))
      (is (= r-chromosome
              (o/exactly 1 e/hasDirectEvent
                         (o/owl-and e/Deletion (o/owl-class h/human chrom))))))))

