;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2014, Newcastle University

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

(ns ncl.karyotype.affects1_test
  (:use [clojure.test])
  (:require
   [ncl.karyotype.affects1 :as a]
   [ncl.karyotype.human :as h]
   [tawny.owl :as o]
   [tawny.reasoner :as r]))

(defn ontology-reasoner-fixture [tests]
  (r/reasoner-factory :hermit)
  (o/ontology-to-namespace a/affects1)
  (binding [r/*reasoner-progress-monitor*
            (atom
             r/reasoner-progress-monitor-silent)]
    (tests)))

(use-fixtures :once ontology-reasoner-fixture)

(deftest Basic
  (is (r/consistent?))
  (is (r/coherent?)))

;; REPEAT? Should also check 300-bands vector
(deftest Get-Band
  (let [band (a/get-band 1 "p36.3")]
    (is (instance? org.semanticweb.owlapi.model.OWLClassExpression band))
    (is (= (o/iri-for-name h/HumanChromosome1Bandp36.3)
           (o/iri-for-name band)))))

(deftest Not-Breakpoint?
  ;; valid inputs
  (let [inputs [h/HumanChromosome1Bandp36.31 h/HumanChromosome1Bandp10
                h/HumanChromosome1BandpTer h/HumanChromosome1Bandp
                h/HumanChromosome1Bandq h/HumanChromosome1Band
                h/HumanChromosomeBand]]

    (doseq [input inputs]
      (is (instance?
           org.semanticweb.owlapi.model.OWLClassExpression
           input))
      (is (h/band? input))
      (is (false? (a/not-breakpoint? input input))))
    (is (every? true? (map #(a/not-breakpoint? %1 %2)
                           inputs
                           (replace
                            {h/HumanChromosome1Bandp h/HumanChromosome1Bandq}
                            (reverse inputs))))))
  ;; invalid input
  (is (thrown?
       AssertionError
       "HumanChromosome"
       (a/not-breakpoint? h/HumanChromosomeBand h/HumanChromosome))))

;; (deftest Subset)
;; (deftest Band-Range)
;; (deftest Get-Affects)
;; (deftest Get-Affects1)
;; (deftest Affects1-Driver)