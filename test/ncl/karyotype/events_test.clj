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

(ns ncl.karyotype.events_test
  (:use [clojure.test])
  (:require
   [ncl.karyotype.events :as e]
   [ncl.karyotype.human :as h]
   [tawny.owl :as o]
   [tawny.reasoner :as r]))

(defn ontology-reasoner-fixture [tests]
  (r/reasoner-factory :hermit)
  (o/ontology-to-namespace e/events)
  (binding [r/*reasoner-progress-monitor*
            (atom
             r/reasoner-progress-monitor-silent)]
    (tests)))

(use-fixtures :once ontology-reasoner-fixture)

(deftest Basic
  (is (r/consistent?))
  (is (r/coherent?)))

(deftest get-telomere

  (is (= h/HumanChromosome1BandpTer
         (#'ncl.karyotype.events/get-telomere
          h/HumanChromosome1Bandp10)))

  (is (= h/HumanChromosome1BandqTer
         (#'ncl.karyotype.events/get-telomere
          h/HumanChromosome1Bandq10)))

  (is (= h/HumanChromosome1Telomere
         (#'ncl.karyotype.events/get-telomere
          h/HumanChromosome1Band)))

  (is (= h/HumanTelomere
         (#'ncl.karyotype.events/get-telomere
          h/HumanChromosomeBand)))

  ;;CORRECT?
  (is (= h/HumanChromosome1BandqTer
         (#'ncl.karyotype.events/get-telomere
          h/HumanChromosome1BandqTer)))

  (is (= h/HumanChromosome1Telomere
         (#'ncl.karyotype.events/get-telomere
          h/HumanChromosome1Telomere)))

  (is (= h/HumanChromosome1Telomere
         (#'ncl.karyotype.events/get-telomere
          h/HumanChromosome1Centromere)))

  (is (= h/HumanTelomere
         (#'ncl.karyotype.events/get-telomere
          h/HumanTelomere)))

  (is (= h/HumanTelomere
         (#'ncl.karyotype.events/get-telomere
          h/HumanCentromere)))

;;   (is (thrown?
;;        IllegalArgumentException
;;        "HumanChromosome1Centromere"
;;          (#'ncl.karyotype.events/get-telomere
;;           h/HumanChromosome1Centromere)))

)

(deftest addition
  (is (= (o/exactly 1 e/hasDirectEvent
                    (o/owl-and e/Addition h/HumanChromosome1))
         (e/addition 1 h/HumanChromosome1)))

  (is (= (o/exactly 1 e/hasDirectEvent
                    (o/owl-and e/Addition h/HumanChromosome))
         (e/addition 1 h/HumanChromosome)))

  (is (= (o/exactly 1 e/hasDirectEvent
                    (o/owl-and e/Addition
                              (o/owl-some e/hasBreakPoint
                                         h/HumanChromosome1Bandp11)))
         (e/addition 1 h/HumanChromosome1Bandp11)))

  (is (= (o/exactly 1 e/hasDirectEvent
                    (o/owl-and e/Addition
                              (o/owl-some e/hasBreakPoint
                                         h/HumanChromosome1Bandp)))
         (e/addition 1 h/HumanChromosome1Bandp)))

  (is (= (o/exactly 1 e/hasDirectEvent
                    (o/owl-and e/Addition
                              (o/owl-some e/hasBreakPoint
                                         h/HumanChromosome1Band)))
         (e/addition 1 h/HumanChromosome1Band)))
)

;; (deftest deletion
;;   (is (= (o/exactly 1 e/hasDirectEvent
;;                     (o/owl-and e/Deletion h/HumanChromosome1))
;;          (e/addition 1 h/HumanChromosome1)))

;;   (is (= (o/exactly 1 e/hasDirectEvent
;;                     (o/owl-and e/Deletion h/HumanChromosome))
;;          (e/addition 1 h/HumanChromosome)))

  ;; (is (= (o/exactly 1 e/hasDirectEvent
  ;;                   (o/owl-and e/Deletion
  ;;                             (o/owl-some e/hasBreakPoint
  ;;                                        h/HumanChromosome1Bandp11
  ;;                                        h/HumanChromosome1BandpTer)))
  ;;        (e/addition 1 h/HumanChromosome1Bandp11)))

  ;; (is (= (o/exactly 1 e/hasDirectEvent
  ;;                   (o/owl-and e/Deletion
  ;;                             (o/owl-some e/hasBreakPoint
  ;;                                        h/HumanChromosome1Bandp
  ;;                                        h/HumanChromosome1BandpTer)))
  ;;        (e/addition 1 h/HumanChromosome1Bandp)))

  ;; (is (= (o/exactly 1 e/hasDirectEvent
  ;;                   (o/owl-and e/Deletion
  ;;                             (o/owl-some e/hasBreakPoint
  ;;                                        h/HumanChromosome1Band
  ;;                                        h/HumanChromosome1Telomere)))
  ;;        (e/addition 1 h/HumanChromosome1Band)))
;;)