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

;; (o/defontology to
;;   :iri "http://ncl.ac.uk/karyotype/test"
;;   :prefix "test:"
;;   :comment "Test ontology for Human Karyotype Ontology, written using
;;   the tawny-owl library.")

(deftest Basic
  (is (r/consistent?))
  (is (r/coherent?)))

;; TODO
;; (deftest Parentband?)

;; REFRESH
(deftest Get-Telomere
  (is (= h/HumanChromosome1BandpTer
         (e/get-telomere h/HumanChromosome1Bandp10)))
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

;; unlike exactly, owl-some returns a lazyseq of hasEvent restrictions
(deftest Some-Event
  (let [events (e/some-event
                (o/owl-and e/Addition h/HumanChromosome1 h/HumanChromosome12))]
    (doseq [event events]
      (is (instance? org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom event))
      (is (re-find #"hasEvent" (str event)))
      (is (not (re-find #"hasDirectEvent" (str event)))))))

(deftest Exactly-Event
  (let [event (e/exactly-event 1 (o/owl-and e/Addition h/HumanChromosome1))]
    (is (instance? org.semanticweb.owlapi.model.OWLObjectExactCardinality event))
    (is (re-find #"hasEvent" (str event)))
    (is (not (re-find #"hasDirectEvent" (str event))))))

(deftest Event
  (let [exact (e/event 1 (o/owl-and e/Addition h/HumanChromosome1))
        some (e/event nil (o/owl-and e/Addition
                                     h/HumanChromosome1 h/HumanChromosome12))
        events (flatten [exact some])]

    (doseq [event events]
      (is (instance? org.semanticweb.owlapi.model.OWLRestriction event))
      ;; TOFIX
      ;; (is (some #(instance? % event)
      ;;           [org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom
      ;;            org.semanticweb.owlapi.model.OWLObjectExactCardinality]))
      (is (re-find #"hasEvent" (str event)))
      (is (not (re-find #"hasDirectEvent" (str event)))))))

(deftest Some-Direct-Event
  (let [events (e/some-direct-event
                (o/owl-and e/Addition h/HumanChromosome1 h/HumanChromosome12))]
    (doseq [event events]
      (is (instance? org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom event))
      (is (re-find #"hasDirectEvent" (str event)))
      (is (not (re-find #"hasEvent" (str event)))))))

(deftest Exactly-Direct-Event
  (let [event (e/exactly-direct-event
               1 (o/owl-and e/Addition h/HumanChromosome1))]
    (is (instance? org.semanticweb.owlapi.model.OWLObjectExactCardinality event))
    (is (re-find #"hasDirectEvent" (str event)))
    (is (not (re-find #"hasEvent" (str event))))))

(deftest Direct-Event
  (let [exact (e/direct-event 1 (o/owl-and e/Addition h/HumanChromosome1))
        some (e/direct-event nil (o/owl-and e/Addition
                                     h/HumanChromosome1 h/HumanChromosome12))
        events (flatten [exact some])]

    (doseq [event events]
      (is (instance? org.semanticweb.owlapi.model.OWLRestriction event))
      ;; TOFIX
      ;; (is (some #(instance? % event)
      ;;           [org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom
      ;;            org.semanticweb.owlapi.model.OWLObjectExactCardinality]))
      (is (re-find #"hasDirectEvent" (str event)))
      (is (not (re-find #"hasEvent" (str event)))))))


;; TODO
;; (deftest Addition-Chromosome)
;; (deftest Addition-Band)

;; REFRESH
(deftest Addition
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

;; TODO