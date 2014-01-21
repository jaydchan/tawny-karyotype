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
         (e/get-telomere h/HumanChromosome1Bandq10)))
  (is (= h/HumanChromosome1Telomere
         (e/get-telomere h/HumanChromosome1Band)))
  (is (= h/HumanTelomere
         (e/get-telomere h/HumanChromosomeBand)))

  ;;TODO correct?
  (is (= h/HumanChromosome1BandqTer
         (e/get-telomere h/HumanChromosome1BandqTer)))
  (is (= h/HumanChromosome1Telomere
         (e/get-telomere h/HumanChromosome1Telomere)))
  (is (= h/HumanChromosome1Telomere
         (e/get-telomere h/HumanChromosome1Centromere)))
  (is (= h/HumanTelomere
         (e/get-telomere h/HumanTelomere)))
  (is (= h/HumanTelomere
         (e/get-telomere h/HumanCentromere)))

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
      (is (instance? org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom
                     event))
      (is (.isObjectRestriction event))
      (is (= (.getProperty event) e/hasEvent))
      (is (not (= (.getProperty event) e/hasDirectEvent))))))

(deftest Exactly-Event
  ;; valid input
  (let [n 1
        event (e/exactly-event n (o/owl-and e/Addition
                                            h/HumanChromosome1))]
    (is (number? n))
    (is (instance?
         org.semanticweb.owlapi.model.OWLObjectExactCardinality event))
    (is (.isObjectRestriction event))
    (is (= (.getProperty event) e/hasEvent))
    (is (not (= (.getProperty event) e/hasDirectEvent))))
  ;; invalid input
  (is (thrown? IllegalArgumentException
               (e/exactly-event "1" (o/owl-and e/Addition
                                               h/HumanChromosome1)))))

(deftest Event
  ;; valid input
  (let [n 1
        exact (e/event n (o/owl-and e/Addition h/HumanChromosome1))
        some (e/event nil (o/owl-and e/Addition
                                     h/HumanChromosome1 h/HumanChromosome12))
        events (flatten [exact some])]
    (is (number? n))
    (doseq [event events]
      (is (instance? org.semanticweb.owlapi.model.OWLRestriction event))
      (is (or
           (instance?
            org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom event)
           (instance?
            org.semanticweb.owlapi.model.OWLObjectExactCardinality event)))
      (is (.isObjectRestriction event))
      (is (= (.getProperty event) e/hasEvent))
      (is (not (= (.getProperty event) e/hasDirectEvent)))))
  ;; invalid input
  (is (thrown? IllegalArgumentException
               (e/event
                "1" (o/owl-and e/Addition h/HumanChromosome1)))))

(deftest Some-Direct-Event
  (let [events (e/some-direct-event
                (o/owl-and e/Addition h/HumanChromosome1 h/HumanChromosome12))]
    (doseq [event events]
      (is (instance?
           org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom event))
      (is (.isObjectRestriction event))
      (is (= (.getProperty event) e/hasDirectEvent))
      (is (not (= (.getProperty event) e/hasEvent))))))

(deftest Exactly-Direct-Event
  ;; valid input
  (let [n 1
        event (e/exactly-direct-event
               n (o/owl-and e/Addition h/HumanChromosome1))]
    (is (number? n))
    (is (instance? org.semanticweb.owlapi.model.OWLObjectExactCardinality
                   event))
    (is (.isObjectRestriction event))
    (is (= (.getProperty event) e/hasDirectEvent))
    (is (not (= (.getProperty event) e/hasEvent))))
  ;; invalid input
  (is (thrown? IllegalArgumentException
               (e/exactly-direct-event
                "1" (o/owl-and e/Addition h/HumanChromosome1)))))

(deftest Direct-Event
  ;; valid input
  (let [n 1
        exact (e/direct-event n (o/owl-and e/Addition h/HumanChromosome1))
        some (e/direct-event nil (o/owl-and e/Addition
                                     h/HumanChromosome1 h/HumanChromosome12))
        events (flatten [exact some])]
    (is (number? n))
    (doseq [event events]
      (is (instance? org.semanticweb.owlapi.model.OWLRestriction event))
      (is (or
           (instance?
            org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom event)
           (instance?
            org.semanticweb.owlapi.model.OWLObjectExactCardinality event)))
      (is (.isObjectRestriction event))
      (is (= (.getProperty event) e/hasDirectEvent))
      (is (not (= (.getProperty event) e/hasEvent)))))
  ;; invalid input
  (is (thrown? IllegalArgumentException
               (e/direct-event
                "1" (o/owl-and e/Addition h/HumanChromosome1)))))

(deftest Addition-Chromosome
  ;; valid inputs
  (let [inputs [h/HumanChromosome1 h/HumanAutosome h/HumanChromosome]
        events (into [] (map e/addition-chromosome inputs))]
    (doseq [i (range (count inputs))]
      (let [event (get events i)]
        (is (instance?
             uk.ac.manchester.cs.owl.owlapi.OWLObjectIntersectionOfImpl
             event))
        (is (.containsConjunct event e/Addition))
        (is (.containsConjunct event (get inputs i))))))
  ;; invalid input
  (is (thrown? AssertionError
               "HumanChromosomeBand"
               (e/addition-chromosome h/HumanChromosomeBand))))

(deftest Addition-Band
  ;; valid inputs
  (let [inputs [h/HumanChromosome1Bandp36.31 h/HumanChromosome1Bandp10
                h/HumanChromosome1BandpTer h/HumanChromosome1Bandp
                h/HumanChromosome1Bandq h/HumanChromosome1Band
                h/HumanChromosomeBand]
        events (into [] (map e/addition-band inputs))]
    (doseq [i (range (count inputs))]
      (let [event (get events i)]
        (is (instance?
             uk.ac.manchester.cs.owl.owlapi.OWLObjectIntersectionOfImpl
             event))
        (is (.containsConjunct event e/Addition))
        (doseq [conjunct (.asConjunctSet event)]
          (if (instance?
               uk.ac.manchester.cs.owl.owlapi.OWLObjectSomeValuesFromImpl
               conjunct)
            (let [property conjunct]
              (is (.isObjectRestriction property))
              (is (= (.getProperty property) e/hasBreakPoint))
              (is (= (.getFiller property) (get inputs i)))))))))
  ;; invalid input
  (is (thrown? AssertionError
               "HumanChromosome"
               (e/addition-band h/HumanChromosome)))
)

(deftest Addition
  ;; valid inputs - chromosomes
  ;; TODO Is HumanAutosome necessary?
  (let [inputs [h/HumanChromosome1 h/HumanAutosome h/HumanChromosome]
        expected (into []
                       (map #(o/exactly 1 e/hasDirectEvent
                                        (o/owl-and e/Addition %)) inputs))
        actual (into [] (map #(e/addition 1 %) inputs))]

    (doseq [i (range (count inputs))]
      (is (= (get expected i) (get actual i)))))

  ;; valid inputs - bands
  (let [inputs [h/HumanChromosome1Bandp36.31 h/HumanChromosome1Bandp10
                h/HumanChromosome1BandpTer h/HumanChromosome1Bandp
                h/HumanChromosome1Bandq h/HumanChromosome1Band
                h/HumanChromosomeBand]
        expected (into []
                       (map #(o/exactly 1 e/hasDirectEvent
                                        (o/owl-and e/Addition
                                                   (o/owl-some e/hasBreakPoint
                                                               %))) inputs))
        actual (into [] (map #(e/addition 1 %) inputs))]

    (doseq [i (range (count inputs))]
      (is (= (get expected i) (get actual i)))))

  ;; invalid input
  ;; TODO is this true?
  (is (thrown? IllegalArgumentException
               "HumanChromosome1Centromere"
               (e/addition 1 h/HumanChromosome1Centromere)))
)

(deftest Deletion-Chromosome
  ;; valid inputs
  (let [inputs [h/HumanChromosome1 h/HumanAutosome h/HumanChromosome]
        events (into [] (map e/deletion-chromosome inputs))]
    (doseq [i (range (count inputs))]
      (let [event (get events i)]
        (is (instance?
             uk.ac.manchester.cs.owl.owlapi.OWLObjectIntersectionOfImpl
             event))
        (is (.containsConjunct event e/Deletion))
        (is (.containsConjunct event (get inputs i))))))
  ;; invalid inputs
  (is (thrown? AssertionError
               "HumanChromosomeBand"
               (e/deletion-chromosome h/HumanChromosomeBand)))
)

(deftest Deletion-Band
  ;; valid inputs
  (let [inputs [h/HumanChromosome1Bandp36.31 h/HumanChromosome1Bandp10
                h/HumanChromosome1BandpTer h/HumanChromosome1Bandp
                h/HumanChromosome1Bandq h/HumanChromosome1Band
                h/HumanChromosomeBand]
        events (into [] (map #(e/deletion-band %1 %2) inputs (reverse inputs)))]
    (doseq [i (range (count inputs))]
      (let [event (get events i)
            properties
            (filter
             #(instance?
               uk.ac.manchester.cs.owl.owlapi.OWLObjectSomeValuesFromImpl %)
             (.asConjunctSet event))]
        (is (instance?
             uk.ac.manchester.cs.owl.owlapi.OWLObjectIntersectionOfImpl event))
        (is (.containsConjunct event e/Deletion))
        (is (some #(= (count properties) %) [1 2]))
        (doseq [property properties]
          (is (.isObjectRestriction property))
          (is (= (.getProperty property) e/hasBreakPoint))
          (is (or (= (.getFiller property) (get inputs i))
                  (= (.getFiller property)
                     (get (into [] (reverse inputs)) i))))))))
  ;; invalid inputs
  (is (thrown? AssertionError
               "HumanChromosome and HumanChromosome1Band"
               (e/deletion-band h/HumanChromosome h/HumanChromosome1Band)))
  (is (thrown? AssertionError
               "HumanChromosome1Band and HumanChromosome"
               (e/deletion-band h/HumanChromosome1Band h/HumanChromosome)))
  (is (thrown? AssertionError
               "HumanChromosome and HumanChromosome"
               (e/deletion-band h/HumanChromosome h/HumanChromosome)))
)

(deftest Deletion
  ;; valid inputs - chromosomes
  ;; Is HumanAutosome necessary?
  (let [inputs [h/HumanChromosome1 h/HumanAutosome h/HumanChromosome]
        expected (into []
                       (map #(o/exactly 1 e/hasDirectEvent
                                        (o/owl-and e/Deletion %)) inputs))
        actual (into [] (map #(e/deletion 1 %) inputs))]

    (doseq [i (range (count inputs))]
      (is (= (get expected i) (get actual i)))))

  ;; valid inputs - bands
  (let [inputs [h/HumanChromosome1Bandp36.31 h/HumanChromosome1Bandp10
                h/HumanChromosome1BandpTer h/HumanChromosome1Bandp
                h/HumanChromosome1Bandq]
        ;; TOFIX - probably a problem with get-terminal function
        ;; h/HumanChromosome1Band h/HumanChromosomeBand]
        interstitial_expected
        (into []
              (map #(o/exactly 1 e/hasDirectEvent
                               (o/owl-and e/Deletion
                                          (o/owl-some e/hasBreakPoint %1 %2)))
                   inputs (reverse inputs)))
        interstitial_actual
        (into [] (map #(e/deletion 1 %1 %2) inputs (reverse inputs)))
        terminal_expected
        (into []
              (map #(o/exactly 1 e/hasDirectEvent
                               (o/owl-and e/Deletion
                                          (o/owl-some e/hasBreakPoint %
                                                      (e/get-telomere %))))
                               inputs))
        terminal_actual (into [] (map #(e/deletion 1 %) inputs))]

    ;; interstitial deletion
    (doseq [i (range (count inputs))]
      (is (= (get interstitial_expected i) (get interstitial_actual i))))
    ;; terminal deletion
    (doseq [i (range (count inputs))]
      (is (= (get terminal_expected i) (get terminal_actual i)))))

  ;; invalid input
  ;; is this true?
  (is (thrown?
       IllegalArgumentException
       "HumanChromosome1Centromere"
       (e/addition 1 h/HumanChromosome1Centromere)))
)

;; TODO
(deftest Duplication)
(deftest Direct-Duplication)
(deftest Inverse-Duplication)
(deftest Fission)

;; TODO Needs work first
(deftest Insertion)

(deftest Inversion)
(deftest Quadruplication)
(deftest Translocation)
(deftest Triplication)
(deftest Direct-Triplication)
(deftest Inverse-Triplication)