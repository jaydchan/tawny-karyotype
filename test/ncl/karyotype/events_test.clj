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
   [ncl.karyotype.karyotype :as k]
   [ncl.karyotype.generic :as g] ;; TESTING
   [tawny.render :as re] ;; TESTING
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

(deftest parentband?
  (is (#'ncl.karyotype.events/parentband? h/HumanChromosomeBand))
  (is (not (#'ncl.karyotype.events/parentband? h/HumanChromosome1Band))))

(deftest telomere-Band
  (let [chromosome h/HumanChromosome1
        test (#'ncl.karyotype.events/telomere-band chromosome)]
    (is (h/chromosome? chromosome))
    (is (instance?
         uk.ac.manchester.cs.owl.owlapi.OWLObjectIntersectionOfImpl
         test))
    (is (.containsConjunct test h/HumanChromosomeBand))
    (is (.containsConjunct test (o/owl-some k/isBandOf h/HumanTelomere)))
    (is (.containsConjunct test (o/owl-some k/isBandOf chromosome))))

  (is (thrown? AssertionError
               "HumanChromosomeBand"
               (#'ncl.karyotype.events/telomere-band h/HumanChromosomeBand))))

(deftest telomere-component
  ;; valid inputs
  (let [chromosome h/HumanChromosome1
        test (#'ncl.karyotype.events/telomere-component chromosome)]
    (is (h/chromosome? chromosome))
    (is (instance?
         uk.ac.manchester.cs.owl.owlapi.OWLObjectIntersectionOfImpl
         test))
    (is (.containsConjunct test h/HumanTelomere))
    (is (.containsConjunct test (o/owl-some k/isComponentOf chromosome))))

  ;; invalid input
  (is (thrown? AssertionError
               "HumanChromosomeBand"
               (#'ncl.karyotype.events/telomere-component
                h/HumanChromosomeBand))))

;; TODO
;; (deftest query-class)

(deftest filter-parent-axioms
  ;; valid inputs
  (let [inputs [h/HumanChromosome1Bandp h/HumanChromosome1Bandp10 e/Addition]
        property k/isBandOf
        expected [(list (o/owl-some k/isBandOf h/HumanChromosome1))
                  (list (o/owl-some k/isBandOf h/HumanChromosome1Centromere)
                        (o/owl-some k/isBandOf h/HumanChromosome1))
                  (list)]
        actual (into [] (map #(#'ncl.karyotype.events/filter-parent-axioms
                               %1 property) inputs))]

    (is (= actual expected))))

(deftest get-chromosome0
  ;; valid input
  (let [chromosome h/HumanChromosome1
        test (#'ncl.karyotype.events/get-chromosome0
              (list (o/owl-some k/isBandOf chromosome)))]

    (is (= chromosome test))

    ;; invalid input
    (is (thrown?
         IllegalArgumentException
         (str chromosome)
         (#'ncl.karyotype.events/get-chromosome0 chromosome)))))

(deftest get-chromosome
  ;; valid inputs
  (let [inputs [h/HumanChromosome1Bandp36.31 h/HumanChromosome1Bandp10
                h/HumanChromosome1BandpTer h/HumanChromosome1Bandp
                h/HumanChromosome1Bandq h/HumanChromosome1Band
                h/HumanChromosome1Centromere h/HumanChromosome1Telomere
                h/HumanChromosome1 h/HumanChromosomeBand
                h/HumanCentromere h/HumanTelomere h/HumanChromosome]
        expected (into [] (reverse (flatten (merge
                                     (repeat 9 h/HumanChromosome1)
                                     (repeat 4 h/HumanChromosome)))))
        actual (into [] (map e/get-chromosome inputs))]
    (doseq [i (range (count inputs))]
      (let [chromosome (get actual i)]
        (is (instance?
             org.semanticweb.owlapi.model.OWLClassExpression
             chromosome))
        (is (h/chromosome? chromosome))
        (is (= chromosome (get expected i))))))

  ;; invalid input
  (is (thrown?
       IllegalArgumentException
       "Addition"
       (e/get-chromosome e/Addition))))

(deftest get-centromere-string
  ;; valid inputs
  (let [inputs [h/HumanChromosome1Bandp36.31 h/HumanChromosome1Bandp10
                h/HumanChromosome1BandpTer h/HumanChromosome1Bandp
                h/HumanChromosome1Bandq h/HumanChromosome1Band
                h/HumanChromosome1Centromere h/HumanChromosome1Telomere
                h/HumanChromosome1 h/HumanChromosomeBand
                h/HumanCentromere h/HumanTelomere h/HumanChromosome
                ]
        expected (into [] (reverse
                           (flatten
                            (merge
                             (repeat 4 h/HumanChromosome1Bandp10)
                             h/HumanChromosome1Bandq10
                             (repeat 4 h/HumanChromosome1Centromere)
                             (repeat 4 h/HumanCentromere)))))
        actual (into [] (map
                         #(#'ncl.karyotype.events/get-centromere-string % nil)
                         inputs))]

    (doseq [i (range (count inputs))]
      (let [centromere (get actual i)]
        (is (instance?
             org.semanticweb.owlapi.model.OWLClassExpression
             centromere))
        (is (h/cen? centromere))
        (is centromere (get expected i)))))

  ;; invalid input
  (is (thrown?
       IllegalArgumentException
       "Addition"
       (#'ncl.karyotype.events/get-centromere-string e/Addition nil))))

(deftest get-band-no
  (let [inputs [h/HumanChromosome1Bandp36.31 h/HumanChromosome1Bandp10
                h/HumanChromosome1BandpTer h/HumanChromosome1Bandp
                h/HumanChromosome1Bandq h/HumanChromosome1Band
                h/HumanChromosomeBand]
        expected (into [] (flatten ["36.31" "10" (repeat 5 nil)]))
        actual (into [] (map #'ncl.karyotype.events/get-band-no inputs))]

    (is (= actual expected)))

  ;; invalid input
  (is (thrown?
       AssertionError
       "HumanChromosome1"
       (#'ncl.karyotype.events/get-band-no h/HumanChromosome1))))

(deftest get-direction
  ;; valid inputs
  (let [inputs [h/HumanChromosome1Bandp36.31 h/HumanChromosome1Bandp10
                h/HumanChromosome1BandpTer h/HumanChromosome1Bandp
                h/HumanChromosome1Bandq h/HumanChromosome1Band
                h/HumanChromosomeBand]
        expected (into [] (reverse
                           (flatten
                            (merge
                             (repeat 2 "Unknown")
                             "Direct"
                             "Unknown"
                             "Inverse"
                             (repeat 2 "Unknown")))))
        actual (into [] (map #(#'ncl.karyotype.events/get-direction
                               %1 %2)
                             inputs (reverse inputs)))]

    (is (= actual expected)))

  ;; invalid inputs
  (is (thrown?
       AssertionError
       (#'ncl.karyotype.events/get-direction
        h/HumanChromosome1 h/HumanChromosome1Band)))
  (is (thrown?
       AssertionError
       (#'ncl.karyotype.events/get-direction
        h/HumanChromosome1Band h/HumanChromosome1)))
  (is (thrown?
       AssertionError
       (#'ncl.karyotype.events/get-direction
        h/HumanChromosome1 h/HumanChromosome1))))

(deftest get-telomere
  ;; tests each get-telmere function
  (let [functions
        [e/get-telomere #'ncl.karyotype.events/get-telomere-string]]
         ;; #'ncl.karyotype.events/get-telomere-ontology]]
    (doseq [function functions]

      ;; valid inputs
      (let [inputs [h/HumanChromosome1Bandp36.31 h/HumanChromosome1Bandp10
                    h/HumanChromosome1BandpTer h/HumanChromosome1Bandp
                    h/HumanChromosome1Bandq h/HumanChromosome1Band
                    h/HumanChromosome1Centromere h/HumanChromosome1Telomere
                    h/HumanChromosome1 h/HumanChromosomeBand
                    h/HumanCentromere h/HumanTelomere h/HumanChromosome]
            expected (into [] (reverse
                               (flatten
                                (merge
                                 (repeat 4 h/HumanChromosome1BandpTer)
                                 h/HumanChromosome1BandqTer
                                 (repeat 4 h/HumanChromosome1Telomere)
                                 (repeat 4 h/HumanTelomere)))))
            actual (into [] (map function inputs))]

        (doseq [i (range (count inputs))]
          (let [telomere (get actual i)]
            (is (instance?
                 org.semanticweb.owlapi.model.OWLClassExpression
                 telomere))
            (is (h/ter? telomere))
            (is telomere (get expected i)))))
      ;; invalid input
      (is (thrown?
           IllegalArgumentException
           "Addition"
           (function e/Addition))))))

;; unlike exactly, owl-some can return a lazyseq of hasEvent
;; restrictions iff more than one restriction
(deftest some-event
  (let [event (#'ncl.karyotype.events/some-event
               (o/owl-and e/Addition h/HumanChromosome1 h/HumanChromosome12))]

    (is (instance? org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom event))
    (is (.isObjectRestriction event))
    (is (= (.getProperty event) e/hasEvent))
    (is (not (= (.getProperty event) e/hasDirectEvent)))))

(deftest exactly-event
  ;; valid input
  (let [n 1
        event (#'ncl.karyotype.events/exactly-event
               n (o/owl-and e/Addition h/HumanChromosome1))]
    (is (number? n))
    (is (instance?
         org.semanticweb.owlapi.model.OWLObjectExactCardinality event))
    (is (.isObjectRestriction event))
    (is (= (.getCardinality event) n))
    (is (= (.getProperty event) e/hasEvent))
    (is (not (= (.getProperty event) e/hasDirectEvent))))

  ;; invalid input
  (is (thrown? AssertionError
               (#'ncl.karyotype.events/exactly-event "1" (o/owl-and e/Addition
                                               h/HumanChromosome1)))))

(deftest event
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
  (is (thrown? AssertionError
               (e/event
                "1" (o/owl-and e/Addition h/HumanChromosome1)))))

(deftest some-direct-event
  (let [event (#'ncl.karyotype.events/some-direct-event
               (o/owl-and e/Addition h/HumanChromosome1 h/HumanChromosome12))]
    (is (instance? org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom event))
    (is (.isObjectRestriction event))
    (is (= (.getProperty event) e/hasDirectEvent))
    (is (not (= (.getProperty event) e/hasEvent)))))

(deftest exactly-direct-event
  ;; valid input
  (let [n 1
        event (#'ncl.karyotype.events/exactly-direct-event
               n (o/owl-and e/Addition h/HumanChromosome1))]
    (is (number? n))
    (is (instance? org.semanticweb.owlapi.model.OWLObjectExactCardinality
                   event))
    (is (.isObjectRestriction event))
    (is (= (.getCardinality event) n))
    (is (= (.getProperty event) e/hasDirectEvent))
    (is (not (= (.getProperty event) e/hasEvent))))
  ;; invalid input
  (is (thrown? AssertionError
               (#'ncl.karyotype.events/exactly-direct-event
                "1" (o/owl-and e/Addition h/HumanChromosome1)))))

(deftest direct-event
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
  (is (thrown? AssertionError
               (e/direct-event
                "1" (o/owl-and e/Addition h/HumanChromosome1)))))

(deftest addition-chromosome
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

(deftest addition-band
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
               (e/addition-band h/HumanChromosome))))

(deftest addition
  ;; valid inputs - chromosomes
  (let [inputs [h/HumanChromosome1 h/HumanAutosome h/HumanChromosome]
        expected (into []
                       (map #(e/direct-event
                              1 (o/owl-and e/Addition %)) inputs))
        actual (into [] (map #(e/addition 1 %) inputs))]

    (doseq [i (range (count inputs))]
      (is (= (get expected i) (get actual i)))))

  ;; valid inputs - bands
  (let [inputs [h/HumanChromosome1Bandp36.31 h/HumanChromosome1Bandp10
                h/HumanChromosome1BandpTer h/HumanChromosome1Bandp
                h/HumanChromosome1Bandq h/HumanChromosome1Band
                h/HumanChromosomeBand]
        expected (into []
                       (map #(e/direct-event
                              1 (o/owl-and e/Addition
                                           (o/owl-some e/hasBreakPoint
                                                       %))) inputs))
        actual (into [] (map #(e/addition 1 %) inputs))]

    (doseq [i (range (count inputs))]
      (is (= (get expected i) (get actual i)))))

  ;; invalid input
  (is (thrown? IllegalArgumentException
               "Addition"
               (e/addition 1 e/Addition))))

(deftest deletion-chromosome
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
               (e/deletion-chromosome h/HumanChromosomeBand))))

(deftest deletion-band
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
               (e/deletion-band h/HumanChromosome h/HumanChromosome))))

(deftest deletion
  ;; valid inputs - chromosomes
  (let [inputs [h/HumanChromosome1 h/HumanAutosome h/HumanChromosome]
        expected (into []
                       (map #(e/direct-event
                              1 (o/owl-and e/Deletion %)) inputs))
        actual (into [] (map #(e/deletion 1 %) inputs))]

    (doseq [i (range (count inputs))]
      (is (= (get expected i) (get actual i)))))

  ;; valid inputs - bands
  (let [inputs [h/HumanChromosome1Bandp36.31 h/HumanChromosome1Bandp10
                h/HumanChromosome1BandpTer h/HumanChromosome1Bandp
                h/HumanChromosome1Bandq h/HumanChromosome1Band
                h/HumanChromosomeBand]
        interstitial_expected
        (into []
              (map #(e/direct-event
                     1 (o/owl-and e/Deletion
                                  (o/owl-some e/hasBreakPoint %1 %2)))
                   inputs (reverse inputs)))
        interstitial_actual
        (into [] (map #(e/deletion 1 %1 %2) inputs (reverse inputs)))
        terminal_expected
        (into []
              (map #(e/direct-event
                     1 (o/owl-and e/Deletion
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
  (is (thrown?
       IllegalArgumentException
       "HumanChromosome1Centromere"
       (e/deletion 1 h/HumanChromosome1Centromere))))

(deftest duplication-pattern
  ;; valid inputs
  (let [type e/Duplication
        bands [h/HumanChromosome1Band h/HumanChromosome2Band]
        event (e/duplication-pattern type (get bands 0) (get bands 1))]

    (is (instance?
         uk.ac.manchester.cs.owl.owlapi.OWLObjectIntersectionOfImpl
         event))
    (is (.containsConjunct event type))
    (doseq [conjunct (.asConjunctSet event)]
      (if (instance?
           uk.ac.manchester.cs.owl.owlapi.OWLObjectSomeValuesFromImpl
           conjunct)
        (let [property conjunct]
          (is (.isObjectRestriction property))
          (is (= (.getProperty property) e/hasBreakPoint))
          (is (or (= (.getFiller property) (get bands 0))
                  (= (.getFiller property) (get bands 1)))))))

    ;; invalid input
    (is (thrown? AssertionError
                 "Addition"
                 (e/duplication-pattern e/Addition
                                        (get bands 0) (get bands 1))))))

(deftest duplication
  ;; valid inputs
  (let [inputs [h/HumanChromosome1Bandp36.31 h/HumanChromosome1Bandp10
                h/HumanChromosome1BandpTer h/HumanChromosome1Bandp
                h/HumanChromosome1Bandq h/HumanChromosome1Band
                h/HumanChromosomeBand]
        types (into [] (reverse
                        (flatten
                         (merge
                          (repeat 2 e/Duplication)
                          e/DirectDuplication
                          e/Duplication
                          e/InverseDuplication
                          (repeat 2 e/Duplication)))))
        actual (into [] (map #(e/duplication 1 %1 %2)
                             inputs (reverse inputs)))]

    (doseq [i (range (count inputs))]
      (let [event (.getFiller (get actual i))]
        (is (instance?
             uk.ac.manchester.cs.owl.owlapi.OWLObjectIntersectionOfImpl
             event))
        (is (.containsConjunct event (get types i)))
        (doseq [conjunct (.asConjunctSet event)]
          (if (instance?
               uk.ac.manchester.cs.owl.owlapi.OWLObjectSomeValuesFromImpl
               conjunct)
            (let [property conjunct]
              (is (.isObjectRestriction property))
              (is (= (.getProperty property) e/hasBreakPoint))
              (is (or (= (.getFiller property) (get inputs i))
                      (= (.getFiller property)
                         (get (into [] (reverse inputs)) i))))))))))

  ;; invalid inputs
  (is (thrown?
       AssertionError
       (e/duplication 1 h/HumanChromosome1 h/HumanChromosome1Band)))
  (is (thrown?
       AssertionError
       (e/duplication 1 h/HumanChromosome1Band h/HumanChromosome1))))

;; TOFIX
(deftest fission
  ;; valid inputs - chromosomes
  ;; (let [n 1
  ;;       inputs [h/HumanChromosome1 h/HumanAutosome h/HumanChromosome]
  ;;        actual (into [] (map #(e/fission n %) inputs))]

  ;;   (doseq [i (range (count inputs))]
  ;;     (let [events (get actual i)]

  ;;       (if (= i 0)
  ;;         (not (= (first events) (second events)))
  ;;         (= (first events) (second events)))

  ;;       (doseq [event events
  ;;               input (get inputs i)]
  ;;         (is (instance?
  ;;              org.semanticweb.owlapi.model.OWLObjectExactCardinality event))
  ;;         (is (.isObjectRestriction event))
  ;;         (is (= (.getProperty event) e/hasDirectEvent))
  ;;         (is (= (.getCardinality event) n))
  ;;         (is (.containsConjunct (.getFiller event) e/Fission))
  ;;         (or (is (.containsConjunct
  ;;                  (.getFiller event)
  ;;                  (first
  ;;                   (o/owl-some e/hasBreakPoint
  ;;                               (#'ncl.karyotype.events/get-centromere-string
  ;;                                input h/pband?)))))
  ;;             (is (.containsConjunct
  ;;                  (.getFiller event)
  ;;                  (first
  ;;                   (o/owl-some e/hasBreakPoint
  ;;                               (#'ncl.karyotype.events/get-centromere-string
  ;;                                input h/qband?))))))))))

  ;; valid inputs - bands
  (let [n 1
        inputs [h/HumanChromosome1Bandp36.31 h/HumanChromosome1Bandp10
                h/HumanChromosome1BandpTer h/HumanChromosome1Bandp
                h/HumanChromosome1Bandq h/HumanChromosome1Band
                h/HumanChromosomeBand]
        events (into [] (map #(e/fission n %) inputs))]

    (doseq [i (range (count inputs))]
      (let [event (get events i)]
        (is (instance?
             org.semanticweb.owlapi.model.OWLObjectExactCardinality event))
        (is (.isObjectRestriction event))
        (is (= (.getProperty event) e/hasDirectEvent))
        (is (= (.getCardinality event) n))
        (is (.containsConjunct (.getFiller event) e/Fission))
        (is (.containsConjunct
             (.getFiller event)
             (o/owl-some e/hasBreakPoint (get inputs i)))))))

  ;; invalid input
  ;; TODO is this true?
  (is (thrown?
       IllegalArgumentException
       "HumanChromosome1Centromere"
       (e/fission 1 h/HumanChromosome1Centromere))))

;; (deftest insertion-pattern)
;; (deftest insertion)

(deftest inversion
  ;; valid input
  (let [n 1
        inputs [h/HumanChromosome1Bandp36.31 h/HumanChromosome1Bandp10
                h/HumanChromosome1BandpTer h/HumanChromosome1Bandp
                h/HumanChromosome1Bandq h/HumanChromosome1Band
                h/HumanChromosomeBand]
        events (into [] (map #(e/inversion n %1 %2) inputs (reverse inputs)))]

    (doseq [i (range (count inputs))]
      (let [event (get events i)
            input1 (get inputs i)
            input2 (get (into [] (reverse inputs)) i)]
        (is (instance?
             org.semanticweb.owlapi.model.OWLObjectExactCardinality event))
        (is (.isObjectRestriction event))
        (is (= (.getProperty event) e/hasDirectEvent))
        (is (= (.getCardinality event) n))
        (is (.containsConjunct (.getFiller event) e/Inversion))
        (is (.containsConjunct
             (.getFiller event)
             (o/owl-some e/hasBreakPoint input1)))
        (is (.containsConjunct
             (.getFiller event)
             (o/owl-some e/hasBreakPoint input2)))
        (if (= input1 input2)
          (is (= 2 (count (.asConjunctSet (.getFiller event)))))
          (is (= 3 (count (.asConjunctSet (.getFiller event)))))))))

  ;; invalid inputs
  (is (thrown? AssertionError
               (e/inversion 1 h/HumanChromosome1Band h/HumanChromosome2Band)))
  (is (thrown? AssertionError
               (e/inversion 1 h/HumanChromosome1Band h/HumanChromosome1)))
  (is (thrown? AssertionError
               (e/inversion 1 h/HumanChromosome1 h/HumanChromosome1Band)))
  (is (thrown? AssertionError
               (e/inversion 1 h/HumanChromosome1 h/HumanChromosome1))))

;; similar to inversion
(deftest quadruplication
  ;; valid input
  (let [n 1
        inputs [h/HumanChromosome1Bandp36.31 h/HumanChromosome1Bandp10
                h/HumanChromosome1BandpTer h/HumanChromosome1Bandp
                h/HumanChromosome1Bandq h/HumanChromosome1Band
                h/HumanChromosomeBand]
        events (into [] (map #(e/quadruplication n %1 %2)
                             inputs (reverse inputs)))]

    (doseq [i (range (count inputs))]
      (let [event (get events i)
            input1 (get inputs i)
            input2 (get (into [] (reverse inputs)) i)]
        (is (instance?
             org.semanticweb.owlapi.model.OWLObjectExactCardinality event))
        (is (.isObjectRestriction event))
        (is (= (.getProperty event) e/hasDirectEvent))
        (is (= (.getCardinality event) n))
        (is (.containsConjunct (.getFiller event) e/Quadruplication))
        (is (.containsConjunct
             (.getFiller event)
             (o/owl-some e/hasBreakPoint input1)))
        (is (.containsConjunct
             (.getFiller event)
             (o/owl-some e/hasBreakPoint input2)))
        (if (= input1 input2)
          (is (= 2 (count (.asConjunctSet (.getFiller event)))))
          (is (= 3 (count (.asConjunctSet (.getFiller event)))))))))

  ;; invalid inputs
  (is (thrown? AssertionError
               (e/inversion 1 h/HumanChromosome1Band h/HumanChromosome2Band)))
  (is (thrown? AssertionError
               (e/inversion 1 h/HumanChromosome1Band h/HumanChromosome1)))
  (is (thrown? AssertionError
               (e/inversion 1 h/HumanChromosome1 h/HumanChromosome1Band)))
  (is (thrown? AssertionError
               (e/inversion 1 h/HumanChromosome1 h/HumanChromosome1))))

;; (deftest adjust-bands)
;; (deftest translocation)
;; (deftest triplication-pattern)
;; (deftest triplication)