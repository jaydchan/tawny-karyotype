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
   [tawny.render :as ren] ;;to delete after testing complete
   [tawny.owl :as o]
   [tawny.reasoner :as r]))

(defn ontology-reasoner-fixture [tests]
  (r/reasoner-factory :hermit)
  (o/ontology-to-namespace ran/random)
  (binding [r/*reasoner-progress-monitor*
            (atom
            r/reasoner-progress-monitor-silent)]
    (tests)))

(use-fixtures :once ontology-reasoner-fixture)

;; to run: M-x 'lein' 'test'

(deftest Basic
  (is (r/consistent?))
  (is (r/coherent?)))

(deftest Random-Sex
  (let [test_sex #{b/k46_XX b/k46_XY}
        random (ran/random-sex)]
    ;; check sex vector
    (is (every? test_sex ran/sex))

    ;; testing random-sex function
    (is (instance? org.semanticweb.owlapi.model.OWLClassExpression random))
    ;; TODO equivalency for owl classes
    (is (some #(= (o/iri-for-name random)
                  (o/iri-for-name %)) test_sex))
    (is (o/superclass? b/base random b/k46_XN))))

(deftest Karyotype-Class
  (let [size (.size (.getClassesInSignature ran/random))]

    ;; classes added are r1, Deletion, pTer, p36.3 and 46,XX (or 46,XY)
    (is
     (= (+ size 5)
        (o/with-probe-entities ran/random
          [clazz (ran/karyotype-class
                  ran/random
                  1 (e/deletion 1 h/HumanChromosome1Bandp36.3))]
          (-> ran/random
              (.getClassesInSignature)
              (.size)))))
    (is
     (instance? org.semanticweb.owlapi.model.OWLClassExpression
                (o/with-probe-entities ran/random
                  [clazz (ran/karyotype-class
                          ran/random
                          1 (e/deletion 1 h/HumanChromosome1Bandp36.3))]
                  (-> clazz))))
    (is (o/with-probe-entities ran/random
          [clazz (ran/karyotype-class
                  ran/random
                  1 (e/deletion 1 h/HumanChromosome1Bandp36.3))]
          (-> (o/superclass? ran/random "r1" ran/RandomKaryotype))))))

(deftest Get-Band
  (let [band (ran/get-band 1 "p36.3")]
    (is (instance? org.semanticweb.owlapi.model.OWLClassExpression band))
    (is (= (o/iri-for-name h/HumanChromosome1Bandp36.3)
           (o/iri-for-name band)))))

(deftest Random-Band
  (let [random (ran/random-band)]
    (is (instance? org.semanticweb.owlapi.model.OWLClassExpression random))
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
    (is (instance? org.semanticweb.owlapi.model.OWLClassExpression random))
    (is (some #(= (o/iri-for-name random)
                  (o/iri-for-name %)) test_chrom))
    (is (r/isuperclass? h/human random h/HumanChromosome))))

(deftest Random-Terminal-Deletion
  (let [random (ran/random-terminal-deletion)
        strings (re-seq #"HumanChromosome[\dXY]+Band[pqTer\d.]+" (str random))
        bands (map #(o/owl-class h/human %) strings)]

    (is (instance? org.semanticweb.owlapi.model.OWLObjectExactCardinality
                   random))
    (is (= (count strings) 2))
    (is (re-find #"Deletion" (str random)))
    ;; assuming there are only two bands, one should be a telomere
    (is (distinct? (map #(r/isuperclass? h/human % h/is-telomere) bands)))
    (is (= random (o/exactly 1 e/hasDirectEvent
                             (o/owl-and e/Deletion
                                        (o/owl-some e/hasBreakPoint
                                                    (first bands)
                                                    (second bands))))))
    (doseq [band bands]
      (is (r/isuperclass? h/human band h/HumanChromosomeBand)))))

(deftest Random-Interstitial-Deletion
  (let [random (ran/random-interstitial-deletion)
        strings (re-seq #"HumanChromosome[\dXY]+Band[pqTer\d.]+" (str random))
        bands (map #(o/owl-class h/human %) strings)]

    (is (instance? org.semanticweb.owlapi.model.OWLObjectExactCardinality
                   random))
    ;; del(q15q15) is ontologically (some hasBreakPoint q15) and not
    ;; (some hasBreakPoint q15 q15) therefore (count strings) can be
    ;; 1 OR 2.
    (is (re-find #"Deletion" (str random)))
    (is (some #(= (count strings) %) [1 2]))
    ;; there should be not telomere bands
    ;; if 2 strings were identified
    (if (= 2 (count strings))
      (is (= random (o/exactly 1 e/hasDirectEvent
                               (o/owl-and e/Deletion
                                          (o/owl-some e/hasBreakPoint
                                                      (first bands)
                                                      (second bands))))))
      (is (= random (o/exactly 1 e/hasDirectEvent
                               (o/owl-and e/Deletion
                                          (o/owl-some e/hasBreakPoint
                                                      (first bands)))))))
    (doseq [band bands]
      (is (r/isuperclass? h/human band h/HumanChromosomeBand))
      (is (not (r/isuperclass? h/human band h/HumanTelomere))))))

(deftest Random-Band-Deletion
  (let [random (ran/random-band-deletion)
        strings (re-seq #"HumanChromosome[\dXY]+Band[pqTer\d.]+" (str random))
        bands (map #(o/owl-class h/human %) strings)]

    (is (instance? org.semanticweb.owlapi.model.OWLObjectExactCardinality
                   random))
    (is (some #(= (count strings) %) [1 2]))
    (is (re-find #"Deletion" (str random)))
    (if (= 2 (count strings))
      (is (= random (o/exactly 1 e/hasDirectEvent
                               (o/owl-and e/Deletion
                                          (o/owl-some e/hasBreakPoint
                                                      (first bands)
                                                      (second bands))))))
      (is (= random (o/exactly 1 e/hasDirectEvent
                               (o/owl-and e/Deletion
                                          (o/owl-some e/hasBreakPoint
                                                      (first bands)))))))
    (doseq [band bands]
      (is (r/isuperclass? h/human band h/HumanChromosomeBand)))))

(deftest Random-Chromosome-Deletion
  (let [random (ran/random-chromosome-deletion)
        string (re-find #"HumanChromosome[\dXY]+" (str random))
        chrom (o/owl-class h/human string)]

    (is (instance? org.semanticweb.owlapi.model.OWLObjectExactCardinality
                   random))
    (is (r/isuperclass? h/human chrom h/HumanChromosome))
    (is (re-find #"Deletion" (str random)))
    (is (= random (o/exactly 1 e/hasDirectEvent
                             (o/owl-and e/Deletion chrom))))))

(deftest Random-Band-Addition
  (let [random (ran/random-band-addition)
        string (re-find #"HumanChromosome[\dXY]+Band[pq\d.]+" (str random))
        band (o/owl-class h/human string)]

    (is (instance? org.semanticweb.owlapi.model.OWLObjectExactCardinality
                   random))
    (is (r/isuperclass? h/human band h/HumanChromosomeBand))
    (is (re-find #"Addition" (str random)))
    (is (= random (o/exactly 1 e/hasDirectEvent
                             (o/owl-and e/Addition
                                        (o/owl-some e/hasBreakPoint
                                                    band)))))))

(deftest Random-Chromosome-Addition
  (let [random (ran/random-chromosome-addition)
        string (re-find #"HumanChromosome[\dXY]+" (str random))
        chrom (o/owl-class h/human string)]

    (is (instance? org.semanticweb.owlapi.model.OWLObjectExactCardinality
                   random))
    (is (r/isuperclass? h/human chrom h/HumanChromosome))
    (is (re-find #"Addition" (str random)))
    (is (= random (o/exactly 1 e/hasDirectEvent
                             (o/owl-and e/Addition chrom))))))

(deftest Random-Abnormality
  (let [random (ran/random-abnormality)]

    (is (instance? org.semanticweb.owlapi.model.OWLObjectExactCardinality
                   random))
    (is (re-find #"Addition|Deletion" (str random)))))

(deftest Random-Abnormality-Driver
  (let [n 3
        randoms (ran/random-abnormality-driver n)]

    (is (= (count randoms) n))
    (doseq [random randoms]
      (is (re-find #"Addition|Deletion" (str random)))
      (is (instance? org.semanticweb.owlapi.model.OWLObjectExactCardinality
                     random)))))

(deftest Refine-Label
  (let [clazz (ran/karyotype-class
               ran/random 1 (e/addition 1 h/HumanChromosome1Bandp36.3))
        axioms (.getAnnotations clazz ran/random)
        sex (re-find #"XX|XY" (str (ren/as-form clazz)))
        parse (str "46," sex ",add(1p36.3)")
        string (str "The " parse " Karyotype")]

    ;; (println clazz)
    ;; (println (ren/as-form clazz))
    ;; (ran/refine-label clazz)
    ;; (println (ren/as-form clazz))
    ;; (println sex)

    (o/remove-entity ran/random clazz)))

;; TODO - Check number of axioms for clazz <= 3
;; (deftest Random-Karyotype
;;   (let [random (ran/random-karyotype 2 3)]
;;     (is (instance? org.semanticweb.owlapi.model.OWLClassExpression random))
;;     (is (o/superclass? ran/random "r2" ran/RandomKaryotype)))
;; )

;; TODO - Check the number of karyotypes added to ontology
;; (deftest Random-Karyotype-Driver)
