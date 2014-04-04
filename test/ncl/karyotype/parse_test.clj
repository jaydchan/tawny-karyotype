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

(ns ncl.karyotype.parse_test
  (:use [clojure.test]
        (incanter core io excel))
  (:require
   [ncl.karyotype.events :as e]
   [ncl.karyotype.human :as h]
   [ncl.karyotype.parse :as p]
   [tawny.owl :as o]
   [tawny.reasoner :as r]
   [clojure.java.io :as io]))

(defn ontology-reasoner-fixture [tests]
  (r/reasoner-factory :hermit)
  (o/ontology-to-namespace p/parse)
  (binding [r/*reasoner-progress-monitor*
            (atom
            r/reasoner-progress-monitor-silent)]
    (tests)))

(use-fixtures :once ontology-reasoner-fixture)

;; to run: M-x 'lein' 'test'

(deftest Basic
  (is (r/consistent?))
  (is (r/coherent?)))

;; TODO Unknown chromosomes and bands

;; (deftest Clean-Up)
;; (deftest Get-Start)

(deftest Addition-Chromosome
  ;; valid inputs
  (let [inputs [h/HumanChromosome1];; h/HumanAutosome h/HumanChromosome]
        expected [["1" "+1"]];; ["?" "+?"] ["?" "+?"]]
        actual (into [] (map #'ncl.karyotype.parse/addition-chromosome inputs))]

    (doseq [i (range (count inputs))]
      (let [string (get actual i)]
        (is (every? string? string))
        (is string (get expected i)))))

  ;; invalid input
  (is (thrown? AssertionError
               "HumanChromosomeBand"
               (#'ncl.karyotype.parse/addition-chromosome
                h/HumanChromosomeBand))))

(deftest Addition-Band
  ;; valid inputs
  (let [inputs [h/HumanChromosome1Bandp36.31 h/HumanChromosome1Bandp10
                h/HumanChromosome1BandpTer] ;; h/HumanChromosome1Bandp
                ;; h/HumanChromosome1Bandq h/HumanChromosome1Band
                ;; h/HumanChromosomeBand]
        expected [["1" "add(1)(p36.31)"] ["1" "add(1)(p10)"]
                  ["1" "add(1)(pTer)"]]
                  ;; ["1" "add(1)(p?)"] ["1" "add(1)(q?)"] ["1" "add(1)(?)"]
                  ;; ["?" "add(?)(?)"]]
        actual (into [] (map #'ncl.karyotype.parse/addition-band inputs))]

    (doseq [i (range (count inputs))]
      (let [string (get actual i)]
        (is (every? string? string))
        (is string (get expected i)))))

  ;; invalid input
  (is (thrown? AssertionError
               "HumanChromosomeBand"
               (#'ncl.karyotype.parse/addition-band h/HumanChromosome))))

(deftest Chrom-Filter
  ;; valid inputs
  (let [inputs [(o/owl-and e/Addition h/HumanChromosome1)
                (o/owl-and
                 e/Addition
                 (o/owl-some e/hasBreakPoint h/HumanChromosome1Bandp10))]
        actual (into [] (map #'ncl.karyotype.parse/chrom-filter inputs))]

     (is (h/chromosome? (first (get actual 0))))
     (is (= h/HumanChromosome1 (first (get actual 0))))

     (is (nil? (first (get actual 1))))))

(deftest Band-Filter
  ;; valid inputs
  (let [inputs [(o/owl-and e/Addition h/HumanChromosome1)
                (o/owl-and
                 e/Addition
                 (o/owl-some e/hasBreakPoint h/HumanChromosome1Bandp10))
                (o/owl-and
                 e/Deletion
                 (o/owl-some e/hasBreakPoint
                             h/HumanChromosome1Bandp10
                             h/HumanChromosome1BandpTer))
                (o/owl-and
                 e/Deletion
                 (o/owl-some e/hasBreakPoint
                             h/HumanChromosome1Bandp10
                             h/HumanChromosome1Bandq10))]
        actual (into [] (map #'ncl.karyotype.parse/band-filter inputs))]

    (is (nil? (first (get actual 0))))

    (doseq [i (range 1 (count inputs))]
      (let [output (get actual i)]
        (doseq [band output]
          (is (h/band? band)))))

    (is (= 1 (count (get actual 1))))
    (is (= 2 (count (get actual 2))))
    (is (= 2 (count (get actual 3))))

    (is (= h/HumanChromosome1Bandp10 (first (get actual 1))))
    (let [expected [h/HumanChromosome1Bandp10 h/HumanChromosome1BandpTer]]
          (is (or
               (= (get actual 2) expected)
               (= (get actual 2) (into [] (reverse expected))))))
    (let [expected [h/HumanChromosome1Bandp10 h/HumanChromosome1Bandq10]]
          (is (or
               (= (get actual 3) expected)
               (= (get actual 3) (into [] (reverse expected))))))))

;; TODO
(deftest Human-Filter
  ;; valid inputs
  (let [inputs [(o/owl-and e/Addition h/HumanChromosome1)
                (o/owl-and
                 e/Addition
                 (o/owl-some e/hasBreakPoint h/HumanChromosome1Bandp10))]
        actual (into [] (map #'ncl.karyotype.parse/human-filter inputs))]))

;; (deftest Addition)

(deftest Deletion-Chromosome
  ;; valid inputs
  (let [inputs [h/HumanChromosome1];; h/HumanAutosome h/HumanChromosome]
        expected [["1" "-1"]];; ["?" "-?"] ["?" "-?"]]
        actual (into [] (map #'ncl.karyotype.parse/deletion-chromosome inputs))]

        (doseq [i (range (count inputs))]
          (let [string (get actual i)]
            (is (every? string? string))
            (is string (get expected i)))))

  ;; invalid input
  (is (thrown? AssertionError
               "HumanChromosomeBand"
               (#'ncl.karyotype.parse/deletion-chromosome
                h/HumanChromosomeBand))))

;; (deftest Deletion-Band)
;; (deftest Deletion-Band-Driver)
;; (deftest Deletion)
;; (deftest Get-Event-String)
;; (deftest Get-Axiom_string)
;; (deftest Chrom-Sort)
;; (deftest Parse-Karyotype-Class)
;; (deftest Create-Karyotype-String0)

(deftest Roundtrip
  ;; Read data from .xlsx file
  (with-data (read-xls
              (.getFile (io/resource "iscnexamples_test.xlsx")))

    (let [parse? (into [] ($ :Parse)) ;; get parse (values)
          labels (into [] ($ :Label)) ;; get label (values)
          filtered ;; filter for 'valid' labels
          (filter #(= (get parse? (.indexOf labels %)) 1.0) labels)
          clazzes (map p/parse-karyotype-string filtered) ;; create classes
          ;; strings ;; create labels
          ;; (map #(p/parse-karyotype-class p/parse %) clazzes)
          ]

      ;; errors parsing str->clazz
      (doseq [f filtered]
        (try (p/parse-karyotype-string f)
             (catch Exception e (println (str "Error: str->clazz " f)))))

      ;; ;; errors parsing clazz->str
      ;; (doseq [f filtered]
      ;;   (try (p/parse-karyotype-class p/parse (p/parse-karyotype-string f))
      ;;        (catch Exception e (println (str "Error: clazz->str " f)))))

      ;; ;; make sure they match
      ;; (doseq [string strings]
      ;;   (println (str string " " (get filtered (.indexOf strings string))))
      ;;   (is (= string (get filtered (.indexOf strings string)))))
)))