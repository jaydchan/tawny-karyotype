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

(ns ncl.karyotype.resolutions_test
  (:use [clojure.test])
  (:require
   [ncl.karyotype.resolutions :as res]
   [ncl.karyotype.human :as h]
   [ncl.karyotype.karyotype :as k]
   [tawny.owl :as o]
   [tawny.reasoner :as r]))

(defn ontology-reasoner-fixture [tests]
  (r/reasoner-factory :hermit)
  (o/ontology-to-namespace res/resolutions)
  (binding [r/*reasoner-progress-monitor*
            (atom
            r/reasoner-progress-monitor-silent)]
    (tests)))

(use-fixtures :once ontology-reasoner-fixture)

(deftest Basic
  (is (r/consistent?))
  (is (r/coherent?)))

;; (deftest Get-lines)

(deftest Get-Band
  ;; valid inputs
  (let [inputs ["1p10" "1q42" "1q42.1" "1q42.11" "2pTer"]
        expected [h/HumanChromosome1Bandp10 h/HumanChromosome1Bandq42
                  h/HumanChromosome1Bandq42.1 h/HumanChromosome1Bandq42.11
                  h/HumanChromosome2BandpTer]
        actual (into [] (map res/get-band inputs))]

    (doseq [i (range (count inputs))]
      (let [band (get actual i)]
        (is (instance?
             org.semanticweb.owlapi.model.OWLClassExpression
             band))
        (is (h/band? band))
        (is (= band (get expected i))))))

  ;; invalid input
  (is (thrown?
       AssertionError
       "1q1"
       (res/get-band "1q1"))))

(deftest Get-Resolution
  ;; valid inputs
  (let [inputs ["300" "400" "550" "700" "850"]
        expected [res/r300-band res/r400-band res/r550-band
                  res/r700-band res/r850-band]
        actual (into [] (map res/get-resolution inputs))]

    (doseq [i (range (count inputs))]
      (let [resolution (get actual i)]
        (is (instance?
             org.semanticweb.owlapi.model.OWLClassExpression
             resolution))
        (is (o/subclass? res/resolutions res/Resolution resolution))
        (is (= resolution (get expected i))))))

  ;; invalid input
  (is (thrown?
       AssertionError
       "100"
       (res/get-resolution "100"))))

;; TODO
;; (deftest Resolution
;;   ;; valid inputs
;;   (let [band h/HumanChromosome1Bandp11.1
;;         resolutions [res/r700-band res/r850-band]]

;;     (println (count (o/superclasses h/human band)))
;;     (println (o/superclasses h/human band))
;;     (println (merge resolutions band))
;;     (println (apply res/resolution (merge resolutions band)))
;;     (println (count (o/superclasses h/human band)))
;;     (println (o/sperclasses h/human band))

;;     (o/remove-axiom h/human
;;                     '((.getOWLSubClassOfAxiom
;;                        (owl-data-factory
;;                         band
;;                         (owl-some res/seenAtResolution (first resolution))))
;;                       (.getOWLSubClassOfAxiom
;;                        (owl-data-factory
;;                         band
;;                         (owl-some res/seenAtResolution (second resolution))))))

;;     ;; (doseq [i (range (count inputs))]
;;     ;;   (let [resolution (get actual i)]
;;     ;;     (is (instance?
;;     ;;          org.semanticweb.owlapi.model.OWLClassExpression
;;     ;;          resolution))
;;     ;;     (is (o/subclass? res/resolutions res/Resolution resolution))
;;     ;;     (is (= resolution (get expected i))))))

;; ))

(deftest Resolution300-band
  (let [result (clojure.set/difference
                (r/isubclasses res/is-300-band)
                (r/isubclasses res/centromere-and-telomere))]

    ;; in an ideal world
    ;; (is (= 300 (count result)))

    ;; reality
    (is (= 307 (count result)))))

(deftest Resolution400-band
  (let [result (clojure.set/difference
                (r/isubclasses res/is-400-band)
                (r/isubclasses res/centromere-and-telomere))]

    ;; in an ideal world
    ;; (is (= 400 (count result)))

    ;; reality
    (is (= 395 (count result)))))

(deftest Resolution550-band
  (let [result (clojure.set/difference
                (r/isubclasses res/is-550-band)
                (r/isubclasses res/centromere-and-telomere))]

    ;; in an ideal world
    ;; (is (= 550 (count result)))

    ;; reality
    (is (= 566 (count result)))))

(deftest Resolution700-band
  (let [result (clojure.set/difference
                (r/isubclasses res/is-700-band)
                (r/isubclasses res/centromere-and-telomere))]

    ;; in an ideal world
    ;; (is (= 700 (count result)))

    ;; reality
    (is (= 758 (count result)))))

(deftest Resolution850-band
  (let [result (clojure.set/difference
                (r/isubclasses res/is-850-band)
                (r/isubclasses res/centromere-and-telomere))]

    ;; in an ideal world
    ;; (is (= 850 (count result)))

    ;; reality
    (is (= 868 (count result)))))