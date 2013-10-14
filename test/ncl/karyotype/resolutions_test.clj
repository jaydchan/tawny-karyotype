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

;; to run: M-x 'lein' 'test'

(deftest Basic
  (is (r/consistent?))
  (is (r/coherent?)))

(deftest Resolution300-band
  (o/defclass t300-band
    :equivalent
    (o/owl-and h/HumanChromosomeBand
               (o/owl-some res/seenAtResolution res/r300-band)))

  (o/defclass band-ignore
    :equivalent
    (o/owl-and h/HumanChromosomeBand
               (o/owl-some res/seenAtResolution res/Resolution)
               (o/owl-or
                (o/owl-some k/isBandOf h/HumanCentromere)
                (o/owl-some k/isBandOf h/HumanTelomere))))

  (let [result (clojure.set/difference
                (r/isubclasses t300-band)
                (r/isubclasses band-ignore))]
    (is (= 300 (count result)))))

(deftest Resolution400-band
  (o/defclass t400-band
    :equivalent
    (o/owl-and h/HumanChromosomeBand
    (o/owl-some res/seenAtResolution res/r400-band)))

  (o/defclass band-ignore
    :equivalent
    (o/owl-and h/HumanChromosomeBand
               (o/owl-some res/seenAtResolution res/Resolution)
               (o/owl-or
                (o/owl-some k/isBandOf h/HumanCentromere)
                (o/owl-some k/isBandOf h/HumanTelomere))))

  (let [result (clojure.set/difference
                (r/isubclasses t400-band)
                (r/isubclasses band-ignore))]
    (is (= 400 (count result)))))

(deftest Resolution550-band
  (o/defclass t550-band
    :equivalent
    (o/owl-and h/HumanChromosomeBand
    (o/owl-some res/seenAtResolution res/r550-band)))

  (o/defclass band-ignore
    :equivalent
    (o/owl-and h/HumanChromosomeBand
               (o/owl-some res/seenAtResolution res/Resolution)
               (o/owl-or
                (o/owl-some k/isBandOf h/HumanCentromere)
                (o/owl-some k/isBandOf h/HumanTelomere))))

  (let [result (clojure.set/difference
                (r/isubclasses t550-band)
                (r/isubclasses band-ignore))]
    (is (= 550 (count result)))))

(deftest Resolution700-band
  (o/defclass t700-band
    :equivalent
    (o/owl-and h/HumanChromosomeBand
    (o/owl-some res/seenAtResolution res/r700-band)))

  (o/defclass band-ignore
    :equivalent
    (o/owl-and h/HumanChromosomeBand
               (o/owl-some res/seenAtResolution res/Resolution)
               (o/owl-or
                (o/owl-some k/isBandOf h/HumanCentromere)
                (o/owl-some k/isBandOf h/HumanTelomere))))

  (let [result (clojure.set/difference
                (r/isubclasses t700-band)
                (r/isubclasses band-ignore))]
    (is (= 700 (count result)))))

(deftest Resolution850-band
  (o/defclass t850-band
    :equivalent
    (o/owl-and h/HumanChromosomeBand
    (o/owl-some res/seenAtResolution res/r850-band)))

  (o/defclass band-ignore
    :equivalent
    (o/owl-and h/HumanChromosomeBand
               (o/owl-some res/seenAtResolution res/Resolution)
               (o/owl-or
                (o/owl-some k/isBandOf h/HumanCentromere)
                (o/owl-some k/isBandOf h/HumanTelomere))))

  (let [result (clojure.set/difference
                (r/isubclasses t850-band)
                (r/isubclasses band-ignore))]
    (is (= 850 (count result)))))