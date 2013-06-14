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

(ns ncl.karyotype.human_test
  (:use [clojure.test])
  (:require
   [ncl.karyotype.human :as h]
   [tawny.owl :as o]
   [tawny.reasoner :as r]))

(defn ontology-reasoner-fixture [tests]
  (r/reasoner-factory :hermit)
  (o/ontology-to-namespace h/human)
  (binding [r/*reasoner-progress-monitor*
            (atom
            r/reasoner-progress-monitor-silent)]
    (tests)))

(use-fixtures :once ontology-reasoner-fixture)

(deftest Basic
  (is (r/consistent?))
  (is (r/coherent?)))

(deftest pBand?
  (is (h/pband? "HumanChromosome1Bandp10"))
  (is (h/pband? "HumanChromosome1Bandp11"))
  (is (h/pband? "HumanChromosome1BandpTer"))
  (is (h/pband? "HumanChromosome1Bandp"))

  (is (not (h/pband? "HumanChromosome1Bandq10")))
  (is (not (h/pband? "HumanChromosome1Bandq11")))
  (is (not (h/pband? "HumanChromosome1BandqTer")))
  (is (not (h/pband? "HumanChromosome1Bandq")))
  (is (not (h/pband? "HumanChromosome1Band")))
  (is (not (h/pband? "HumanChromosomeBand")))
  (is (not (h/pband? "HumanChromosome"))))

(deftest qBand?
  (is (h/qband? "HumanChromosome1Bandq10"))
  (is (h/qband? "HumanChromosome1Bandq11"))
  (is (h/qband? "HumanChromosome1BandqTer"))
  (is (h/qband? "HumanChromosome1Bandq"))

  (is (not (h/qband? "HumanChromosome1Bandp10")))
  (is (not (h/qband? "HumanChromosome1Bandp11")))
  (is (not (h/qband? "HumanChromosome1BandpTer")))
  (is (not (h/qband? "HumanChromosome1Bandp")))
  (is (not (h/qband? "HumanChromosome1Band")))
  (is (not (h/qband? "HumanChromosomeBand")))
  (is (not (h/qband? "HumanChromosome"))))

(deftest ter?
  (is (h/ter? "HumanChromosome1BandqTer"))
  (is (h/ter? "HumanChromosome1BandpTer"))

  (is (not (h/ter? "HumanChromosome1Bandp10")))
  (is (not (h/ter? "HumanChromosome1Bandp11")))
  (is (not (h/ter? "HumanChromosome1Bandp")))
  (is (not (h/ter? "HumanChromosome1Bandq10")))
  (is (not (h/ter? "HumanChromosome1Bandq11")))
  (is (not (h/ter? "HumanChromosome1Bandq")))
  (is (not (h/ter? "HumanChromosome1Band")))
  (is (not (h/ter? "HumanChromosomeBand")))
  (is (not (h/ter? "HumanChromosome"))))

(deftest cen?
  (is (h/cen? "HumanChromosome1Bandq10"))
  (is (h/cen? "HumanChromosome1Bandp10"))

  (is (not (h/cen? "HumanChromosome1BandpTer")))
  (is (not (h/cen? "HumanChromosome1Bandp11")))
  (is (not (h/cen? "HumanChromosome1Bandp")))
  (is (not (h/cen? "HumanChromosome1BandqTer")))
  (is (not (h/cen? "HumanChromosome1Bandq11")))
  (is (not (h/cen? "HumanChromosome1Bandq")))
  (is (not (h/cen? "HumanChromosome1Band")))
  (is (not (h/cen? "HumanChromosomeBand")))
  (is (not (h/cen? "HumanChromosome"))))

(deftest group-for-band
  (is (str "HumanChromosome1Bandq")
      (h/group-for-band "HumanChromosome1Band" "qTer"))
  (is (str "HumanChromosome1Bandq")
      (h/group-for-band "HumanChromosome1Band" "q11"))
  (is (str "HumanChromosome1Bandq")
      (h/group-for-band "HumanChromosome1Band" "q10"))
  (is (str "HumanChromosome1Bandp")
      (h/group-for-band "HumanChromosome1Band" "pTer"))
  (is (str "HumanChromosome1Bandp")
      (h/group-for-band "HumanChromosome1Band" "p11"))
  (is (str "HumanChromosome1Bandp")
      (h/group-for-band "HumanChromosome1Band" "p10")))

;; ;; DO NOT REPLACE THE original ter? function! - Needed for string
;; ;; compilation.
;; (deftest ter?-new
;;   (println (superclasses h/HumanChromosome1BandqTer))
;;   (is (h/ter?-new h/HumanChromosome1BandqTer))
;;   (is (h/ter?-new h/HumanChromosome1BandpTer))

;;   (is (not (h/ter?-new h/HumanChromosome1Bandp10)))
;;   (is (not (h/ter?-new h/HumanChromosome1Bandp11)))
;;   (is (not (h/ter?-new h/HumanChromosome1Bandp)))
;;   (is (not (h/ter?-new h/HumanChromosome1Bandq10)))
;;   (is (not (h/ter?-new h/HumanChromosome1Bandq11)))
;;   (is (not (h/ter?-new h/HumanChromosome1Bandq)))
;;   (is (not (h/ter?-new h/HumanChromosome1Band)))
;;   (is (not (h/ter?-new h/HumanChromosomeBand)))
;;   (is (not (h/ter?-new h/HumanChromosome))))

;; ;; DO NOT REPLACE THE original cen? function! - Needed for string
;; ;; compilation.
;; (deftest cen?-new
;;   (println (superclasses h/HumanChromosome1BandqTer))
;;   (is (h/cen?-new h/HumanChromosome1Bandq10))
;;   (is (h/cen?-new h/HumanChromosome1Bandp10))

;;   (is (not (h/cen?-new h/HumanChromosome1BandpTer)))
;;   (is (not (h/cen?-new h/HumanChromosome1Bandp11)))
;;   (is (not (h/cen?-new h/HumanChromosome1Bandp)))
;;   (is (not (h/cen?-new h/HumanChromosome1BandqTer)))
;;   (is (not (h/cen?-new h/HumanChromosome1Bandq11)))
;;   (is (not (h/cen?-new h/HumanChromosome1Bandq)))
;;   (is (not (h/cen?-new h/HumanChromosome1Band)))
;;   (is (not (h/cen?-new h/HumanChromosomeBand)))
;;   (is (not (h/cen?-new h/HumanChromosome))))
