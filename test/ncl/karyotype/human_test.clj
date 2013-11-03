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

(deftest str-pband?
  (is (h/str-pband? "HumanChromosome1Bandp10"))
  (is (h/str-pband? "HumanChromosome1Bandp11"))
  (is (h/str-pband? "HumanChromosome1BandpTer"))
  (is (h/str-pband? "HumanChromosome1Bandp"))

  (is (not (h/str-pband? "HumanChromosome1Bandq10")))
  (is (not (h/str-pband? "HumanChromosome1Bandq11")))
  (is (not (h/str-pband? "HumanChromosome1BandqTer")))
  (is (not (h/str-pband? "HumanChromosome1Bandq")))
  (is (not (h/str-pband? "HumanChromosome1Band")))
  (is (not (h/str-pband? "HumanChromosomeBand")))
  (is (not (h/str-pband? "HumanChromosome"))))

(deftest str-qband?
  (is (h/str-qband? "HumanChromosome1Bandq10"))
  (is (h/str-qband? "HumanChromosome1Bandq11"))
  (is (h/str-qband? "HumanChromosome1BandqTer"))
  (is (h/str-qband? "HumanChromosome1Bandq"))

  (is (not (h/str-qband? "HumanChromosome1Bandp10")))
  (is (not (h/str-qband? "HumanChromosome1Bandp11")))
  (is (not (h/str-qband? "HumanChromosome1BandpTer")))
  (is (not (h/str-qband? "HumanChromosome1Bandp")))
  (is (not (h/str-qband? "HumanChromosome1Band")))
  (is (not (h/str-qband? "HumanChromosomeBand")))
  (is (not (h/str-qband? "HumanChromosome"))))

(deftest str-ter?
  (is (h/str-ter? "HumanChromosome1BandqTer"))
  (is (h/str-ter? "HumanChromosome1BandpTer"))

  (is (not (h/str-ter? "HumanChromosome1Bandp10")))
  (is (not (h/str-ter? "HumanChromosome1Bandp11")))
  (is (not (h/str-ter? "HumanChromosome1Bandp")))
  (is (not (h/str-ter? "HumanChromosome1Bandq10")))
  (is (not (h/str-ter? "HumanChromosome1Bandq11")))
  (is (not (h/str-ter? "HumanChromosome1Bandq")))
  (is (not (h/str-ter? "HumanChromosome1Band")))
  (is (not (h/str-ter? "HumanChromosomeBand")))
  (is (not (h/str-ter? "HumanChromosome"))))

(deftest str-cen?
  (is (h/str-cen? "HumanChromosome1Bandq10"))
  (is (h/str-cen? "HumanChromosome1Bandp10"))

  (is (not (h/str-cen? "HumanChromosome1BandpTer")))
  (is (not (h/str-cen? "HumanChromosome1Bandp11")))
  (is (not (h/str-cen? "HumanChromosome1Bandp")))
  (is (not (h/str-cen? "HumanChromosome1BandqTer")))
  (is (not (h/str-cen? "HumanChromosome1Bandq11")))
  (is (not (h/str-cen? "HumanChromosome1Bandq")))
  (is (not (h/str-cen? "HumanChromosome1Band")))
  (is (not (h/str-cen? "HumanChromosomeBand")))
  (is (not (h/str-cen? "HumanChromosome"))))

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

(deftest pband?
  (is (h/pband? h/HumanChromosome1Bandp10))
  (is (h/pband? h/HumanChromosome1Bandp11))
  (is (h/pband? h/HumanChromosome1BandpTer))
  (is (h/pband? h/HumanChromosome1Bandp))

  (is (not (h/pband? h/HumanChromosome1Bandq10)))
  (is (not (h/pband? h/HumanChromosome1Bandq11)))
  (is (not (h/pband? h/HumanChromosome1BandqTer)))
  (is (not (h/pband? h/HumanChromosome1Bandq)))
  (is (not (h/pband? h/HumanChromosome1Band)))
  (is (not (h/pband? h/HumanChromosomeBand)))
  (is (not (h/pband? h/HumanChromosome))))

(deftest qband?
  (is (h/qband? h/HumanChromosome1Bandq10))
  (is (h/qband? h/HumanChromosome1Bandq11))
  (is (h/qband? h/HumanChromosome1BandqTer))
  (is (h/qband? h/HumanChromosome1Bandq))

  (is (not (h/qband? h/HumanChromosome1Bandp10)))
  (is (not (h/qband? h/HumanChromosome1Bandp11)))
  (is (not (h/qband? h/HumanChromosome1BandpTer)))
  (is (not (h/qband? h/HumanChromosome1Bandp)))
  (is (not (h/qband? h/HumanChromosome1Band)))
  (is (not (h/qband? h/HumanChromosomeBand)))
  (is (not (h/qband? h/HumanChromosome))))

(deftest ter?
  (is (h/ter? h/HumanChromosome1BandqTer))
  (is (h/ter? h/HumanChromosome1BandpTer))

  (is (not (h/ter? h/HumanChromosome1Bandp10)))
  (is (not (h/ter? h/HumanChromosome1Bandp11)))
  (is (not (h/ter? h/HumanChromosome1Bandp)))
  (is (not (h/ter? h/HumanChromosome1Bandq10)))
  (is (not (h/ter? h/HumanChromosome1Bandq11)))
  (is (not (h/ter? h/HumanChromosome1Bandq)))
  (is (not (h/ter? h/HumanChromosome1Band)))
  (is (not (h/ter? h/HumanChromosomeBand)))
  (is (not (h/ter? h/HumanChromosome))))

(deftest cen?
  (is (h/cen? h/HumanChromosome1Bandq10))
  (is (h/cen? h/HumanChromosome1Bandp10))

  (is (not (h/cen? h/HumanChromosome1BandpTer)))
  (is (not (h/cen? h/HumanChromosome1Bandp11)))
  (is (not (h/cen? h/HumanChromosome1Bandp)))
  (is (not (h/cen? h/HumanChromosome1BandqTer)))
  (is (not (h/cen? h/HumanChromosome1Bandq11)))
  (is (not (h/cen? h/HumanChromosome1Bandq)))
  (is (not (h/cen? h/HumanChromosome1Band)))
  (is (not (h/cen? h/HumanChromosomeBand)))
  (is (not (h/cen? h/HumanChromosome))))