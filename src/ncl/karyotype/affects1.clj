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

(ns ncl.karyotype.affects1
  (:use [tawny.owl])
  (:require [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]
            [ncl.karyotype [events :as e]]
            [ncl.karyotype [base :as b]]))


(defontology affects1
  :iri "http://ncl.ac.uk/karyotype/affects1"
  :prefix "af1:")

(defclass AffectsKaryotype
  :subclass k/Karyotype)

;; import all ncl.karyotype axioms
(defoproperty affects)

;; missing chromo 2-22,X,Y bands
(def bands-300 [1
  ["pTer" "p36.3" "p36.3" "p36.2" "p36.1" "p35" "p34" "p33" "p32" "p31" "p22"
   "p21" "p13" "p12" "p11" "p10" "q10" "q11" "q12" "q21" "q22q23q24" "q25"
   "q31" "q32" "q41" "q42" "q43q44" "qTer"]])

(defn get-band [chromosome band]
  (owl-class h/human (str "HumanChromosome" chromosome "Band" band)))

(defn subset [start finish]
    (take-while #(not (= finish %)) (drop-while #(not (= start %)))))

(defn band-range [start finish]
  (get-band
   (if (nil? finish)
     (subset start (e/get-terminal start))
     (subset start finish))

(defn affect [start finish]
  (some-only affects (band-range start finish)))

(defn addition-chromosome [chromosome]
  (exactly n hasEvent
           (owl-and Addition chromosome)))

(defn addition-band [band]
  (exactly n hasEvent
           (owl-and Addition
                    (owl-some hasBreakPoint band))))

(defn addition-band-driver [band]
  (list (addition-band band)
        (affects band (e/get-telomere band))))

(defn addition
  "Returns an addition retriction.
n is the number of addition restrictions. chrom_band is either of type
HumanChromosome or HumanChromosomeBand." [chrom_band]
;; In order for superclass? to work, need to use the human ontology.
(with-ontology h/human
  (cond
   ;; If chrom_band is of type HumanChromosome then restriction
   ;; represents a chromosomal gain.
   (or
    (= h/HumanChromosome chrom_band)
    (superclass? chrom_band h/HumanChromosome))
   (addition-chromosome chrom_band)
   ;; If chrom_band is of type HumanChromosomeBand then
   ;; restriction represents a chromosomal band addition.
   (or
    (= h/HumanChromosomeBand chrom_band)
    (superclass? chrom_band h/HumanChromosomeBand))
   (addition-band chrom_band)
   :default
   (throw
    (IllegalArgumentException.
     (str "Addition expects a Chromosome or ChromosomeBand. Got:"
          chrom_band))))))

;; tests
(defclass test-addition-chromosome
  :label "The 47,XX,+21 karyotype"
  :comment "ISCN2009 pg 57 -> 'A karyotype with trisomy 21.'"
  :subclass AffectsKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (addition 1 h/HumanChromosome21))

;; (defclass test-addition-chromosome-exactly
;;   :label "The 47,XX,+21 karyotype"
;;   :comment "ISCN2009 pg 57 -> 'A karyotype with trisomy 21.'"
;;   :subclass AffectsKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (addition [exactly 1] h/HumanChromosome21))

;; (defclass test-addition-chromosome-some
;;   :label "The 47,XX,+21 karyotype"
;;   :comment "ISCN2009 pg 57 -> 'A karyotype with trisomy 21.'"
;;   :subclass AffectsKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (addition [some] h/HumanChromosome21))

(defclass test-addition-band
  :label "The 46,XX,add(19)(p13.3) karyotype"
  :comment "ISCN2009 pg 60 -> 'Additional material attached to band
  19p13.3, but neither the origin of the extra segment nor the type of
  arrangement is known.' 46,XX,add(19)(?::p13.3->qter)"
  :subclass AffectsKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (addition 1 h/HumanChromosome19Bandp13.3))