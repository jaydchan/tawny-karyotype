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

(ns ncl.karyotype.affects3
  (:use [tawny.owl])
  (:require [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]
            [ncl.karyotype [events :as e]]
            [ncl.karyotype [base :as b]]))


(defontology affects3
  :iri "http://ncl.ac.uk/karyotype/affects3"
  :prefix "af3:")

;; import all ncl.karyotype axioms
(owl-import h/human)

(defclass AffectsKaryotype
  :subclass k/Karyotype)

(defoproperty affects)

;; shoudl really import sequence.owl.rdf
(defoproperty precedes)
(defoproperty directlyPrecedes)
(defoproperty directlyFollows)
(defoproperty follows)

;; AUXILIARY FUNCTIONS
(defn get-band [chromosome band]
  (owl-class h/human (str "HumanChromosome" chromosome "Band" band)))

;; missing chromo 2-22,X,Y bands
(def bands-300 (map #(get-band 1 %)
                    ["pTer" "p36.3" "p36.3" "p36.2" "p36.1" "p35" "p34"
                     "p33" "p32" "p31" "p22" "p21" "p13" "p12" "p11" "p10"
                     "q10" "q11" "q12" "q21" "q22q23q24" "q25" "q31" "q32"
                     "q41" "q42" "q43q44" "qTer"]))

(defn not-breakpoint? [breakpoint band]
  (not (= breakpoint band)))

(defn subset [start finish]
  (conj
   (into []
         (take-while (partial not-breakpoint? finish)
                     (drop-while (partial not-breakpoint? start)
                                 bands-300)))
   finish))

(defn band-range [start finish]
  (if (< (.indexOf bands-300 finish) (.indexOf bands-300 start))
    (subset finish start)
    (subset start finish)))


;; PATTERNS
(defn sequence-pattern [clazzes]
  (owl-and (first clazzes)
           (owl-some directlyPrecedes (rest clazzes))))

(defn affects-band [start finish]
  (owl-some affects
            (sequence-pattern (band-range start finish))))


;; DRIVERS
(defn addition-band-driver [n band]
  (list (e/addition-band n band)
        (affects-band band band)))

(defn deletion-band-driver [n band1 band2]
  (list (e/deletion-band n band1 band2)
        (affects-band band1 band2)))

(defn addition
  "Returns an addition retriction.
n is the number of addition restrictions. chrom_band is either of type
HumanChromosome or HumanChromosomeBand." [n chrom_band]
;; In order for superclass? to work, need to use the human ontology.
(with-ontology h/human
  (cond
   ;; If chrom_band is of type HumanChromosome then restriction
   ;; represents a chromosomal gain.
   (or
    (= h/HumanChromosome chrom_band)
    (superclass? chrom_band h/HumanChromosome))
   (e/addition-chromosome 1 chrom_band)
   ;; If chrom_band is of type HumanChromosomeBand then
   ;; restriction represents a chromosomal band addition.
   (or
    (= h/HumanChromosomeBand chrom_band)
    (superclass? chrom_band h/HumanChromosomeBand))
   (addition-band-driver 1 chrom_band)
   :default
   (throw
    (IllegalArgumentException.
     (str "Addition expects a Chromosome or ChromosomeBand. Got:"
          chrom_band))))))

(defn deletion
  "Returns a deletion retriction.
n is the number of deletion restrictions.
chrom_band is either of type HumanChromosome or HumanChromosomeBand.
band, band1, band2 are of type HumanChromosomeBand."
  ([n chrom_band]
     ;; In order for superclass? to work, need to use the human ontology.
     (with-ontology
       ncl.karyotype.human/human
       (cond
        ;; If chrom_band is of type HumanChromosome then restriction
        ;; represents a chromosomal loss.
        (or
         (= h/HumanChromosome chrom_band)
         (superclass? chrom_band h/HumanChromosome))
        (e/deletion-chromosome n chrom_band)
        ;; If chrom_band is of type HumanChromosomeBand then
        ;; restriction represents a terminal band deletion with a break
        ;; (:).
        (or
         (= h/HumanChromosomeBand chrom_band)
         (superclass? chrom_band h/HumanChromosomeBand))
        (deletion-band-driver n chrom_band (e/get-telomere chrom_band))
        :default
        (throw
         (IllegalArgumentException.
          (str "Deletion expects a HumanChromosome or
               HumanChromosomeBand. Got:" chrom_band))))))
  ([n band1 band2]
     ;; This represents Interstitial band deletion with breakage and
     ;; reunion (::).  band1, band2 are of type HumanChromosomeBand.
     (deletion-band-driver n band1 band2)))


;; TESTS

;; addition
(defclass test-addition-chromosome
  :label "The 47,XX,+21 karyotype"
  :comment "ISCN2009 pg 57 -> 'A karyotype with trisomy 21.'"
  :subclass AffectsKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (addition 1 h/HumanChromosome21))

;; (defclass test-addition-band
;;   :label "The 46,XX,add(1)(p13) karyotype"
;;   :subclass AffectsKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (addition 1 h/HumanChromosome1Bandp13))

;; deletion
(defclass test-deletion-chromosome
  :label "The 47,XX,-21 karyotype"
  :subclass AffectsKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (deletion 1 h/HumanChromosome21))

;; (defclass test-deletion-band-terimal
;;   :label "The 46,XX,del(1)(p13) karyotype"
;;   :subclass AffectsKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (deletion 1 h/HumanChromosome1Bandp13))

;; (defclass test-deletion-two-bands
;;   :label "The 46,XX,del(1)(p13p11) karyotype"
;;   :subclass AffectsKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (deletion 1 h/HumanChromosome1Bandp13 h/HumanChromosome1Bandp11))

;; (defclass test-deletion-one-band
;;   :label "The 46,XX,del(1)(p13p13) karyotype"
;;   :subclass AffectsKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (deletion 1 h/HumanChromosome1Bandp13 h/HumanChromosome1Bandp13))