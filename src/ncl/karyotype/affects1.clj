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

(ns ^{:doc "Redefining chromosomal band addition and deletion event
definitions to include affects object property."
      :author "Jennifer Warrender"}
  ncl.karyotype.affects1
  (:use [tawny.owl])
  (:require [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]
            [ncl.karyotype [events :as e]]
            [ncl.karyotype [parse :as p]]
            ;; Testing purposes only
            [ncl.karyotype [base :as b]]))

(defontology affects1
  :iri "http://ncl.ac.uk/karyotype/affects1"
  :prefix "af1:"
  :comment "Affects (take 1) ontology for Human Karyotype Ontology,
  written using the tawny-owl library.")

(defoproperty affects
  :domain k/Karyotype
  :range h/HumanChromosomeBand)

;; PATTERN
(defn affects-band
  "Pattern - returns some-only axiom for BANDS, using affects
oproperty."
  [bands] {:pre [(every? h/band? bands)]}
  (apply some-only affects bands))

;; AUXILIARY FUNCTIONS
(defn get-band
  "Returns a 300-band chromosomal band class."
  [chromosome band] {:post [(h/band? %)]}
  (owl-class h/human (str "HumanChromosome" chromosome "Band" band)))
;; missing chromo 2-22,X,Y bands
(def ^{:doc "An ordered array of available 300-band resolution band
  information for chromosome 1 only."}
  bands-300 (map #(get-band 1 %)
  ["pTer" "p36.3" "p36.2" "p36.1" "p35" "p34" "p33" "p32" "p31" "p22" "p21" "p13"
   "p12" "p11" "q11" "q12" "q21" "q22q23q24" "q25" "q31" "q32" "q41" "q42"
   "q43q44" "qTer"]))

(defn not-breakpoint?
  "Determines if BAND is equal to BREAKPOINT."
  [breakpoint band] {:pre [(h/band? breakpoint) (h/band? band)]}
  (not (= breakpoint band)))

(defn subset
  "Returns a range of 300 resolution bands for given START and FINISH
bands."
  [start finish]
  (conj (into [] (take-while
                  (partial not-breakpoint? finish)
                  (drop-while (partial not-breakpoint? start)
                              bands-300)))
        finish))

(defn band-range
  "Returns a range of 300 resolution bands for given START and FINISH
bands. Swaps START and FINISH bands if START band index is greater
than FINISH band, determined by bands-300 vector."
  [start finish]
  (if (> (.indexOf bands-300 start) (.indexOf bands-300 finish))
    (subset finish start)
    (subset start finish)))

(defn- get-band-range
  "Returns either one band or a range of bands, determined by BANDS."
  [bands]
  (cond
   (= (count bands) 1)
   (first bands)
   (= (count bands) 2)
   (band-range (first bands) (second bands))
   :default
   (throw
    (IllegalArgumentException.
     (str "Get-band-range expects one or two bands. Got:" bands)))))

(defn- get-breakpoints
  "Returns a list of breakpoint bands for a given CLAZZ in ontology
O."
  [o clazz]
  (let [parents (direct-superclasses o clazz)
        restrictions (filter #(instance?
                               org.semanticweb.owlapi.model.OWLRestriction %)
                             parents)
        events (filter #(= (.getProperty %) e/hasDirectEvent) restrictions)
        axioms (into [] (map #(.getFiller %) events))
        chrom_band (map p/human-filter axioms)
        bands (into [] (filter #(h/band? (first %)) chrom_band))]
    bands))

(defn get-bands
  "Returns a list of affected bands for a given CLAZZ in ontology O."
  [o clazz]
  (for [band (get-breakpoints o clazz)]
    (get-band-range band)))

;; PATTERN
(defn- affects-band
  "Pattern - returns some-only axiom for BANDS, using affects
object property."
  [bands] {:pre [(every? h/band? bands)]}
  (apply some-only affects bands))

;; DRIVERS
(defn affects1-driver
  "Returns the updated class definition of CLAZZ in ontology O."
  [o clazz]
  (let [bands (flatten (get-bands o clazz))]
    (if (= (count bands) 0)
      clazz
      (refine clazz
              :ontology o
              :subclass (affects-band bands)))))

;; ;; TESTS
;; ;; import human ontology axioms
;; (owl-import h/human)

;; (defclass AffectsKaryotype
;;   :subclass k/Karyotype)

;; ;; addition
;; (defclass test-addition-chromosome
;;   :label "The 47,XX,+21 karyotype"
;;   :subclass AffectsKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/addition 1 h/HumanChromosome21))

;; (defclass test-addition-band
;;   :label "The 46,XX,add(1)(p13) karyotype"
;;   :subclass AffectsKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/addition 1 h/HumanChromosome1Bandp13))

;; (defclass test-addition-both
;;   :label "The 47,XX,add(1)(p13),+21 karyotype"
;;   :subclass AffectsKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/addition 1 h/HumanChromosome1Bandp13)
;;   (e/addition 1 h/HumanChromosome21))

;; ;; deletion
;; (defclass test-deletion-chromosome
;;   :label "The 47,XX,-21 karyotype"
;;   :subclass AffectsKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/deletion 1 h/HumanChromosome21))

;; (defclass test-deletion-band-terminal
;;   :label "The 46,XX,del(1)(p13) karyotype"
;;   :subclass AffectsKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/deletion 1 h/HumanChromosome1Bandp13))

;; (defclass test-deletion-two-bands
;;   :label "The 46,XX,del(1)(p13p11) karyotype"
;;   :subclass AffectsKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/deletion 1 h/HumanChromosome1Bandp13 h/HumanChromosome1Bandp11))

;; (defclass test-deletion-one-band
;;   :label "The 46,XX,del(1)(p13p13) karyotype"
;;   :subclass AffectsKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/deletion 1 h/HumanChromosome1Bandp13 h/HumanChromosome1Bandp13))

;; ;; both events
;; (defclass test-both-events
;;   :label "The 46,XX,del(1)(p13pTer),add(q21) karyotype"
;;   :subclass AffectsKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/deletion 1 h/HumanChromosome1Bandp13 h/HumanChromosome1BandpTer)
;;   (e/addition 1 h/HumanChromosome1Bandq21))

;; ;; MAIN - (may) redefine classes defined above to include affects
;; ;; oproperty.
;; (doseq [clazz (subclasses affects1 AffectsKaryotype)]
;;   (affects1-driver affects1 clazz))