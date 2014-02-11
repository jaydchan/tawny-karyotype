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
definitions to include affects data property."
      :author "Jennifer Warrender"}
  ncl.karyotype.affects2
  (:use [tawny.owl])
  (:require [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]
            [ncl.karyotype [events :as e]]
            [ncl.karyotype [parse :as p]]
            [ncl.karyotype [affects1 :as a]]
            ;; Testing purposes only
            [ncl.karyotype [base :as b]]
            [tawny [render :as r]]))

(defontology affects2
  :iri "http://ncl.ac.uk/karyotype/affects2"
  :prefix "af2:"
  :comment "Affects (take 2) ontology for Human Karyotype Ontology,
  written using the tawny-owl library.")

(defdproperty affects)
(defdproperty hasOrdinalNumber)

;; AUXILIARY FUNCTIONS

;; Awful!!!
;; (defn generate-ordinal-number [band]
;;   (let [str-band (re-find #"[\dXY]+Band[pq][\d\.]+" (str band))
;;         chromosome (re-find #"[\d+XY]" str-band)
;;         arm (if (= "p" (re-find #"[pq]" str-band)) "0" "1")
;;         band-name (subs (re-find #"[pq][\d\.]+" str-band) 1)
;;         band-ordinal (int (* (read-string band-name) 10))]
;;     (read-string (str "1" chromosome arm band-ordinal))))

(defn get-ordinal [band]
  (+ 1 (.indexOf a/bands-300 band)))

;; Set ordinal value for each chromosome
(doseq [clazz a/bands-300]
  (refine clazz
          :subclass (data-has-value hasOrdinalNumber
                                    (literal (get-ordinal clazz)))))

;; PATTERNS
(defn affects-band [bands]
  (data-only affects
             (apply data-oneof
                    (map #(literal %) bands))))

;; DRIVERS
(defn get-affects [bands]
  "Returns either one band or a range of bands, determined by BANDS."
  (cond
   (= (count bands) 1)
   (get-ordinal (first bands))
   (= (count bands) 2)
   (map get-ordinal (a/band-range (first bands) (second bands)))
   :default
   (throw
    (IllegalArgumentException.
     (str "Get-affects expects at most two bands. Got:" bands)))))

(defn get-affects2 [o clazz]
  "Returns a list of affected bands for a given CLAZZ in
ontology O."
  (let [parents (direct-superclasses o clazz)
        restrictions (filter #(instance?
                               org.semanticweb.owlapi.model.OWLRestriction %)
                             parents)
        events (filter #(= (.getProperty %) e/hasDirectEvent) restrictions)
        axioms (into [] (map #(.getFiller %) events))
        chrom_band (map p/human-filter axioms)
        bands (into [] (filter #(h/band? (first %)) chrom_band))]

    (flatten
     (for [band bands]
       (get-affects band)))))

(defn affects2-driver [o clazz]
  "Returns the updated class definition of CLAZZ in ontology O."
  (let [bands (get-affects2 o clazz)]
    (if (= (count bands) 0)
      clazz
      (refine clazz :subclass (affects-band bands)))))

;; TESTS
;; import human ontology axioms
(owl-import h/human)

(defclass AffectsKaryotype
  :subclass k/Karyotype)

;; addition
(defclass test-addition-chromosome
  :label "The 47,XX,+21 karyotype"
  :subclass AffectsKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 1 h/HumanChromosome21))

(defclass test-addition-band
  :label "The 46,XX,add(1)(p13) karyotype"
  :subclass AffectsKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 1 h/HumanChromosome1Bandp13))

(defclass test-addition-both
  :label "The 47,XX,add(1)(p13),+21 karyotype"
  :subclass AffectsKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 1 h/HumanChromosome1Bandp13)
  (e/addition 1 h/HumanChromosome21))

;; deletion
(defclass test-deletion-chromosome
  :label "The 47,XX,-21 karyotype"
  :subclass AffectsKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/deletion 1 h/HumanChromosome21))

(defclass test-deletion-band-terminal
  :label "The 46,XX,del(1)(p13) karyotype"
  :subclass AffectsKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/deletion 1 h/HumanChromosome1Bandp13))

(defclass test-deletion-two-bands
  :label "The 46,XX,del(1)(p13p11) karyotype"
  :subclass AffectsKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/deletion 1 h/HumanChromosome1Bandp13 h/HumanChromosome1Bandp11))

(defclass test-deletion-one-band
  :label "The 46,XX,del(1)(p13p13) karyotype"
  :subclass AffectsKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/deletion 1 h/HumanChromosome1Bandp13 h/HumanChromosome1Bandp13))

;; both events
(defclass test-both-events
  :label "The 46,XX,del(1)(p13pTer),add(q21) karyotype"
  :subclass AffectsKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/deletion 1 h/HumanChromosome1Bandp13 h/HumanChromosome1BandpTer)
  (e/addition 1 h/HumanChromosome1Bandq21))

;; MAIN - (may) redefine classes defined above to include affects
;; dproperty.
(doseq [clazz (subclasses affects2 AffectsKaryotype)]
  (affects2-driver affects2 clazz))