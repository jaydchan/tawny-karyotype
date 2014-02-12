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
definitions to include affects object property and sequence pattern"
      :author "Jennifer Warrender"}
  ncl.karyotype.affects3
  (:use [tawny.owl])
  (:require [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]
            [ncl.karyotype [events :as e]]
            [ncl.karyotype [base :as b]]
            [ncl.karyotype [affects1 :as a]]))

(defontology affects3
  :iri "http://ncl.ac.uk/karyotype/affects3"
  :prefix "af3:"
  :comment "Affects (take 3) ontology for Human Karyotype Ontology,
  written using the tawny-owl library.")

(defoproperty affects)

;; TODO should really import sequence.owl.rdf
(defoproperty precedes)
(defoproperty directlyPrecedes)
(defoproperty directlyFollows)
(defoproperty follows)

;; PATTERNS
(defn- sequence-pattern [clazzes]
  "Pattern - encoded sequence ODP."
  {:pre (= 2 (count clazzes))}
  (owl-and (first clazzes)
           (owl-some directlyPrecedes (rest clazzes))))

(defn- affects-band [bands]
  "Pattern - returns sequence ODP variant for given BANDS, using
affects object property"
  (owl-some affects
            (if (vector? bands)
              (sequence-pattern bands)
              bands)))

;; DRIVERS
(defn- get-affects [o clazz]
  "Returns a list of affects restrictions for a given CLAZZ in
ontology O."
    (flatten
     (for [bands (a/get-bands o clazz)]
       (affects-band bands))))

(defn affects3-driver [o clazz]
  "Returns the updated class definition of CLAZZ in ontology O."
  (let [bands (get-affects o clazz)]
    (if (= (count bands) 0)
      clazz
      (refine clazz
              :ontology o
              :subclass bands))))

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
;; oproperty and sequence pattern.
(doseq [clazz (subclasses affects3 AffectsKaryotype)]
  (affects3-driver affects3 clazz))