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

(ns ^{:doc "Defining feature information for human karyotypes."
      :author "Jennifer Warrender"}
  ncl.karyotype.features
  (:use [tawny.owl])
  (:require [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]
            [ncl.karyotype [events :as e]]))

(defontology features
  :iri "http://ncl.ac.uk/karyotype/features"
  :prefix "fea:"
  :comment "Feature ontology for Human Karyotype Ontology, written using
  the tawny-owl library.")

;; import ncl.karyotype.event axioms
(owl-import e/events)

;; OWL CLASSES - STRUCTURAL FEATURES
(defclass Feature)

(as-disjoint-subclasses
 Feature
 (defclass DerivativeChromosome)
 (defclass IsodicentricChromosome)
 (defclass DicentricChromosome)
 (defclass FragileSite)
 (defclass HomogeneouslyStainingRegion)
 (defclass Isochromosome)
 (defclass IsoderivativeChromosome)
 (defclass MarkerChromosome)
 (defclass Neocentromere)
 (defclass PseudodicentricChromosome)
 (defclass PseudoisodicentricChromosome)
 (defclass RecombiantChromosome)
 (defclass RobertsonianTranslocation)
 (defclass RingChromosome)
 (defclass TelomericAssociations)
 (defclass TricentricChromosome)
 (defclass UniparentalDisomy))

(as-disjoint-subclasses
 RingChromosome
 (defclass MonoCentricRingChromosome)
 (defclass DicentricRingChromosome)
 (defclass TricentricRingChromosome))

;; define object properties
;; feature object properties
(as-inverse
 (defoproperty hasFeature
   :range Feature
   :domain k/Karyotype)
 (defoproperty isFeatureOf))

(as-inverse
 (defoproperty hasDirectFeature
   :subproperty hasFeature)
 (defoproperty isDirectFeatureOf
   :subproperty isFeatureOf))

;; due to build dependancy, the subproperty chain axiom will be added
;; in named-clj
(as-inverse
 (defoproperty hasDerivedFeature
   :subproperty hasFeature)
 (defoproperty isDerivedFeatureOf
   :subproperty isFeatureOf))

;; TODO
(as-inverse
 (defoproperty hasRearrangedChromosome
   :range k/Karyotype
   :domain h/HumanChromosome)
 (defoproperty isRearrangedChromosomeOf))

;; hasFeature auxiliary functions
(defn- some-feature
  "Returns a LazySeq of SomeValuesFrom hasFeature restrictions."
  [axiom]
  (owl-some hasFeature axiom))

(defn- exactly-feature
  "Returns a (single) ExactCardinality hasFeature restriction."
  [n axiom] {:pre (number? n)}
  (exactly n hasFeature axiom))

(defn feature
  "(Either) Returns a LazySeq SomeValuesFrom or one ExactCardinality
hasFeature restrictions."
  [n axiom]
  (if (nil? n)
    (some-feature axiom)
    (exactly-feature n axiom)))

;; hasDirectFeature auxiliary functions
(defn- some-direct-feature
  "Returns a LazySeq of SomeValuesFrom hasDirectFeature restrictions"
  [axiom]
  (owl-some hasDirectFeature axiom))

(defn- exactly-direct-feature
  "Returns a (single) ExactCardinality hasDirectFeature restriction."
  [n axiom] {:pre (number? n)}
  (exactly n hasDirectFeature axiom))

(defn direct-feature
  "(Either) Returns a LazySeq SomeValuesFrom or one ExactCardinality
hasDirectFeature restrictions."
  [n axiom]
  (if (nil? n)
    (some-direct-feature axiom)
    (exactly-direct-feature n axiom)))

;; FUNCTIONS
;; TODO if whole arm translocation can be defined as either rob or der
(defn derivative
  "Returns a derivative restriction. N is the number of derivative
  restrictions. ARGS is a list of events."
  [n chromosome & args] {:pre (true? (every? h/chromosome? chromosome))}
  (direct-feature n
                  (owl-and DerivativeChromosome
                           (apply (partial owl-some isRearrangedChromosomeOf)
                                  chromosome)
                           args)))

;; MUST be defined before dicentric as dicentric calls isodicentric function!
(defn isodicentric
  "Returns an isodicentric restriction. N is the number of
  isodicentric restrictions. BAND is of type HumanChromosomeBand."
  [n band] {:pre (true? (h/band? band))}
  (direct-feature n
                  (owl-and IsodicentricChromosome
                    (owl-some e/hasBreakPoint band))))

(defn dicentric
  "Returns either a dicentric or isdicentric restriction. N is the
  number of isodicentric restrictions. BAND1, BAND2 is of type
  HumanChromosomeBand."
  [n band1 band2]
  {:pre (true? (and (h/band? band1) (true? (h/band? band2))))}
  (if (= band1 band2)
    ;; If band1 and band2 are equivalent then create an isodicentric
    ;; restriction.
    (isodicentric n band1)
    ;; Else create a dicentric restriction.
    (direct-feature n
                    (owl-and DicentricChromosome
                             (owl-some e/hasBreakPoint band1 band2)))))

(defn fragilesite
  "Returns a fragilesite restriction. N is the number of fragilesite
  restrictions. BAND is of type HumanChromosomeBand."
  [n band] {:pre (true? (h/band? band))}
  (direct-feature n
                  (owl-and FragileSite
                           (owl-some e/hasBreakPoint band))))

(defn hsr
  "Returns a homogeneouslystainingregion restriction.
n is the number of homogeneouslystainingregion restrictions.
band, band1, band2 is of type HumanChromosomeBand."
  ;; Used to describe the presence, but not the size, of a hsr region
  ;; on a chromosome, arm or band.
  ([n band]
     {:pre (true? (h/band? band))}
     (direct-feature n
                     (owl-and HomogeneouslyStainingRegion
                              (owl-some e/hasBreakPoint band))))
  ;; Used to describe the presence of a hsr, located at the interface
  ;; between segments of different chromosome involved in a
  ;; rearrangement.
  ([n band1 band2]
     {:pre (true? (and (h/band? band1) (true? (h/band? band2))))}
     (direct-feature n
                     (owl-and HomogeneouslyStainingRegion
                              (owl-some e/hasBreakPoint band1 band2)))))

(defn isochromosome
  "Returns an isochromosome restriction. N is the number of
isochromosome restrictions. BAND is of type HumanChromosomeBand."
  [n band] {:pre (true? (h/band? band))}
  (direct-feature n
                  (owl-and Isochromosome
                           (owl-some e/hasBreakPoint band))))

;; TODO
(defn isoderivative
  "Returns an isoderivative restriction.
N is the number of isoderivative restrictions.
CHROMOSOME is ...
ARM is ...
EVENTS is ..."
  [n chromosome arm & events]
  (direct-feature n
                  (owl-and IsoderivativeChromosome chromosome arm events)))

(defn marker
  "Returns a marker restriction. N is the number of marker
restrictions."
  [n]
  (direct-feature n
                  (owl-and MarkerChromosome h/HumanChromosome)))

;; TODO Neocentromere - LONG STRING FORMAT

(defn pseudo_dicentric
  "Returns a pseudodicentric restriction. N is the number of
pseudodicentric restrictions. BAND1, BAND2 is of type
HumanChromosomeBand."
  [n band1 band2]
  {:pre (true? (and (h/band? band1) (true? (h/band? band2))))}
  (direct-feature n
                  (owl-and PseudodicentricChromosome
                           (owl-some e/hasBreakPoint band1 band2))))

(defn pseudo_isodicentric
  "Returns a pseudoisodicentric restriction. N is the number of
pseudoisodicentric restrictions. BAND is of type HumanChromosomeBand."
  [n band] {:pre (true? (h/band? band))}
  (direct-feature n
                  (owl-and PseudoisodicentricChromosome
                           (owl-some e/hasBreakPoint band))))

;; TODO RecombiantChromosome

;; if whole arm translocation can be defined as either rob or der
(defn robertsonian
  "Returns a robertsonian restriction. N is the number of robertsonian
restrictions. BAND1, BAND2 is of type HumanChromosomeBand."
  [n band1 band2]
  {:pre (true? (and (h/band? band1) (true? (h/band? band2))))}
  (direct-feature n
                  (owl-and RobertsonianTranslocation
                           (owl-some e/hasBreakPoint band1 band2))))

;; TOFIX - ORDER IS IMPORTANT
(defn ring
  "Returns a ring restriction. N is the number of ring
restrictions. CHROMOSOME is of type HumanChromosome. BAND1, BAND2 is
of type HumanChromosomeBand."
  [n & chrom_bands]
  (cond
   (and (= (count chrom_bands) 1) (h/chromosome? (first chrom_bands)))
   (direct-feature n
                   (owl-and RingChromosome (first chrom_bands)))
   (and (> (count chrom_bands) 1) (every? h/band? chrom_bands))
   (direct-feature n
                   (owl-and RingChromosome
                            (apply
                             (partial owl-some e/hasBreakPoint)
                             chrom_bands)))
   :default
   (throw
    (IllegalArgumentException.
     (str "Ring expects a HumanChromosome or
               atleast two HumanChromosomeBand. Got:" chrom_bands)))))

;; TODO TelomericAssociations

;; TOFIX - hard-coded!
(defn tricentric
  "Returns a tricentric restriction. N is the number of tricentric restrictions.
band1, band2, band3, band4 is of type HumanChromosomeBand."
  [n band1 band2 band3 band4]
  (direct-feature n
                  (owl-and TricentricChromosome
                           (owl-some e/hasBreakPoint band1 band2 band3 band4))))

;; TODO UniparentalDisomy