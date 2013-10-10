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

(ns ncl.karyotype.features
  (:use [tawny.owl])
  (:require [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]
            [ncl.karyotype [events :as e]]))

(defontology features
  :iri "http://ncl.ac.uk/karyotype/features"
  :prefix "fea:")

;; import all ncl.karyotype axioms
(owl-import e/events)

(defclass Feature)

;; define object properties
(as-inverse
 (defoproperty hasFeature
   ;; :range Feature
   :domain k/Karyotype)

 (defoproperty isFeatureOf
   :range k/Karyotype
   ;;:domain Feature
   ))

(as-inverse
 (defoproperty hasRearrangedChromosome
   :range k/Karyotype)

 (defoproperty isRearrangedChromosomeOf
   :domain k/Karyotype
   ))


;; OWL CLASSES - STRUCTURAL FEATURES
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


;; FUNCTIONS
;; TODO if whole arm translocation can be defined as either rob or der
(defn derivative
  "Returns a derivative restriction.
n is the number of derivative restrictions.
args is a list of events."
  [n chromosome & args]
  (exactly n hasFeature
           (owl-and DerivativeChromosome
                    (owl-some isRearrangedChromosomeOf chromosome
                    args))))

;; MUST be defined before dicentric as dicentric calls isodicentric function!
(defn isodicentric
  "Returns an isodicentric restriction.
n is the number of isodicentric restrictions.
band is of type HumanChromosomeBand."
  [n band]
  (exactly n hasFeature
           (owl-and IsodicentricChromosome
                   (owl-some e/hasBreakPoint band))))

(defn dicentric
  "Returns either a dicentric or isdicentric restriction.
n is the number of isodicentric restrictions.
band1, band2 is of type HumanChromosomeBand."
  [n band1 band2]
  (if (= (str band1) (str band2))
    ;; If band1 and band2 are equivalent then create an isodicentric
    ;; restriction.
    (isodicentric n band1)
    ;; Else create a dicentric restriction.
    (exactly n hasFeature
             (owl-and DicentricChromosome
                     (owl-some e/hasBreakPoint band1 band2)))))

(defn fragilesite
  "Returns a fragilesite restriction.
n is the number of fragilesite restrictions.
band is of type HumanChromosomeBand."
  [n band]
  (exactly n hasFeature
           (owl-and FragileSite
                   (owl-some e/hasBreakPoint band))))

(defn hsr
  "Returns a homogeneouslystainingregion restriction.
n is the number of homogeneouslystainingregion restrictions.
band, band1, band2 is of type HumanChromosomeBand."
  ;; Used to describe the presence, but not the size, of a hsr region
  ;; on a chromosome, arm or band.
  ([n band]
     (exactly n hasFeature
              (owl-and HomogeneouslyStainingRegion
                      (owl-some e/hasBreakPoint band))))
  ;; Used to describe the presence of a hsr, located at the interface
  ;; between segments of different chromosome involved in a
  ;; rearrangement.
  ([n band1 band2]
     (exactly n hasFeature
              (owl-and HomogeneouslyStainingRegion
                      (owl-some e/hasBreakPoint band1 band2)))))

(defn isochromosome
  "Returns an isochromosome restriction.
n is the number of isochromosome restrictions.
band is of type HumanChromosomeBand."
  [n band]
  (exactly n hasFeature
           (owl-and Isochromosome
                   (owl-some e/hasBreakPoint band))))

(defn isoderivative
  "Returns an isoderivative restriction.
n is the number of isoderivative restrictions.
chromosome is ...
arm is ...
events is ..."
  [n chromosome arm & events]
  (exactly n hasFeature
           (owl-and IsoderivativeChromosome chromosome arm events)))

(defn marker
  "Returns a marker restriction.
n is the number of marker restrictions."
  [n]
  (exactly n hasFeature
           (owl-and MarkerChromosome h/HumanChromosome)))

;; TODO Neocentromere - LONG STRING FORMAT

(defn pseudo_dicentric
  "Returns a pseudodicentric restriction.
n is the number of pseudodicentric restrictions.
band1, band2 is of type HumanChromosomeBand."
  [n band1 band2]
  (exactly n hasFeature
           (owl-and PseudodicentricChromosome
                   (owl-some e/hasBreakPoint band1 band2))))

(defn pseudo_isodicentric
  "Returns a pseudoisodicentric restriction.
n is the number of pseudoisodicentric restrictions.
band is of type HumanChromosomeBand."
  [n band]
  (exactly n hasFeature
           (owl-and PseudoisodicentricChromosome
                   (owl-some e/hasBreakPoint band))))

;; TODO RecombiantChromosome

;; if whole arm translocation can be defined as either rob or der
(defn robertsonian
  "Returns a robertsonian restriction.
n is the number of robertsonian restrictions.
band1, band2 is of type HumanChromosomeBand."
[n band1 band2]
  (exactly n hasFeature
           (owl-and RobertsonianTranslocation
                   (owl-some e/hasBreakPoint band1 band2))))

;; TOFIX - ORDER IS IMPORTANT
(defn ring
  "Returns a ring restriction.
n is the number of ring restrictions.
chromosome is of type HumanChromosome.
band1, band2 is of type HumanChromosomeBand."
  ([n chromosome]
     (exactly n hasFeature
              (owl-and RingChromosome chromosome)))
  ([n band1 band2]
     (exactly n hasFeature
              (owl-and RingChromosome
                      (owl-some e/hasBreakPoint band1 band2))))
  ([n band1 band2 band3 band4]
     (exactly n hasFeature
              (owl-and RingChromosome
                      (owl-some e/hasBreakPoint band1 band2 band3 band4))))
  ([n band1 band2 band3 band4 band5]
     (exactly n hasFeature
              (owl-and RingChromosome
                      (owl-some e/hasBreakPoint band1 band2 band3
                               band4 band5)))))

;; TODO TelomericAssociations

;; TOFIX - hard-coded!
(defn tricentric
  "Returns a tricentric restriction.
n is the number of tricentric restrictions.
band1, band2, band3, band4 is of type HumanChromosomeBand."
  [n band1 band2 band3 band4]
  (exactly n hasFeature
           (owl-and TricentricChromosome
                   (owl-some e/hasBreakPoint band1 band2 band3 band4))))

;; TODO UniparentalDisomy