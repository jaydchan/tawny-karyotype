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
  (:require [tawny [reasoner :as r]]
            [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]
            [ncl.karyotype [events :as e]]
            ))

(defontology features
  :iri "http://ncl.ac.uk/karyotype/features"
  :prefix "fea:")

;; import all ncl.karyotype axioms
(owlimport e/events)

(defclass Feature)

;; define object properties
(as-inverse
 (defoproperty hasFeature
   :range Feature
   :domain k/Karyotype
   )

 (defoproperty isFeatureOf
   :range k/Karyotype
   :domain Feature
   )
)

;; define all the structural features
(as-disjoint-subclasses
 Feature

  (defclass DerivativeChromosome)
  ;; TODO if whole arm translocation can be defined as either rob or der
  (defn derivative [n & args]
    (exactly n hasFeature (owland DerivativeChromosome args)))

  (defclass Isochromosome)
  (defn isochromosome [n band]
    (exactly n hasFeature (owland Isochromosome (owlsome e/hasBreakPoint band))))

  (defclass IsodicentricChromosome)
  ;; MUST be defined before dicentric as dicentric calls isodicentric function!
  (defn isodicentric [n band]
    (exactly n hasFeature (owland IsodicentricChromosome (owlsome e/hasBreakPoint band))))

  (defclass DicentricChromosome)
  (defn dicentric [n band1 band2]
    (if (= (str band1) (str band2))
      (isodicentric n band1)
      (exactly n hasFeature (owland DicentricChromosome (owlsome e/hasBreakPoint band1 band2)))))

  (defclass FragileSite)
  (defn fragilesite [n band]
    (exactly n hasFeature (owland FragileSite (owlsome e/hasBreakPoint band))))

  (defclass HomogeneouslyStainingRegion)
  (defn hsr 
    ([n band]
       (exactly n hasFeature (owland HomogeneouslyStainingRegion (owlsome e/hasBreakPoint band))))
    ([n band1 band2]
       (exactly n hasFeature (owland HomogeneouslyStainingRegion (owlsome e/hasBreakPoint band1 band2)))))

  (defclass IsoderivativeChromosome)
  (defn isoderivative [n chromosome arm & events]
    (exactly n hasFeature (owland IsoderivativeChromosome chromosome arm events)))

  (defclass MarkerChromosome)
  (defn marker [n]
    (exactly n hasFeature (owland MarkerChromosome h/HumanChromosome)))

  (defclass Neocentromere)
  
  (defclass PseudodicentricChromosome)
  (defn pseudo_dicentric [n band1 band2]
    (exactly n hasFeature (owland PseudodicentricChromosome (owlsome e/hasBreakPoint band1 band2))))

  (defclass PseudoisodicentricChromosome)
  (defn pseudo_isodicentric [n band]
    (exactly n hasFeature (owland PseudoisodicentricChromosome (owlsome e/hasBreakPoint band))))

  (defclass RecombiantChromosome)

  ;; if whole arm translocation can be defined as either rob or der
  (defclass RobertsonianTranslocation)
  (defn robertsonian [n band1 band2]
    (exactly n hasFeature (owland RobertsonianTranslocation (owlsome e/hasBreakPoint band1 band2))))

  (defclass RingChromosome)
  ;; TOFIX - ORDER IS IMPORTANT
  (defn ring
    ([n chromosome]
       (exactly n hasFeature (owland RingChromosome chromosome)))
    ([n band1 band2]
       (exactly n hasFeature (owland RingChromosome (owlsome e/hasBreakPoint band1 band2))))
    ([n band1 band2 band3 band4]
       (exactly n hasFeature (owland RingChromosome (owlsome e/hasBreakPoint band1 band2 band3 band4))))
    ([n band1 band2 band3 band4 band5]
       (exactly n hasFeature (owland RingChromosome (owlsome e/hasBreakPoint band1 band2 band3 band4 band5)))))

  (defclass TelomericAssociations)

  ;; TOFIX - hard-coded!
  (defclass TricentricChromosome)
  (defn tricentric [n band1 band2 band3 band4]
      (exactly n hasFeature (owland TricentricChromosome (owlsome e/hasBreakPoint band1 band2 band3 band4))))

  (defclass TricentricRingChromosome)

  (defclass UniparentalDisomy)

)

(as-disjoint-subclasses
 RingChromosome
 (defclass MonoCentricRingChromosome)
 (defclass DicentricRingChromosome)
 (defclass TricentricRingChromosome))