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
  (defn derivative [n chromosome & events]
    (exactly n hasFeature (owland DerivativeChromosome chromosome events)))

  ;; MUST be defined before dicentric as dicentric calls isodicentric function!
  (defclass Isochromosomes)
  (defclass IsodicentricChromosome)
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
  (defclass Neocentromere)
  
  (defclass PseudodicentricChromosome)
  (defn pseudo_dicentric [n band1 band2]
    (exactly n hasFeature (owland PseudodicentricChromosome (owlsome e/hasBreakPoint band1 band2))))

  (defclass PseudoisodicentricChromosome)
  (defn pseudo_isodicentric [n band]
    (exactly n hasFeature (owland PseudoisodicentricChromosome (owlsome e/hasBreakPoint band))))

  (defclass RecombiantChromosome)
  (defclass RingChromosome)
  (defclass TelomericAssociations)
  (defclass TricentricRingChromosome)
)