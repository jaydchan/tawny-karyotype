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
(defoproperty hasFeature
  :range Feature
  :domain k/Karyotype
  )



;; define all the structural features
(as-disjoint-subclasses
 Feature
  (defclass DerivativeChromosome)

  (defn derivative [n chromosome & events]
    (exactly n hasFeature (owland DerivativeChromosome chromosome events)))

  (defclass DicentricChromosome)
  (defclass FragileSites)
  (defclass HomogeneouslyStainingRegion)
  (defclass IsoderiviativeChromosome)
  (defclass Isochromosomes)
  (defclass IsodicentricChromosome)
  (defclass MarkerChromosome)
  (defclass Neocentromere)
  (defclass PseudodicentricChromosome)
  (defclass PseudoisodicentricChromosome)
  (defclass RecombiantChromosome)
  (defclass RingChromosome)
  (defclass TelomericAssociations)
  (defclass TricentricRingChromosome)
)