;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2012, Newcastle University

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


(ns ncl.karyotype.events
  (:use [owl.owl])
  (:require [owl [reasoner :as r]]
            [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]))

(defontology events
  :iri "http://ncl.ac.uk/karyotype/events"
  :prefix "evn:")

;; import karyotype and human axioms
;;(owlimport k/Karyotype)
;;(owlimport h/Human)

(defclass Event)

;; define object properties
(defoproperty hasEvent
  :range Event
  :domain k/Karyotype
  )

(defoproperty effects
  :range k/ChromosomeBand
  :domain k/Karyotype
  )

(defoproperty hasBreakPoint
  :range k/ChromosomeBand
  :domain k/Karyotype
  )

;; define all the events
;; TODO define restrictions
(as-disjoint-subclasses
 Event
  (defclass Addition)
  (defclass Deletion)
  (defclass Duplication)
  (defclass Fission)
  (defclass Insertion)
  (defclass Inversion)
  (defclass Quadruplication)
  (defclass Translocation)
  (defclass Triplication)
)

;; Potential new .clj file
;; (defclass Feature)

;; define object properties
;; (defoproperty hasFeature
;;   :range Feature
;;   :domain k/Karyotype
;;   )

;; define all the structural features
;; (as-disjoint-subclasses
;;  Feature
;;   (defclass derivative_chromosome)
;;   (defclass dicentric_chromosome)
;;   (defclass fragile_sites)
;;   (defclass homogeneously_staining_region)
;;   (defclass isoderiviative_chromosome)
;;   (defclass isochromosomes)
;;   (defclass isodicentric_chromosome)
;;   (defclass marker_chromosome)
;;   (defclass neocentromere)
;;   (defclass pseudodicentric_chromosome)
;;   (defclass pseudoisodicentric_chromosome)
;;   (defclass recombiant_chromosome)
;;   (defclass ring_chromosome)
;;   (defclass telomeric_associations)
;;   (defclass tricentric_ring_chromosome)
;; )

