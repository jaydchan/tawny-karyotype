;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2012, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.


(ns ncl.karyotype.events
  (:use [owl.owl])
  (:require [owl [reasoner :as r]]))

(defontology events
  :file "events.omn"
  :iri "http://ncl.ac.uk/events"
  :prefix "evn:")

(defclass Event)

;; define object properties
(defoproperty hasEvent
  :range Event
  :domain Karyotype
  )

(defoproperty affects
  :range Band
  :domain Chromosome
  )

(defoproperty hasBreakPoints
  :range Band
  :domain Chromosome
  )

;; define all the events
(as-disjoint-subclasses
 Event
  (def addition)
  (def deletion)
  (def derivative_chromosome)
  (def recombiant_chromosome)
  (def isoderiviative_chromosome)
  (def dicentric_chromosome)
  (def isodicentric_chromosome)
  (def pseudodicentric_chromosome)
  (def pseudoisodicentric_chromosome)
  (def duplication)
  (def fission)
  (def fragile_sites)
  (def homogeneously_staining_region)
  (def insertion)
  (def inversions)
  (def isochromosomes)
  (def marker_chromosome)
  (def neocentromere)
  (def quadruplication)
  (def ring_chromosome)
  (def tricentric_ring_chromosome)
  (def telomeric_associations)
  (def translocations)
  (def triplication))
           

