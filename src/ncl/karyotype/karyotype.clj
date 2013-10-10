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

(ns ncl.karyotype.karyotype
  (:use [tawny.owl]))

(defontology karyotype
  :iri "http://ncl.ac.uk/karyotype/karyotype"
  :prefix "kar:")

;; OWL CLASSES
(as-disjoint
 (defclass Karyotype)
 (defclass Chromosome)
 (defclass ChromosomeComponent))

(as-disjoint-subclasses
 ChromosomeComponent
 (defclass ChromosomeBand)
 (defclass Centromere)
 (defclass Telomere))

;; define object properties
(as-inverse
 (defoproperty isComponentOf
   :domain ChromosomeComponent)

 (defoproperty hasComponent))

(as-inverse
 (defoproperty isBandOf
   :subproperty isComponentOf
   :domain ChromosomeBand)

 (defoproperty hasBand))

(as-inverse
 (defoproperty isSubBandOf
   :domain ChromosomeBand
   :range ChromosomeBand)

 (defoproperty hasSubBand))