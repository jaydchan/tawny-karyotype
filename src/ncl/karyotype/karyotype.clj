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


(ns ncl.karyotype.karyotype
  (:use [tawny.owl])
  (:require [tawny [reasoner :as r]]))

(defontology karyotype
  :iri "http://ncl.ac.uk/karyotype/karyotype"
  :prefix "kar:")

(defclass Karyotype)

(defclass Chromosome)

(defclass ChromosomeComponent)

(as-disjoint-subclasses
 ChromosomeComponent
 
 (defclass ChromosomeBand)
 (defclass Centromere)
 (defclass Telomere))

(as-inverse
 (defoproperty isBandOf
   :domain ChromosomeBand
   :range Chromosome
   :characteristics functional)
 
 (defoproperty hasBand
   :domain Chromosome
   :range ChromosomeBand)
 )


