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

(ns ncl.karyotype.base
  (:use [tawny.owl])
  (:require [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [events :as e]]
            [ncl.karyotype [features :as f]]))

(defontology base
  :iri "http://ncl.ac.uk/karyotype/base"
  :prefix "base:")

;; import all ncl.karyotype axioms
(owl-import k/karyotype)
(owl-import e/events)
(owl-import f/features)

(defclass BaseKaryotype
  :subclass k/Karyotype)

;; define object properties
(as-inverse
 (defoproperty derivedFrom
   :characteristic :transitive
   :domain k/Karyotype
   :range k/Karyotype)

 (defoproperty derivedTo
   :range k/Karyotype
   :domain k/Karyotype))

;; define chain properties
;; due to build dependancy, the subproperty chain axiom for
;; hasDerivedEvent and isDerivedEventOf is defined here i.e. after
;; derivedFrom and derivedTo have been defined.
(add-subpropertychain e/hasDerivedEvent (list derivedFrom e/hasEvent))
(add-subpropertychain e/isDerivedEventOf (list e/isEventOf derivedTo))

;; define all the baseKaryotypes
;; we have to pass these in as strings because they start with
;; integers which brings up an NumberFormatException therefore we
;; could use :name "46_XX" or :label "The 46,XX karyotype"

;; define all haploid base karyotypes
(defclass k23_N
  :label "The 23,N karyotype"
  :subclass BaseKaryotype)
(as-disjoint-subclasses
 k23_N
 (defclass k23_X
   :label "The 23,X karyotype")
 (defclass k23_Y
   :label "The 23,Y karyotype"))

;; define all diploid base karyotypes
(defclass k46_XN
  :label "The 46,XN karyotype"
  :subclass BaseKaryotype)
(as-disjoint-subclasses
 k46_XN
 (defclass k46_XX
   :label "The 46,XX karyotype")
 (defclass k46_XY
   :label "The 46,XY karyotype"))

;; define all triploid base karyotypes #CHECK
(defclass k69_XNN
  :label "The 69,XNN karyotype"
  :subclass BaseKaryotype)
(as-disjoint-subclasses
 k69_XNN
 (defclass k69_XXX
   :label "The 69,XXX karyotype")
 (defclass k69_XXY
   :label "The 69,XXY karyotype")
 (defclass k69_XYY
   :label "The 69,XYY karyotype"))

;; define all tetraploid base karyotypes #CHECK
(defclass k92_XNNN
  :label "The 92,XNNN karyotype"
  :subclass BaseKaryotype)
(as-disjoint-subclasses
 k92_XNNN
 (defclass k92_XXXX
   :label "The 92,XXXX karyotype")
 (defclass k92_XXXY
   :label "The 92,XXXY karyotype")
 (defclass k92_XXYY
   :label "The 92,XXYY karyotype")
 (defclass k92_XYYY
   :label "The 92,XYYY karyotype"))
