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

(ns ncl.karyotype.named
  (:use [tawny.owl])
  (:require [tawny [reasoner :as r]]
            [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]
            [ncl.karyotype [events :as e]]
            )
  )

(defontology named
  :iri "http://ncl.ac.uk/karyotype/named"
  :prefix "nmd:"
  )

;; import all ncl.karyotype axioms
(owlimport k/karyotype)
(owlimport h/human)
(owlimport e/events)

(defclass NamedKaryotype
  :subclass k/Karyotype)

(defclass BaseKaryotype
  :subclass k/Karyotype)

;; define object properties
(defoproperty derivedFrom
  :characteristics transitive
  :domain NamedKaryotype)

;; define all the baseKaryotypes
 ;;we have to pass these in as strings because they start with
 ;;integers which brings up an NumberFormatException therefore we
 ;;could use :name "46_XX" or :label "The 46,XX karyotype"

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

;; TODO
;; define all triploid base karyotypes
(defclass k69_N
  :label "The 69,N karyotype"
  :subclass BaseKaryotype)
(defclass k69_X
  :label "The 69,X karyotype"
  :subclass k69_N)
(defclass k69_Y
  :label "The 69,Y karyotype"
  :subclass k69_N)
;; TODO
;; define all tetraploid base karyotypes
(defclass k92_N
  :label "The 92,N karyotype"
  :subclass BaseKaryotype)
(defclass k92_X
  :label "The 92,X karyotype"
  :subclass k92_N)
(defclass k92_Y
  :label "The 92,Y karyotype"
  :subclass k92_N)

;; Define the namedKaryotypes

;; An individual with three copies of chromosome 21 ;;aka Trisomy21
(defclass DownSyndrome
  :subClass NamedKaryotype
  :equivalent
  (exactly 1 e/hasEvent ((owland e/Addition h/HumanChromosome21))))

;; An individual with only one sex chromosome, an X)
(defclass TurnerSyndrome
  :subClass NamedKaryotype
  :equivalent
  (exactly 1 e/hasEvent ((owland e/Deletion h/HumanChromosomeY))))

;; example defined classes

;; Define gender

;; male diploid only
(defclass MaleKaryotype
  :equivalent
  (owlor k46_XY
         (owlsome derivedFrom k46_XY)))

;; female diploid only = those derived from 46,XX - as 46,XY is an asserted subclass of 46,XN cannot be included in the definition - what happens about 45,X?
(defclass FemaleKaryotype
  :equivalent
  (owlor k46_XX
         (owlsome derivedFrom k46_XX)))

;; Define numerical abnormalities - in order for these to work, need to import the human axioms from h/human

(defclass NumericalAbnormalKaryotypeAutosomalGain
  :equivalent
  (owlsome e/hasEvent (owland e/Addition h/HumanAutosome)))

(defclass NumericalAbnormalKaryotypeAutosomalLoss
  :equivalent
  (owlsome e/hasEvent (owland e/Deletion h/HumanAutosome)))

(defclass NumericalAbnormalKaryotypeAutosomalGainOrLoss
  :equivalent
  (owlor (owlsome e/hasEvent (owland e/Addition h/HumanAutosome))
         (owlsome e/hasEvent (owland e/Deletion h/HumanAutosome))))

(defclass NumericalAbnormalKaryotypeAllosomalGain
  :equivalent
  (owlsome e/hasEvent (owland e/Addition h/HumanAllosome)))

(defclass NumericalAbnormalKaryotypeAllosomalLoss
  :equivalent
  (owlsome e/hasEvent (owland e/Deletion h/HumanAllosome)))

(defclass NumericalAbnormalKaryotypeAllosomalGainOrLoss
  :equivalent
  (owlor (owlsome e/hasEvent (owland e/Addition h/HumanAllosome))
         (owlsome e/hasEvent (owland e/Deletion h/HumanAllosome))))

(defclass NumericalAbnormalKaryotypeChromosomalGain
  :equivalent
  (owlsome e/hasEvent (owland e/Addition h/HumanChromosome)))

(defclass NumericalAbnormalKaryotypeChromosomalLoss
  :equivalent
  (owlsome e/hasEvent (owland e/Deletion h/HumanChromosome)))

;;AKA aneuploidy
(defclass NumericalAbnormalKaryotype
  :equivalent
  (owlor (owlsome e/hasEvent (owland e/Addition h/HumanChromosome))
         (owlsome e/hasEvent (owland e/Deletion h/HumanChromosome))))

;; Define structural abnormalities - in order for these to work, need to import the human axioms from h/human and e/events

(defclass StructuralAbnormalKaryotypeInsertion
  :equivalent
  (owlsome e/hasEvent (owland e/Insertion (owlsome e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotypeInversion
  :equivalent
  (owlsome e/hasEvent (owland e/Inversion (owlsome e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotype
  :equivalent
  (owlsome e/hasEvent (owland e/Event (owlsome e/hasBreakPoint h/HumanChromosomeBand))))
