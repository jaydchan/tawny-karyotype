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
  (:require [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]
            [ncl.karyotype [events :as e]]))

(defontology named
  :iri "http://ncl.ac.uk/karyotype/named"
  :prefix "nmd:"
  )

;; import all ncl.karyotype axioms
(owl-import k/karyotype)
(owl-import h/human)
(owl-import e/events)

(defclass NamedKaryotype
  :subclass k/Karyotype)

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

;; Define the namedKaryotypes

;; Named Karyotypes that are caused by numerical abnormalities
;; Allosomal abnormalites

;; An (female) individual with only one sex chromosome = X ;;aka 45,X
;; or 45,X0 ;; aka abnormal female
(defclass TurnerSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some derivedFrom k46_XN)
   (e/deletion 1 h/HumanSexChromosome)))

;; An (male) individual with an extra X chromosome ;; aka 47,XXY ;;
;; aka abnormal male
(defclass KlinefelterSyndromeMostCommonVariation
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some derivedFrom k46_XY)
   (e/addition 1 h/HumanChromosomeX)
   (owl-not
    (owl-some e/hasEvent
             (owl-and e/Addition h/HumanChromosomeY)))))

;; An (male) individual with extra X chromosomes ;;aka abnormal male
(defclass KlinefelterSyndromeAllVariations
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some derivedFrom k46_XY)
   (owl-some e/hasEvent
            (owl-and e/Addition h/HumanChromosomeX))))

;; #TODO An individual with no X chromosome
;; (defclass Lethal
;;   :subclass NamedKaryotype)

;; Autsomal abnormalities

;; An individual with three copies of chromosome 8 ;;aka Trisomy8
(defclass Warkany2Syndrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some derivedFrom k46_XN)
   (e/addition 1 h/HumanChromosome8)))

;; An individual with three copies of chromosome 9
(defclass Trisomy9
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some derivedFrom k46_XN)
   (e/addition 1 h/HumanChromosome9)))

;; An individual with three copies of chromosome 13 ;;aka Trisomy13
(defclass PatauSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some derivedFrom k46_XN)
   (e/addition 1 h/HumanChromosome13)))

;; An individual with three copies of chromosome 16
(defclass Trisomy16
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some derivedFrom k46_XN)
   (e/addition 1 h/HumanChromosome16)))

;; An individual with three copies of chromosome 18 ;;aka Trisomy18
(defclass EdwardsSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some derivedFrom k46_XN)
   (e/addition 1 h/HumanChromosome18)))

;; An individual with three copies of chromosome 21 ;;aka Trisomy21
(defclass DownSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some derivedFrom k46_XN)
   (e/addition 1 h/HumanChromosome21)))

;; An individual with three copies of chromosome 22
(defclass Trisomy22
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some derivedFrom k46_XN)
   (e/addition 1 h/HumanChromosome22)))

;; Named Karyotypes that are caused by structural abnormalities

;; An individual with loss of part of the short arm of chromosome 1
;; (defclass DeletionSyndrome1p36
;;   :subclass NamedKaryotype
;;   :equivalent
;;   (owl-and
;;    (owl-some derivedFrom k46_XN)
;;    (owl-some e/hasEvent
;;             (owl-and e/Deletion
;;                     (owl-some e/hasBreakPoint h/HumanChromosome1Bandp)))))

;; ;; An individual with the partial deletion of chromosomal material of
;; ;; the short arm of chromosome 4 ;;aka del(4p16.3) ;;WHS, Chromosome
;; ;; Deletion Dillian 4p Syndrome, Pitt-Rogers-Danks Syndrome, PRDS,
;; ;; Pitt Syndrome
;; (defclass WolfHirschhornSydrome
;;   :subclass NamedKaryotype
;;   :equivalent
;;   (owl-and
;;    (owl-some derivedFrom k46_XN)
;;    (owl-some e/hasEvent
;;             (owl-and e/Deletion
;;                     (owl-some e/hasBreakPoint h/HumanChromosome4Bandp)))))

;; ;; TODO An individual with a truncated short arm on chromosome 5 ;;aka cry of the cat
;; (defclass CriDuChat
;;   :subclass NamedKaryotype)

;; ;; TODO An individual with ... chromosome 5
;; (defclass DeletionSyndrome5q
;;   :subclass NamedKaryotype)

;; ;; TODO An individual with ... chromosome 7
;; (defclass WilliamsSyndrome
;;   :subclass NamedKaryotype)

;; ;; TODO An individual with ... chromosome 11
;; (defclass JacobsenSyndrome
;;   :subclass NamedKaryotype)

;; ;; TODO An individual with a segment of the long arm of chromosome 15 missing
;; (defclass AngelmanSyndrome
;;   :subclass NamedKaryotype)

;; ;; TODO An individual with a segment of the long arm of chromosome 15 missing
;; (defclass PraderWilliSyndrome
;;   :subclass NamedKaryotype)

;; ;; TODO An individual with ... chromosome 17
;; (defclass MillerDiekerSyndrome
;;   :subclass NamedKaryotype)

;; ;; TODO An individual with ... chromosome 17
;; (defclass SmithMagenisSyndrome
;;   :subclass NamedKaryotype)

;; ;; TODO An individual with ... chromosome 18
;; (defclass DeletionSyndrome18q
;;   :subclass NamedKaryotype)

;; ;; TODO An individual with ... chromosome 22
;; (defclass DiGeorgeSyndrome
;;   :subclass NamedKaryotype)

;; ;; TODO An individual with ... chromosome 22
;; (defclass CatEyeSyndrome
;;   :subclass NamedKaryotype)

;; example defined classes

;; Ploidy descriptions

(defclass HaploidKaryotype
  :equivalent
  (owl-or k23_N
         (owl-some derivedFrom k23_N)))

(defclass DiploidKaryotype
  :equivalent
  (owl-or k46_XN
         (owl-some derivedFrom k46_XN)))

(defclass TriploidKaryotype
  :equivalent
  (owl-or k69_XNN
         (owl-some derivedFrom k69_XNN)))

(defclass TetraploidKaryotype
  :equivalent
  (owl-or k92_XNNN
         (owl-some derivedFrom k92_XNNN)))

;; Define gender - diploid only

;; male
(defclass MaleKaryotype
  :equivalent
  (owl-or k46_XY
         (owl-some derivedFrom k46_XY)))

;; female = those derived from 46,XX - as 46,XY is an asserted
;; subclass of 46,XN cannot be included in the definition - what
;; happens about 45,X?
(defclass FemaleKaryotype
  :equivalent
  (owl-or k46_XX
         (owl-some derivedFrom k46_XX)))

;; Define numerical abnormalities - in order for these to work, need
;; to import the axioms from h/human and e/events

(defclass NumericalAbnormalKaryotypeAutosomalGain
  :equivalent
  (owl-some e/hasEvent
           (owl-and e/Addition h/HumanAutosome)))

(defclass NumericalAbnormalKaryotypeAutosomalLoss
  :equivalent
  (owl-some e/hasEvent
           (owl-and e/Deletion h/HumanAutosome)))

(defclass NumericalAbnormalKaryotypeAutosomalGainOrLoss
  :equivalent
  (owl-or (owl-some e/hasEvent
                  (owl-and e/Addition h/HumanAutosome))
         (owl-some e/hasEvent
                  (owl-and e/Deletion h/HumanAutosome))))

(defclass NumericalAbnormalKaryotypeAllosomalGain
  :equivalent
  (owl-some e/hasEvent
           (owl-and e/Addition h/HumanSexChromosome)))

(defclass NumericalAbnormalKaryotypeAllosomalLoss
  :equivalent
  (owl-some e/hasEvent
           (owl-and e/Deletion h/HumanSexChromosome)))

(defclass NumericalAbnormalKaryotypeAllosomalGainOrLoss
  :equivalent
  (owl-or
   (owl-some e/hasEvent
            (owl-and e/Addition h/HumanSexChromosome))
   (owl-some e/hasEvent
            (owl-and e/Deletion h/HumanSexChromosome))))

(defclass NumericalAbnormalKaryotypeChromosomalGain
  :equivalent
  (owl-some e/hasEvent
           (owl-and e/Addition h/HumanChromosome)))

(defclass NumericalAbnormalKaryotypeChromosomalLoss
  :equivalent
  (owl-some e/hasEvent
           (owl-and e/Deletion h/HumanChromosome)))

;;aka aneuploidy
(defclass NumericalAbnormalKaryotype
  :equivalent
  (owl-or
   (owl-some e/hasEvent
            (owl-and e/Addition h/HumanChromosome))
   (owl-some e/hasEvent
            (owl-and e/Deletion h/HumanChromosome))))

(defclass DerivedNumericalAbnormalKaryotype
  :equivalent
  (owl-or
   (owl-some e/hasDerivedEvent
            (owl-and e/Addition h/HumanChromosome))
   (owl-some e/hasDerivedEvent
            (owl-and e/Deletion h/HumanChromosome))))


;; Define structural abnormalities - in order for these to work, need
;; to import the axioms from h/human and e/events

(defclass StructuralAbnormalKaryotypeAddition
  :equivalent
  (owl-some e/hasEvent
           (owl-and e/Addition
                   (owl-some e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotypeDeletion
  :equivalent
  (owl-some e/hasEvent
           (owl-and e/Deletion
                   (owl-some e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotypeDuplication
  :equivalent
  (owl-some e/hasEvent
           (owl-and e/Duplication
                   (owl-some e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotypeFission
  :equivalent
  (owl-some e/hasEvent
           (owl-and e/Fission
                   (owl-some e/hasBreakPoint
                            (owl-some k/isBandOf h/HumanCentromere)))))

(defclass StructuralAbnormalKaryotypeInsertion
  :equivalent
  (owl-some e/hasEvent
           (owl-and e/Insertion
                   (owl-some e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotypeInversion
  :equivalent
  (owl-some e/hasEvent
           (owl-and e/Inversion
                   (owl-some e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotypeQuadruplication
  :equivalent
  (owl-some e/hasEvent
           (owl-and e/Quadruplication
                   (owl-some e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotypeTranslocation
  :equivalent
  (owl-some e/hasEvent
           (owl-and e/Translocation
                   (owl-some e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotypeTriplication
  :equivalent
  (owl-some e/hasEvent
           (owl-and e/Triplication
                   (owl-some e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotype
  :equivalent
  (owl-some e/hasEvent
           (owl-and e/Event
                   (owl-some e/hasBreakPoint k/ChromosomeComponent))))

(defclass DerivedStructuralAbnormalKaryotype
  :equivalent
  (owl-some e/hasDerivedEvent
           (owl-and e/Event
                   (owl-some e/hasBreakPoint k/ChromosomeComponent))))
