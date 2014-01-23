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
            [ncl.karyotype [events :as e]]
            [ncl.karyotype [base :as b]]))

(defontology named
  :iri "http://ncl.ac.uk/karyotype/named"
  :prefix "nmd:")

;; import all ncl.karyotype axioms
(owl-import k/karyotype)
(owl-import h/human)
(owl-import e/events)
(owl-import b/base)

(defclass NamedKaryotype
  :subclass k/Karyotype)

;; Define the namedKaryotypes

;; Named Karyotypes that are caused by numerical abnormalities
;; Allosomal abnormalites

;; An (female) individual with only one sex chromosome = X ;;aka 45,X
;; or 45,X0 ;; aka abnormal female
(defclass TurnerSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/deletion 1 h/HumanSexChromosome)))

;; An (male) individual with an extra X chromosome ;; aka 47,XXY ;;
;; aka abnormal male
(defclass KlinefelterSyndromeMostCommonVariation
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XY)
   (e/addition 1 h/HumanChromosomeX)
   (owl-not
    (owl-some e/hasEvent
             (owl-and e/Addition h/HumanChromosomeY)))))

;; An (male) individual with extra X chromosomes ;;aka abnormal male
(defclass KlinefelterSyndromeAllVariations
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XY)
   (e/event nil (e/addition-chromosome h/HumanChromosomeX))))

;; #TODO An individual with no X chromosome
;; (defclass Lethal
;;   :subclass NamedKaryotype)

;; Autsomal abnormalities

;; An individual with three copies of chromosome 8 ;;aka Trisomy8
(defclass Warkany2Syndrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/addition 1 h/HumanChromosome8)))

;; An individual with three copies of chromosome 9
(defclass Trisomy9
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/addition 1 h/HumanChromosome9)))

;; An individual with three copies of chromosome 13 ;;aka Trisomy13
(defclass PatauSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/addition 1 h/HumanChromosome13)))

;; An individual with three copies of chromosome 16
(defclass Trisomy16
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/addition 1 h/HumanChromosome16)))

;; An individual with three copies of chromosome 18 ;;aka Trisomy18
(defclass EdwardsSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/addition 1 h/HumanChromosome18)))

;; An individual with three copies of chromosome 21 ;;aka Trisomy21
(defclass DownSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/addition 1 h/HumanChromosome21)))

;; An individual with three copies of chromosome 22
(defclass Trisomy22
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/addition 1 h/HumanChromosome22)))

;; Named Karyotypes that are caused by structural abnormalities

;; An individual with loss of part of the short arm of chromosome 1
;; (defclass DeletionSyndrome1p36
;;   :subclass NamedKaryotype
;;   :equivalent
;;   (owl-and
;;    (owl-some b/derivedFrom b/k46_XN)
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
;;    (owl-some b/derivedFrom b/k46_XN)
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
  (owl-or b/k23_N
         (owl-some b/derivedFrom b/k23_N)))

(defclass DiploidKaryotype
  :equivalent
  (owl-or b/k46_XN
         (owl-some b/derivedFrom b/k46_XN)))

(defclass TriploidKaryotype
  :equivalent
  (owl-or b/k69_XNN
         (owl-some b/derivedFrom b/k69_XNN)))

(defclass TetraploidKaryotype
  :equivalent
  (owl-or b/k92_XNNN
         (owl-some b/derivedFrom b/k92_XNNN)))

;; Define gender - diploid only

;; male
(defclass MaleKaryotype
  :equivalent
  (owl-or b/k46_XY
         (owl-some b/derivedFrom b/k46_XY)))

;; female = those derived from 46,XX - as 46,XY is an asserted
;; subclass of 46,XN cannot be included in the definition - what
;; happens about 45,X?
(defclass FemaleKaryotype
  :equivalent
  (owl-or b/k46_XX
         (owl-some b/derivedFrom b/k46_XX)))

;; Define numerical abnormalities - in order for these to work, need
;; to import the axioms from h/human and e/events

(defclass NumericalAbnormalKaryotypeAutosomalGain
  :equivalent
  (owl-some e/hasEvent
            (e/addition-chromosome h/HumanAutosome)))

  ;; DOES WORK HOWEVER IN THIS CASE WE WANT HASEVENT NOT HASDIRECTEVENT
  ;; (e/event nil h/HumanAutosome))

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
