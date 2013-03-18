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
(as-inverse
 (defoproperty derivedFrom
   :characteristics transitive
   ;; :range k/Karyotype
   :domain NamedKaryotype)
 
 (defoproperty derivedTo
   ;; :range NamedKaryotype
   :domain k/Karyotype)
 )

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

;; ;; TODO define all triploid base karyotypes
;; (defclass k69_NNN
;;   :label "The 69,N karyotype"
;;   :subclass BaseKaryotype)
;; ;; TODO define all tetraploid base karyotypes
;; (defclass k92_NNNN
;;   :label "The 92,N karyotype"
;;   :subclass BaseKaryotype)

;; Define the namedKaryotypes

;; Named Karyotypes that are caused by numerical abnormalities
;; Allosomal abnormalites

;; An (female) individual with only one sex chromosome = X ;;aka 45,X or 45,X0 ;;aka abnormal female
(defclass TurnerSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owland
   (owlsome derivedFrom k46_XN)
   (e/deletion 1 h/HumanAllosome)))

;; An (male) individual with an extra X chromosome ;;aka 47,XXY ;;aka abnormal male
(defclass KlinefelterSyndromeMostCommonVariation
  :subclass NamedKaryotype
  :equivalent
  (owland
   (owlsome derivedFrom k46_XY)
   (e/addition 1 h/HumanChromosomeX)
   (owlnot
    (owlsome e/hasEvent  
             (owland e/Addition h/HumanChromosomeY)))
   ))

;; An (male) individual with extra X chromosomes ;;aka abnormal male
(defclass KlinefelterSyndromeAllVariations 
  :subclass NamedKaryotype
  :equivalent
  (owland
   (owlsome derivedFrom k46_XY)
   (owlsome e/hasEvent
            (owland e/Addition h/HumanChromosomeX))))

;; TODO An individual with no X chromosome
(defclass Lethal
  :subclass NamedKaryotype)

;; Autsomal abnormalities

;; An individual with three copies of chromosome 8 ;;aka Trisomy8
(defclass Warkany2Syndrome
  :subclass NamedKaryotype
  :equivalent
  (owland
   (owlsome derivedFrom k46_XN)
   (e/addition 1 h/HumanChromosome8)))

;; An individual with three copies of chromosome 9
(defclass Trisomy9
  :subclass NamedKaryotype
  :equivalent
  (owland
   (owlsome derivedFrom k46_XN)
   (e/addition 1 h/HumanChromosome9)))

;; An individual with three copies of chromosome 13 ;;aka Trisomy13
(defclass PatauSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owland
   (owlsome derivedFrom k46_XN)
   (e/addition 1 h/HumanChromosome13)))

;; An individual with three copies of chromosome 16
(defclass Trisomy16
  :subclass NamedKaryotype
  :equivalent
  (owland
   (owlsome derivedFrom k46_XN)
   (e/addition 1 h/HumanChromosome16)))
  
;; An individual with three copies of chromosome 18 ;;aka Trisomy18
(defclass EdwardsSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owland
   (owlsome derivedFrom k46_XN)
   (e/addition 1 h/HumanChromosome18)))

;; An individual with three copies of chromosome 21 ;;aka Trisomy21
(defclass DownSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owland
   (owlsome derivedFrom k46_XN)
   (e/addition 1 h/HumanChromosome21)))

;; An individual with three copies of chromosome 22
(defclass Trisomy22
  :subclass NamedKaryotype
  :equivalent
  (owland
   (owlsome derivedFrom k46_XN)
   (e/addition 1 h/HumanChromosome22)))

;; Named Karyotypes that are caused by structural abnormalities

;; TODO An individual with loss of part of the short arm of chromosome 1
(defclass DeletionSyndrome1p36
  :subclass NamedKaryotype)

;; TODO An individual with the partial deletion of chromosomal material of the short arm of chromosome 4 ;;aka del(4p16.3) ;;WHS, Chromosome Deletion Dillian 4p Syndrome, Pitt-Rogers-Danks Syndrome, PRDS, Pitt Syndrome
(defclass WolfHirschhornSydrome
  :subclass NamedKaryotype)

;; (defclass WolfHirschhornSydrome
;;   :subclass NamedKaryotype
;;   :equivalent
;;   (owland
;;    (owlsome derivedFrom k46_XN)
;;    (exactly 1 e/hasEvent
;;             (owland e/Deletion
;;                     (exactly 2 e/hasBreakPoint HumanChromosome4Band4p16.3)))))
  
;; TODO An individual with a truncated short arm on chromosome 5 ;;aka cry of the cat
(defclass CriDuChat
  :subclass NamedKaryotype)

;; TODO An individual with ... chromosome 5
(defclass DeletionSyndrome5q
  :subclass NamedKaryotype)

;; TODO An individual with ... chromosome 7
(defclass WilliamsSyndrome
  :subclass NamedKaryotype)

;; TODO An individual with ... chromosome 11
(defclass JacobsenSyndrome
  :subclass NamedKaryotype)

;; TODO An individual with a segment of the long arm of chromosome 15 missing
(defclass AngelmanSyndrome
  :subclass NamedKaryotype)

;; TODO An individual with a segment of the long arm of chromosome 15 missing
(defclass PraderWilliSyndrome
  :subclass NamedKaryotype)

;; TODO An individual with ... chromosome 17
(defclass MillerDiekerSyndrome
  :subclass NamedKaryotype)

;; TODO An individual with ... chromosome 17
(defclass SmithMagenisSyndrome
  :subclass NamedKaryotype)

;; TODO An individual with ... chromosome 18
(defclass DeletionSyndrome18q
  :subclass NamedKaryotype)

;; TODO An individual with ... chromosome 22
(defclass DiGeorgeSyndrome
  :subclass NamedKaryotype)

;; TODO An individual with ... chromosome 22
(defclass CatEyeSyndrome
  :subclass NamedKaryotype)

;; example defined classes

;; Ploidy descriptions

(defclass HaploidKaryotype
  :equivalent
  (owlor k23_N
         (owlsome derivedFrom k23_N)))

(defclass DiploidKaryotype
  :equivalent
  (owlor k46_XN
         (owlsome derivedFrom k46_XN)))

;; TODO Ploidy description for triploid and tetraploid karyotypes when the base karyotypes have been implemented
;; (defclass TriploidKaryotype
;;   :equivalent
;;   (owlor k69_NNN
;;          (owlsome derivedFrom k69_NNN)))

;; (defclass TetraploidKaryotype
;;   :equivalent
;;   (owlor k92_NNNN
;;          (owlsome derivedFrom k92_NNNN)))

;; Define gender

;; TODO Description of Female/Male karyotypes for haploid, triploid, and tetraploid?

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

;; Define numerical abnormalities - in order for these to work, need to import the axioms from h/human and e/events

(defclass NumericalAbnormalKaryotypeAutosomalGain
  :equivalent
  (owlsome e/hasEvent
           (owland e/Addition h/HumanAutosome)))

(defclass NumericalAbnormalKaryotypeAutosomalLoss
  :equivalent
  (owlsome e/hasEvent
           (owland e/Deletion h/HumanAutosome)))

(defclass NumericalAbnormalKaryotypeAutosomalGainOrLoss
  :equivalent
  (owlor (owlsome e/hasEvent
                  (owland e/Addition h/HumanAutosome))
         (owlsome e/hasEvent
                  (owland e/Deletion h/HumanAutosome))))

(defclass NumericalAbnormalKaryotypeAllosomalGain
  :equivalent
  (owlsome e/hasEvent
           (owland e/Addition h/HumanAllosome)))

(defclass NumericalAbnormalKaryotypeAllosomalLoss
  :equivalent
  (owlsome e/hasEvent
           (owland e/Deletion h/HumanAllosome)))

(defclass NumericalAbnormalKaryotypeAllosomalGainOrLoss
  :equivalent
  (owlor
   (owlsome e/hasEvent
            (owland e/Addition h/HumanAllosome))
   (owlsome e/hasEvent
            (owland e/Deletion h/HumanAllosome))))

(defclass NumericalAbnormalKaryotypeChromosomalGain
  :equivalent
  (owlsome e/hasEvent
           (owland e/Addition h/HumanChromosome)))

(defclass NumericalAbnormalKaryotypeChromosomalLoss
  :equivalent
  (owlsome e/hasEvent
           (owland e/Deletion h/HumanChromosome)))

;;aka aneuploidy
(defclass NumericalAbnormalKaryotype
  :equivalent
  (owlor
   (owlsome e/hasEvent
            (owland e/Addition h/HumanChromosome))
   (owlsome e/hasEvent
            (owland e/Deletion h/HumanChromosome))))

;; Define structural abnormalities - in order for these to work, need to import the axioms from h/human and e/events

;; TODO: consider if they are h/HumanChromosomeBand, h/HumanChromosomeCentromere, h/HumanChromosomeComponent, etc...

(defclass StructuralAbnormalKaryotypeAddition
  :equivalent
  (owlsome e/hasEvent
           (owland e/Addition
                   (owlsome e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotypeDeletion
  :equivalent
  (owlsome e/hasEvent
           (owland e/Deletion
                   (owlsome e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotypeDuplication
  :equivalent
  (owlsome e/hasEvent
           (owland e/Duplication
                   (owlsome e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotypeFission
  :equivalent
  (owlsome e/hasEvent
           (owland e/Fission
                   (owlsome e/hasBreakPoint h/HumanCentromere))))

(defclass StructuralAbnormalKaryotypeInsertion
  :equivalent
  (owlsome e/hasEvent
           (owland e/Insertion
                   (owlsome e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotypeInversion
  :equivalent
  (owlsome e/hasEvent
           (owland e/Inversion
                   (owlsome e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotypeQuadruplication
  :equivalent
  (owlsome e/hasEvent
           (owland e/Quadruplication
                   (owlsome e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotypeTranslocation
  :equivalent
  (owlsome e/hasEvent
           (owland e/Translocation
                   (owlsome e/hasBreakPoint k/ChromosomeComponent))))

(defclass StructuralAbnormalKaryotypeTriplication
  :equivalent
  (owlsome e/hasEvent
           (owland e/Triplication
                   (owlsome e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotype
  :equivalent
  (owlsome e/hasEvent
           (owland e/Event
                   (owlsome e/hasBreakPoint k/ChromosomeComponent))))
