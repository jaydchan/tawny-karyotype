;; \chapter{Querying karyotypes}
;; \label{chap:named}

;; %% The contents of this file are subject to the LGPL License, Version 3.0.

;; %% Copyright (C) 2012-2015, Newcastle University

;; %% This program is free software: you can redistribute it and/or modify
;; %% it under the terms of the GNU General Public License as published by
;; %% the Free Software Foundation, either version 3 of the License, or
;; %% (at your option) any later version.

;; %% This program is distributed in the hope that it will be useful,
;; %% but WITHOUT ANY WARRANTY; without even the implied warranty of
;; %% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; %% GNU General Public License for more details.

;; %% You should have received a copy of the GNU General Public License
;; %% along with this program. If not, see http://www.gnu.org/licenses/.

;; \begin{code}

(ns ^{:doc "TODO"
      :author "Jennifer Warrender"}
  ncl.karyotype.named
  (:use [tawny.owl])
  (:require [ncl.karyotype
             [karyotype :as k]
             [human :as h]
             [events :as e]
             [base :as b]]))

(defontology named
  :iri "http://www.purl.org/captau/karyotype/named"
  :prefix "nmd:"
  :comment "TODO")

;; import all ncl.karyotype axioms
(owl-import k/karyotype)
(owl-import h/human)
(owl-import e/events)
(owl-import b/base)

(defclass NamedKaryotype
  :subclass k/Karyotype)

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
  (e/event nil
           (e/addition-chromosome h/HumanAutosome)))

(defclass NumericalAbnormalKaryotypeAutosomalLoss
  :equivalent
  (e/event nil
           (e/deletion-chromosome h/HumanAutosome)))

(defclass NumericalAbnormalKaryotypeAutosomalGainOrLoss
  :equivalent
  (owl-or
   (e/event nil
            (e/addition-chromosome h/HumanAutosome))
   (e/event nil
            (e/deletion-chromosome h/HumanAutosome))))

(defclass NumericalAbnormalKaryotypeAllosomalGain
  :equivalent
   (e/event nil
            (e/addition-chromosome h/HumanSexChromosome)))

(defclass NumericalAbnormalKaryotypeAllosomalLoss
  :equivalent
   (e/event nil
            (e/deletion-chromosome h/HumanSexChromosome)))

(defclass NumericalAbnormalKaryotypeAllosomalGainOrLoss
  :equivalent
  (owl-or
   (e/event nil
            (e/addition-chromosome h/HumanSexChromosome))
   (e/event nil
            (e/deletion-chromosome h/HumanSexChromosome))))

(defclass NumericalAbnormalKaryotypeChromosomalGain
  :equivalent
  (e/event nil
           (e/addition-chromosome h/HumanChromosome)))

(defclass NumericalAbnormalKaryotypeChromosomalLoss
  :equivalent
  (e/event nil
           (e/deletion-chromosome h/HumanChromosome)))

;;aka aneuploidy
(defclass NumericalAbnormalKaryotype
  :equivalent
  (owl-or
   (e/event nil
            (e/addition-chromosome h/HumanChromosome))
   (e/event nil
            (e/deletion-chromosome h/HumanChromosome))))

(defclass DerivedNumericalAbnormalKaryotype
  :equivalent
  (owl-or
   (owl-some e/hasDerivedEvent
             (e/addition-chromosome h/HumanChromosome))
   (owl-some e/hasDerivedEvent
             (e/addition-chromosome h/HumanChromosome))))


;; Define structural abnormalities - in order for these to work, need
;; to import the axioms from h/human and e/events

(defclass StructuralAbnormalKaryotypeAddition
  :equivalent
  (e/event nil
           (e/addition-band h/HumanChromosomeBand)))

(defclass StructuralAbnormalKaryotypeDeletion
  :equivalent
  (e/event nil
           (e/deletion-band h/HumanChromosomeBand
                            h/HumanChromosomeBand)))

(defclass StructuralAbnormalKaryotypeDuplication
  :equivalent
  (e/event nil
           (e/duplication-pattern e/Duplication
                                  h/HumanChromosomeBand
                                  h/HumanChromosomeBand)))

(defclass StructuralAbnormalKaryotypeInverseDuplication
  :equivalent
  (e/event nil
           (owl-and e/Duplication
                    (owl-some e/hasBreakPoint
                              h/HumanChromosomeBand
                              h/HumanChromosomeBand)
                    (e/event nil (owl-and e/Inversion
                                          (owl-some e/hasBreakPoint
                                                    h/HumanChromosomeBand
                                                    h/HumanChromosomeBand))))))

(defclass StructuralAbnormalKaryotypeFission
  :equivalent
  (e/event nil
           (owl-and e/Fission
                    (owl-some e/hasBreakPoint
                              (owl-some k/isBandOf h/HumanCentromere)))))

(defclass StructuralAbnormalKaryotypeInsertion
  :equivalent
  (e/event nil
           (e/insertion-pattern e/Insertion
                                h/HumanChromosomeBand
                                h/HumanChromosomeBand
                                h/HumanChromosomeBand)))

(defclass StructuralAbnormalKaryotypeInversion
  :equivalent
  (e/event nil
           (owl-and e/Inversion
                    (owl-some e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotypeQuadruplication
  :equivalent
  (e/event nil
           (owl-and e/Quadruplication
                    (owl-some e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotypeTranslocation
  :equivalent
  (e/event nil
           (owl-and e/Translocation
                    (owl-some e/hasBreakPoint h/HumanChromosomeBand))))

(defclass StructuralAbnormalKaryotypeTriplication
  :equivalent
  (e/event nil
           (e/triplication-pattern e/Triplication
                                   h/HumanChromosomeBand
                                   h/HumanChromosomeBand)))

(defclass StructuralAbnormalKaryotype
  :equivalent
  (e/event nil
           (owl-and e/Event
                    (owl-some e/hasBreakPoint k/ChromosomeComponent))))

(defclass DerivedStructuralAbnormalKaryotype
  :equivalent
  (owl-some e/hasDerivedEvent
            (owl-and e/Event
                     (owl-some e/hasBreakPoint k/ChromosomeComponent))))


;; Define the namedKaryotypes

;; Named Karyotypes that are caused by numerical abnormalities
;; Allosomal abnormalites

;; An (female) individual with only one sex chromosome = X ;;aka 45,X0
;; or 45,X ;; aka abnormal female
;; REFINE without derived
(defclass TurnerSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/deletion 1 h/HumanSexChromosome)
   (owl-not
    (e/event nil (e/addition-chromosome h/HumanSexChromosome)))))

;; An (male) individual with an extra X chromosome ;; aka 47,XXY ;;
;; aka abnormal male
;; REFINE without derived
(defclass KlinefelterSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XY)
   (e/addition 1 h/HumanChromosomeX)
   (owl-not
    (e/event nil (e/addition-chromosome h/HumanChromosomeY)))
))

;; An (male) individual with extra X chromosomes ;;aka abnormal male
;; aka 47,XXY, 48,XXYY, 48,XXXY, 49,XXXYY, 49,XXXXY
(defclass MaleWithExtraX
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XY)
   (e/event nil (e/addition-chromosome h/HumanChromosomeX))
   (owl-not
    (e/event nil (e/deletion-chromosome h/HumanChromosomeX)))
))

;; An (male) individual with extra Y chromosomes ;; aka 47,XYY,
;; 48,XYYY, 49,XYYYY
(defclass MaleWithExtraY
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XY)
   (e/event nil (e/addition-chromosome h/HumanChromosomeY))
   (owl-not
    (e/event nil (e/deletion-chromosome h/HumanChromosomeY)))
))

;; An individual with 3 X chromosomes ;;aka 47,XXX
;; REFINE without derived
(defclass TripleXSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XX)
   (e/addition 1 h/HumanChromosomeX)
   (owl-not
    (e/event nil (e/deletion-chromosome h/HumanChromosomeX)))
))

;; An (female) individual with extra X chromosomes ;;aka 47,XXX,
;; 48,XXXX, 49,XXXXX
(defclass FemaleWithExtraX
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XX)
   (e/event nil (e/addition-chromosome h/HumanChromosomeX))
   (owl-not
    (e/event nil (e/deletion-chromosome h/HumanChromosomeX)))
))

;; An individual with no X chromosome
;; REFINE without derived
(defclass Lethal
  :subclass NamedKaryotype
  :equivalent
  (owl-or
   (owl-and
    (owl-some b/derivedFrom b/k46_XX)
    (e/deletion 2 h/HumanSexChromosome)
    (e/event nil (e/addition-chromosome h/HumanChromosomeX))
    )
   (owl-and
    (owl-some b/derivedFrom b/k46_XY)
    (e/event nil (e/deletion-chromosome h/HumanChromosomeX))
    (e/event nil (e/addition-chromosome h/HumanChromosomeX))
)))

;; Autsomal abnormalities
;; REFINE without derived

;; An individual with three copies of chromosome 8 ;;aka Trisomy8
(defclass WarkanySyndrome2
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/addition 1 h/HumanChromosome8)
   (owl-not (e/event nil (e/deletion-chromosome h/HumanChromosome8)))
))

;; An individual with three copies of chromosome 9
(defclass Trisomy9
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/addition 1 h/HumanChromosome9)
   (owl-not (e/event nil (e/deletion-chromosome h/HumanChromosome9)))
))

;; An individual with three copies of chromosome 13 ;;aka Trisomy13
(defclass PatauSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/addition 1 h/HumanChromosome13)
   (owl-not (e/event nil (e/deletion-chromosome h/HumanChromosome13)))
))

;; An individual with three copies of chromosome 16
(defclass Trisomy16
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/addition 1 h/HumanChromosome16)
   (owl-not (e/event nil (e/deletion-chromosome h/HumanChromosome16)))
))

;; An individual with three copies of chromosome 18 ;;aka Trisomy18
(defclass EdwardsSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/addition 1 h/HumanChromosome18)
   (owl-not (e/event nil (e/deletion-chromosome h/HumanChromosome18)))
))

;; An individual with three copies of chromosome 21 ;;aka Trisomy21
(defclass DownSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/addition 1 h/HumanChromosome21)
   (owl-not (e/event nil (e/deletion-chromosome h/HumanChromosome21)))
))

;; An individual with three copies of chromosome 22 ;;aka Trisomy 22
(defclass CatEyeSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/addition 1 h/HumanChromosome22)
   (owl-not (e/event nil (e/deletion-chromosome h/HumanChromosome22)))
))

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

;; An individual with the partial deletion of chromosomal material of
;; the short arm of chromosome 4 ;;aka del(4p16.3) ;;WHS, Chromosome
;; Deletion Dillian 4p Syndrome, Pitt-Rogers-Danks Syndrome, PRDS,
;; Pitt Syndrome
;; Terminal deletion
(defclass WolfHirschhornSydrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/event nil (e/deletion-band h/HumanChromosome4Bandp
                                 h/HumanChromosome4BandpTer))))

;; An individual with a truncated short arm on chromosome 5 ;;aka cry
;; of the cat ;;Chromosome 5p Feletion Syndrome
(defclass CriDuChat
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/event nil (e/deletion-band h/HumanChromosome5Bandp
                                 h/HumanChromosome5BandpTer))))

;; An individual with chromosome 5 monosomy.
(defclass Chromosome5qDeletionSyndrome
  :subclass NamedKaryotype
  :equivalent
  (owl-and
   (owl-some b/derivedFrom b/k46_XN)
   (e/event nil (e/deletion-band h/HumanChromosome5Bandq
                                 h/HumanChromosome5BandqTer))))

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
;; (defclass Chromosome18qDeletionSyndrome
;;   :subclass NamedKaryotype)

;; ;; TODO An individual with ... chromosome 22
;; (defclass DiGeorgeSyndrome
;;   :subclass NamedKaryotype)

;; \end{code}

;; %%% Local Variables:
;; %%% TeX-master: "master.tex"
;; %%% End:
