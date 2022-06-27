;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2012-2015, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see http://www.gnu.org/licenses/.

(ns ^{:doc "Defining example karyotypes from the ISCN2013."
      :author "Jennifer Warrender"}
  ncl.karyotype.iscnexamples
  (:use [tawny.owl])
  (:require [ncl.karyotype
             [karyotype :as k]
             [human :as h]
             [events :as e]
             [features :as f]
             [base :as b]
             [named :as n]
             [generic :as g :only [tk-iri]]]))

(defontology iscnexamples
  :iri (clojure.core/str g/tk-iri "iscnexamples")
  :prefix "iex:"
  :comment "ISCN Example Karyotypes ontology for Human Karyotype
  Ontology, written using the tawny-owl library.")

;; import all ncl.karyotype axioms
(owl-import k/karyotype)
(owl-import h/human)
(owl-import e/events)
(owl-import f/features)
(owl-import b/base)
(owl-import n/named)

(defclass ISCNExampleKaryotype
  :subclass k/Karyotype)

(as-disjoint
;; Chapter 8 -> Numerical Chromosome Abnormalities

;; example ploidy karyotypes
;; example haploid karyotype
(defclass k26_X_+4_+6_+21
  :label "The 26,X,+4,+6,+21 karyotype"
  :comment "ISCN2009 pg 55 -> 'A near-haploid karyotype with two copies
  of chromosomes 4, 6, and 21, and a single copy of all other
  chromosomes.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k23_X)
  (e/addition 1 h/HumanChromosome4)
  (e/addition 1 h/HumanChromosome6)
  (e/addition 1 h/HumanChromosome21))
;; example triploid karyotype
(defclass k71_XXX_+8_+10
  :label "The 71,XXX,+8,+10 karyotype"
  :comment "ISCN2009 pg 55 -> 'A near-triploid karyotype with four
  copies of chromosomes 8 and 10, and three copies of all other
  chromosomes.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k69_XXX)
  (e/addition 1 h/HumanChromosome8)
  (e/addition 1 h/HumanChromosome10))
;; example tetraploid karyotype
(defclass k89_XXYY_-1_-3_-5_+8_-21
  :label "The 89,XXYY,-1,-3,-5,+8,-21 karyotype"
  :comment "ISCN2009 pg 55 -> 'A near-tetraploid karyotype with three
  copies of chromosomes 1, 3, 5, and 21, five copies of chromosome 8,
  and four copies of all other autosomes.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k92_XXYY)
  (e/addition 1 h/HumanChromosome8)
  (e/deletion 1 h/HumanChromosome1)
  (e/deletion 1 h/HumanChromosome3)
  (e/deletion 1 h/HumanChromosome5)
  (e/deletion 1 h/HumanChromosome21))

;; NEW TESTING

;; a test about direct-superclasses
;; (direct-superclasses  k89_XXYY_-1_-3_-5_+8_-21)
;; works as expected

;; a test about restrictions function
;;  (let [parents (direct-superclasses k89_XXYY_-1_-3_-5_+8_-21)
;;         restrictions (filter
;;                       #(instance?
;;                         org.semanticweb.owlapi.model.OWLRestriction %) parents)
;;        _(println "start")
;;        ;;_ (println parents)
;;        _ (println restrictions)
;;        _(println "end")
;;        ])
;; works as expected

;; mos 47,XY,+21[12]/46,XY[18]
;; 76~102<4n>,XXXX,...
;; 58<2n>,XY,+X,+4,+6,+8,+10,+11,+14,+14,+17,+18,+21,+21[10]

;; example constitutional sex chromosome abnormalities
(defclass k45_X
  :label "The 45,X karyotype"
  :comment "ISCN2009 pg 56 -> 'A karyotype with one X
  chromosome (Turner syndrome).'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XN)
            (e/deletion 1 h/HumanSexChromosome))))

(defclass k47_XXY
  :label "The 47,XXY karyotype"
  :comment "ISCN2009 pg 56 -> 'A karyotype with two X
  chromosomes and one Y chromosome (Klinefelter syndrome).'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/addition 1 h/HumanChromosomeX))))

(defclass k47_XXX
  :label "The 47,XXX karyotype"
  :comment "ISCN2009 pg 56 -> 'A karyotype with three X
  chromosomes.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XX)
            (e/addition 1 h/HumanChromosomeX))))

(defclass k47_XYY
  :label "The 47,XYY karyotype"
  :comment "ISCN2009 pg 56 -> 'A karyotype with one X
  chromosome and two Y chromosomes.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/addition 1 h/HumanChromosomeY))))

(defclass k48_XXXY
  :label "The 48,XXXY karyotype"
  :comment "ISCN2009 pg 56 -> 'A karyotype with three X
  chromosomes and one Y chromosome.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/addition 1 h/HumanChromosomeX))))

;; mos 47,XXY[10]/46,XY[20]
;; mos 45,X[25]/47,XXX[12]/46,XX[13]
;; mos 47,XXX[25]/45,X[12]/46,XX[13]

;; example acquired sex chromosome abnormalities
(defclass k47_XX_+X
  :label "The 47,XX,+X karyotype"
  :comment "ISCN2009 pg 56 -> 'A tumor karyotype in a female with an
  additional X chromosome.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 1 h/HumanChromosomeX))
(defclass k45_X_-X
  :label "The 45,X,-X karyotype"
  :comment "ISCN2009 pg 56 -> 'A tumor karyotype in a female with loss
  of one X chromosome.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/deletion 1 h/HumanChromosomeX))
(defclass k45_X_-Y
  :label "The 45,X,-Y karyotype"
  :comment "ISCN2009 pg 56 -> 'A tumor karyotype in a male with loss
  of the Y chromosome.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/deletion 1 h/HumanChromosomeY))
(defclass k45_Y_-X
  :label "The 45,Y,-X karyotype"
  :comment "ISCN2009 pg 57 -> 'A tumor karyotype in a male with loss
  of the X chromosome.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/deletion 1 h/HumanChromosomeX))
(defclass k48_XY_+X_+Y
  :label "The 48,XY,+X,+Y karyotype"
  :comment "ISCN2009 pg 57 -> 'A tumor karyotype in a male with one
  additional X and one additional Y chromosome.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/addition 1 h/HumanChromosomeX)
  (e/addition 1 h/HumanChromosomeY))

;; example acquired chromosome abnormalities with a constitutional sex
;; chromosome abnormalities
(defclass k48_XXYc_+X
  :label "The 48,XXYc,+X karyotype"
  :comment "ISCN2009 pg 57 -> 'Tumor cells with an acquired additional
  X chromosome in a patient with Klinefelter syndrome.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/addition 1 h/HumanChromosomeX))) ;;aka 47,XXY
  (e/addition 1 h/HumanChromosomeX))
(defclass k46_Xc_+X
  :label "The 46,Xc,+X karyotype"
  :comment "ISCN2009 pg 57 -> 'Tumor cells with an acquired additional
  X chromosome in a patient with Turner syndrome.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XN)
            (e/deletion 1 h/HumanAutosome))) ;;aka 45,X
  (e/addition 1 h/HumanChromosomeX))
(defclass k46_XXYc_-X
  :label "The 46,XXYc,-X karyotype"
  :comment "ISCN2009 pg 57 -> 'Tumor cells with an acquired loss of
  one X chromosome in a patient with Klinefelter syndrome.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/addition 1 h/HumanChromosomeX))) ;;aka 47,XXY
  (e/deletion 1 h/HumanChromosomeX))
(defclass k44_Xc_-X
  :label "The 44,Xc,-X karyotype"
  :comment "ISCN2009 pg 57 -> 'Tumor cells with an acquired loss of
  the X chromosome in a patient with Turner syndrome.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XN)
            (e/deletion 1 h/HumanSexChromosome))) ;;aka 45,X
  (e/deletion 1 h/HumanChromosomeX))
(defclass k46_Xc_+21
  :label "The 46,Xc,+21 karyotype"
  :comment "ISCN2009 pg 57 -> 'Tumor cells with an acquired eaxtra
  chromosome 21 in a patient with Turner syndrome.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XN)
            (e/deletion 1 h/HumanSexChromosome))) ;;aka 45,X
  (e/addition 1 h/HumanChromosome21))

;; TODO How do I model this?
;; (defclass k47_XXX?c
;;   :label "The  47,XXX?c karyotype"
;;   :comment "ISCN2009 pg 57 -> 'Tumor cells with an uncertain karyotype
;;   with an extra X chromosome. The question mark indicates that it is
;;   unclear the extra X is constitutional or acquired.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom #TODO))

;; (defclass k48_XXY_marc
;;   :label "The 48,XXY,mar c karyotype"
;;   :comment "ISCN2009 pg 57 -> 'For constitutional markers, there is a
;;   space between mar and c.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom
;;            (owl-and
;;             (owl-some b/derivedFrom b/k46_XY)
;;             (e/addition 1 h/HumanChromosomeX)
;;             (f/marker 1)))) ;;aka 48,XXY,mar

;; example acquired autosomal abnormalities
(defclass k47_XX_+21
  :label "The 47,XX,+21 karyotype"
  :comment "ISCN2009 pg 57 -> 'A karyotype with trisomy 21.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 1 h/HumanChromosome21))
(defclass k48_XX_+13_+21
  :label "The 48,XX,+13,+21 karyotype"
  :comment "ISCN2009 pg 57 -> 'A karyotype with trisomy 13 and trisomy
  21.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 1 h/HumanChromosome13)
  (e/addition 1 h/HumanChromosome21))
(defclass k45_XX_-22
  :label "The 45,XX,-22 karyotype"
  :comment "ISCN2009 pg 57 -> 'A karyotype with monosomy 22.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/deletion 1 h/HumanChromosome22))
(defclass k46_XX_+8_-21
  :label "The 46,XX,+8,-21 karyotype"
  :comment "ISCN2009 pg 58 -> 'A karyotype with trisomy 8 and monosomy
  21.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 1 h/HumanChromosome8)
  (e/deletion 1 h/HumanChromosome21))

;; example acquired chromosome abnormalities with a constitutional anomaly
(defclass k48_XY_+21c_+21
  :label "The 48,XY,+21c,+21 karyotype"
  :comment "ISCN2009 pg 58 -> 'An acquired extra chromosome 21 in a
  patient with Down syndrome.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/addition 1 h/HumanChromosome21))) ;;aka 47,XY,+21
  (e/addition 1 h/HumanChromosome21))
(defclass k46_XY_+21c_-21
  :label "The 46,XY,+21c,-21 karyotype"
  :comment "ISCN2009 pg 58 -> 'Acquired loss of one chromosome 21 in a
  patient with Down syndrome.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/addition 1 h/HumanChromosome21))) ;;aka 47,XY,+21
  (e/deletion 1 h/HumanChromosome21))


;; UNIPARENTAL DISOMY
;; 46,XY,upd(15)mat
;; mos 47,XX,+2[23]/46,XX,upd(21)pat[7]
;; 45,XY,upd(13)der(13;13)(q10;q10)pat


;; Chapter 9 -> Structural Chromosome Rearrangements
(defclass k69_XXX_del!7!!p11.2!
  :label "The 69,XXX,del(7)(p11.2) karyotype"
  :comment "ISCN2009 pg 60 -> 'Two normal chromosomes 7 and one with a
  deletion of the short arm.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k69_XXX)
  (e/deletion 1 h/HumanChromosome7Bandp11.2))

(defclass k69_XXY_del!7!!q22!_inv!7!!p13q22!_t!7_14!!p15_q11.1!
  :label "The  69,XXY,del(7)(q22),inv(7)(p13q22),t(7;14)(p15;q11.1) karyotype"
  :comment "ISCN2009 pg 60 -> 'No normal chromosome 7: one has a long
  arm deletion, one has an inversion, and one is involved in a balanced
  translocation with chromosome 14.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k69_XXY)
  (e/deletion 1 h/HumanChromosome7Bandq22)
  (e/inversion 1
               h/HumanChromosome7Bandp13
               h/HumanChromosome7Bandq22)
  (e/translocation 1
                   [h/HumanChromosome7Bandp15]
                   [h/HumanChromosome14Bandq11.1]))

;; REDEFINE
(defclass k70_XXX_+del!7!!p11.2!
  :label "The 70,XXX,+del(7)(p11.2) karyotype"
  :comment "ISCN2009 pg 60 -> 'Three normal chromosomes 7 and an
  additional structurally abnormal chromosome 7 with a deletion of the
  short arm.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k69_XXX)
  (e/addition 1 h/HumanChromosome7)
  (e/deletion 1 h/HumanChromosome7Bandp11.2))

(defclass k92_XXYY_del!7!!p11.2!_t!7_14!!p15_q11.1!
  :label "The 92,XXYY,del(7)(p11.2),t(7;14)(p15;q11.1) karyotype"
  :comment "ISCN2009 pg 60 -> 'Two normal and two abnormal chromosomes
  7: one has a deletion of the short arm, and one is involved in a
  balanced translocation with chromosome 14.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k92_XXYY)
  (e/deletion 1 h/HumanChromosome7Bandp11.2)
  (e/translocation 1
                   [h/HumanChromosome7Bandp15]
                   [h/HumanChromosome14Bandq11.1]))

(defclass k92_XXYY_del!7!!p11.2!_del!7!!q22!_del!7!!q34!
  :label "The 92,XXYY,del(7)(p11.2),del(7)(q22),del(7)(q34) karyotype"
  :comment "ISCN2009 pg 60 -> 'One normal chromosome 7 and three with
  different deletions'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k92_XXYY)
  (e/deletion 1 h/HumanChromosome7Bandp11.2)
  (e/deletion 1 h/HumanChromosome7Bandq22)
  (e/deletion 1 h/HumanChromosome7Bandq34))

;; 46,XX,inv(3)(q21q26.2) - karyotype defined later
;; 45,XX,dic(13;15)(q22;q24) - karyotype defined later

(defclass k46_Y_t!X_8!!p22.3_q24.1!
  :label "The 46,Y,t(X;8)(p22.3;q24.1) karyotype"
  :comment "ISCN2009 pg 60 -> 'Male karyotype showing a balanced
  translocation between the X chromosome and chromosome 8. Note that
  the normal sex chromosome, in this case a Y, is shown first.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/translocation 1
                   [h/HumanChromosomeXBandp22.3]
                   [h/HumanChromosome8Bandq24.1]))

(defclass k46_XY_der!1!t!1_3!!p22_q13.1!
  :label "The 46,XY,der(1)t(1;3)(p22;q13.1) karyotype"
  :comment "ISCN2009 pg 60 -> 'The der(1) chromosome replaces a normal
  chromosome 1 and there is no need to indicate the missing normal
  chromosome. It is obvious from the description that the karyotype
  contains one normal chromosome 1 and two normal chromosomes 3.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (f/derivative 1 [h/HumanChromosome10]
                (e/translocation nil
                                 [h/HumanChromosome1Bandp22]
                                 [h/HumanChromosome3Bandq13.1])))

(defclass k46_XX_ins!1_?!!p22_?!
  :label "The 46,XX,ins(1;?)(p22;?) karyotype"
  :comment "ISCN2009 pg 60 -> 'Material of unknown origin has been
  inserted at band p22 in one chromosome 1. The homologous chromosome
  1 is normal.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/insertion 1
               [h/HumanChromosome1Bandp22]
               [h/HumanChromosomeBand
                h/HumanChromosomeBand]))

(defclass k45_XY_-10_der!10!t!10_17!!q22_p12!
  :label "The 45,XY,-10,der(10)t(10;17)(q22;p12) karyotype"
  :comment "ISCN2009 pg 60 -> 'The der(10) replaces a normal
  chromosome 10; the homologous chromosome 10 is lost. In this
  situation the missing chromosome 10 must, of course, be indicated.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/deletion 1 h/HumanChromosome10)
  (f/derivative 1 [h/HumanChromosome10]
                (e/translocation nil
                                 [h/HumanChromosome10Bandq22]
                                 [h/HumanChromosome17Bandp12])))


;; ADDITIONAL MATERIAL OF UNKNOWN ORIGIN
(defclass k46_XX_add!19!!p13.3!
  :label "The 46,XX,add(19)(p13.3) karyotype"
  :comment "ISCN2009 pg 60 -> 'Additional material attached to band
  19p13.3, but neither the origin of the extra segment nor the type of
  arrangement is known.' 46,XX,add(19)(?::p13.3->qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 1 h/HumanChromosome19Bandp13.3))

(defclass k46_XY_add!12!!q13!
  :label "The 46,XY,add(12)(q13) karyotype"
  :comment "ISCN2009 pg 60 -> 'Additional material of unknown origin
  replaces the segment 12q13qter.' 46,XY,add(12)(pter->q13::?)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/addition 1 h/HumanChromosome12Bandq13))

(defclass k46_XX_der!5!add!5!!p15.3!add!5!!q23!
  :label "The 46,XX,der(5)add(5)(p15.3)add(5)(q23) karyotype"
  :comment "ISCN2009 pg 61 -> 'Additional material of unknown origin
  is attached at band 5p15.3 in the short arm and additional material
  replaces the seqment 5q23qter in the long arm.'
  46,XX,der(5)(?::p15.3->q23::?)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/derivative 1 [h/HumanChromosome5]
                (e/addition nil h/HumanChromosome5Bandp15.3)
                (e/addition nil h/HumanChromosome5Bandq23)))

(defclass k46_XX_ins!5_?!!q13_?!
  :label "The 46,XX,ins(5;?)(q13;?) karyotype"
  :comment "ISCN2009 pg 61 -> 'Material of unknown origin has been
  inserted into the long arm of chromosome 5 at band 5q13. Use of the
  symbol add in this situation, i.e., add(5)(q13), would have denoted
  that unknown material had replaced the segment 5q13qter.'
  46,XX,ins(5;?)(pter->q13::?::q13->qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/insertion 1
               [h/HumanChromosome5Bandq13]
               [h/HumanChromosomeBand
                h/HumanChromosomeBand]))


;; DELETIONS
(defclass k46_XX_del!5!!q13!
  :label "The 46,XX,del(5)(q13) karyotype"
  :comment "ISCN2009 pg 61 -> 'Terminal deletion with a break (:) in
  band 5q13. The remaining chromosome consistes of the entire short
  arm of chromomsome 5 and the long arm lying between the centromere
  and band 5q13.' 46,XX,del(5)(pter->q13:)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/deletion 1 h/HumanChromosome5Bandq13))

(defclass k46_XX_del!5!!q13q33!
  :label "The 46,XX,del(5)(q13q33) karyotype"
  :comment "ISCN2009 pg 61 -> 'Interstitial deletion with breakage and
  reunion (::) of bands 5q13 and 5q33. The segment lying between these
  bands has been deleted.' 46,XX,del(5)(pter->q13::q33->qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/deletion 1
              h/HumanChromosome5Bandq13
              h/HumanChromosome5Bandq33))

(defclass k46_XX_del!5!!q13q13!
  :label "The 46,XX,del(5)(q13q13) karyotype"
  :comment "ISCN2009 pg 61 -> 'Interstitial deletion of a small
  segment within band 5q13, i.e., both breakpoints are in band 5q13.'
  46,XX,del(5)(pter->q13::q13->qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/deletion 1
              h/HumanChromosome5Bandq13
              h/HumanChromosome5Bandq13))

(defclass k46_XX_del!5!!q?!
  :label "The 46,XX,del(5)(q?) karyotype"
  :comment "ISCN2009 pg 61 -> 'Deletion of the long arm of chromosome
5, but it is unclear whether it is a terminal or an interstitial
deletion, and also the breakpoints are unknown.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/deletion 1
              h/HumanChromosome5Bandq
              h/HumanChromosome5Bandq))

(defclass k46_Y_del!X!!p21p21!
  :label "The 46,Y,del(X)(p21p21) karyotype"
  :comment "ISCN2009 pg 61 -> 'Interstitial deletion of a small
  segment within band Xp21'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/deletion 1
              h/HumanChromosomeXBandp21
              h/HumanChromosomeXBandp21))


;; DERIVATIVE CHROMOSOMES
;; 46,XX,der(6)(pter->q25.2::p22.2->pter)
;; 46,XX,rec(6)dup(6p)inv(6)(p22.2q25.2)mat

;; example derivative chromosome generated by more than one
;; rearrangement within a chromosome

(defclass k46_XY_der!9!del!9!!p12!del!9!!q31!
  :label "The 46,XY,der(9)del(9)(p12)del(9)(q31) karyotype"
  :comment "ISCN2009 pg 62 -> 'A derivative chromosome 9 resulting from
  terminal deletions in both the short and long arms with breakpoints
  in bands 9p12 and 9q31.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (f/derivative 1 [h/HumanChromosome9]
                (e/deletion nil h/HumanChromosome9Bandp12)
                (e/deletion nil h/HumanChromosome9Bandq31)))

(defclass k46_XY_der!9!inv!9!!p13p23!del!9!!q22q33!
  :label "The 46,XY,der(9)inv(9)(p13p23)del(9)(q22q33) karyotype"
  :comment "ISCN2009 pg 62 -> 'A derivative chromosome 9 rsulting from
  ab inversion in the short arm with breakpoints in 9p13 and 9p23, and
  an interstitial deletion of the long arm with breakpoints in 9q22
  and 9q33.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (f/derivative 1 [h/HumanChromosome9]
                (e/inversion nil h/HumanChromosome9Bandp13
                             h/HumanChromosome9Bandp23)
                (e/deletion nil h/HumanChromosome9Bandq22
                            h/HumanChromosome9Bandq33)))

(defclass k46_XX_der!7!add!7!!p22!add!7!!q22!
  :label "The 46,XX,der(7)add(7)(p22)add(7)(q22) karyotype"
  :comment "ISCN2009 pg 62 -> 'A derivative chromosome 7 with
  additional material of unknown origin attached at band
  7p22. Similarly, additional material of unknown origin is attached
  to 7q22, replacing the segment 7q22qter.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/derivative 1 [h/HumanChromosome7]
                (e/addition nil h/HumanChromosome7Bandp22)
                (e/addition nil h/HumanChromosome7Bandq22)))

(defclass k46_XX_der!5!add!5!!p15.1!del!5!!q13!
  :label "The 46,XX,der(5)add(5)(p15.1)del(5)(q13) karyotype"
  :comment "ISCN2009 pg 62 -> 'A derivative chromosome 5 with
  additional material of unknown origin attached at 5p15.1 and a
  terminal deletion of the long arm distal to band 5q13.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/derivative 1 [h/HumanChromosome5]
                (e/addition nil h/HumanChromosome5Bandp15.1)
                (e/addition nil h/HumanChromosome5Bandq13)))

(defclass k46_Y_der!X!t!X_8!!p22.3_q24.1!
  :label "The 46,Y,der(X)t(X;8)(p22.3;q24.1) karyotype"
  :comment "ISCN2009 pg 63 -> 'A male showing a derivative X
  chromosome derived from a tranlocation between Xp22.3 and 8q24.1.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (f/derivative 1 [h/HumanChromosomeX]
                (e/translocation nil [h/HumanChromosomeXBandp22.3]
                                 [h/HumanChromosome8Bandq24.1])))

(defclass k46_XX_der!1!t!1_3!!p22_q13.1!
  :label "The 46,XX,der(1)t(1;3)(p22;q13.1) karyotype"
  :comment "ISCN2009 pg 63 -> 'The derivative chromosome 1 has
  resulted from a translocation of the chromosome 3 segment distal to
  3q13.1 to the short arm of chromosome 1 at band 1p22. The der(1)
  replaces a normal chromosome 1 and there is no need to indicate the
  missing chromosome. There are obviously two normal chromosomes
  3. The karyotype is unbalanced with loss of the segemnt 1p22pter and
  gain of 3q13.1qter.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/derivative 1 [h/HumanChromosome1]
                (e/translocation nil [h/HumanChromosome1Bandp22]
                                 [h/HumanChromosome3Bandq13.1])))

(defclass k45_XY_der!1!t!1_3!!p22_q13.1!_-3
  :label "The 45,XY,der(1)t(1;3)(p22;q13.1),-3 karyotype"
  :comment "ISCN2009 pg 63 -> 'The derivative chromosome 1 (same as
  above) replaces a normal chromosome 1, but there is only one normal
  chromsome 3. One can presume that it is the der(3) resulting from
  the t(1;3) that has been lost, but the karyotype cannot make
  explicit such assumptions.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (f/derivative 1 [h/HumanChromosome1]
                (e/translocation nil [h/HumanChromosome1Bandp22]
                                 [h/HumanChromosome3Bandq13.1]))
  (e/deletion 1 h/HumanChromosome3))

;; example derivative chromosome generated by more than one
;; rearrangement involving two or more chromosome

(defclass k46_XX_der!1!t!1_3!!p32_q21!t!1_11!!q25_q13!
  :label "The 46,XX,der(1)t(1;3)(p32;q21)t(1;11)(q25;q13) karyotype"
  :comment "ISCN2009 pg 63 -> 'A derivative chromosome 1 generated by
  two translocations, one involving the short arm with a breakpoint in
  1p32 and the other involving the long arm with a breakpoint in
  1q25.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/derivative 1 [h/HumanChromosome1]
                (e/translocation nil [h/HumanChromosome1Bandp32]
                                 [h/HumanChromosome3Bandq21])
                (e/translocation nil [h/HumanChromosome1Bandq25]
                                 [h/HumanChromosome11Bandq13])))

(defclass k46_XY_der!1!t!1_3!!p32_q21!t!3_7!!q28_q11.2!
  :label "The 46,XY,der(1)t(1;3)(p32;q21)t(3;7)(q28;q11.2) karyotype"
  :comment "ISCN2009 pg 63 -> 'A derivative chromosome 1 resulting
  from a translocation of the chromosome 3 segment distal to 3q21 onto
  1p32, and a translocation of the segment 7q11.2qter to band 3q28 of
  the chromosome 3 segment attached to chromosome 1.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (f/derivative 1 [h/HumanChromosome1]
                (e/translocation nil [h/HumanChromosome1Bandp32]
                                 [h/HumanChromosome3Bandq21])
                (e/translocation nil [h/HumanChromosome3Bandq28]
                                 [h/HumanChromosome7Bandq11.2])))

(defclass k46_XY_der!1!t!1_3!!p32_q21!dup!1!!q25q42!
  :label "The 46,XY,der(1)t(1;3)(p32;q21)dup(1)(q25q42) karyotype"
  :comment "ISCN2009 pg 63 -> 'A derivative chromosome 1 resulting
  from a t(1;3) with a breakpoint in 1p32 and a duplication of the
  long arm segment 1q25q42.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (f/derivative 1 [h/HumanChromosome1]
                (e/translocation nil [h/HumanChromosome1Bandp32]
                                 [h/HumanChromosome3Bandq21])
                (e/duplication nil h/HumanChromosome1Bandq25
                               h/HumanChromosome1Bandq42)))

(defclass k46_XY_der!9!del!9!!p12!t!9_13!!q34_q11!
  :label "The 46,XY,der(9)del(9)(p12)t(9;13)(q34;q11) karyotype"
  :comment "ISCN2009 pg 64 -> 'A derivative chromosome 9 generated by
  a terminal deletion of the short arm with a breakpoint in 9p12, and
  by a t(9;13) involving the long arm with a breakpoint in 9q34.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (f/derivative 1 [h/HumanChromosome9]
                (e/deletion nil h/HumanChromosome9Bandp12)
                (e/translocation nil [h/HumanChromosome9Bandq34]
                                 [h/HumanChromosome13Bandq11])))

(defclass k46_XX_der!1!t!1_11!!p32_q13!t!1_3!!q25_q21!
  :label "The 46,XX,der(1)t(1;11)(p32;q13)t(1;3)(q25;q21) karyotype"
  :comment "ISCN2009 pg 64 -> 'A derivative chromosome 1 generated by
  two translocations, one involving a breakpoint in 1p32 and 11q13 and
  the other involving a breakpoint in 1q25 and 3q21. The detailed
  system describes the derivative 1 from 11qter to 3qter as the
  aberrations are listed according to the orientation of chromosome 1,
  from the p arm to the q arm.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/derivative 1 [h/HumanChromosome1]
                (e/translocation nil [h/HumanChromosome1Bandp32]
                                 [h/HumanChromosome11Bandq13])
                (e/translocation nil [h/HumanChromosome1Bandq25]
                                 [h/HumanChromosome3Bandq21])))

;; 47,XY,+mos der(8)r(1;8;17)(p36.3p35;p12q13;q25q25)

;; ;; ERROR in book! - there is no resolution band 17q11
;; (defclass k46_XX_der!1!del!1!!p22p34!ins!1_17!!p22_q11q25!
;;   :label "The 46,XX,der(1)del(1)(p22p34)ins(1;17)(p22;q11q25) karyotype"
;;   :comment "ISCN2009 pg 64 -> 'A derivative chromosome 1 resulting
;;   from an interstitial deletion of the short arm with breakpoints in
;;   1p22 and 1p34, and a replacement of this segment by an insertion of
;;   a segment from the long arm of chromosome 17. In such situations,
;;   when there are two breakpoints in the recipient chromosome, the
;;   proximal one is listed as the point of insertion.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (f/derivative 1 h/HumanChromosome1
;;                 (e/deletion 1 h/HumanChromosome1Bandp22
;;                             h/HumanChromosome1Bandp34)
;;                 (e/direct-insertion 1 h/HumanChromosome1Bandp22
;;                                     h/HumanChromosome17Bandq11
;;                                     h/HumanChromosome17Bandq25)))

(defclass k46_XY_der!7!t!2_7!!q21_q22!ins!7_?!!q22_?!
  :label "The 46,XY,der(7)t(2;7)(q21;q22)ins(7;?)(q22;?) karyotype"
  :comment "ISCN2009 pg 64 -> 'A derivative chromosome 7 in which
  material of unknown origin has replaced the segment 6q22qter, and
  the seqment 2q21qter from the long arm of chromosome 2 is attached
  to the unknown chromosome material. By convention, the breakpoint in
  the derivative chromosome is specified as the point of insertion of
  the unknown material.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (f/derivative 1 [h/HumanChromosome7]
                (e/translocation nil [h/HumanChromosome2Bandq21]
                                 [h/HumanChromosome7Bandq22])
                (e/insertion nil
                             [h/HumanChromosome7Bandq22]
                             [h/HumanChromosomeBand
                              h/HumanChromosomeBand])))

(defclass k46_XX_der!8!t!8_17!!p23_q21!inv!8!!p22q13!t!8_22!!q22_q12!
  :label "The
  46,XX,der(8)t(8;17)(p23;q21)inv(8)(p22q13)t(8;22)(q22;q12)
  karyotype"
  :comment "ISCN2009 pg 64 -> 'A derivative chromosome 8 resulting
  from two translocations, one affecting the short arm, one the long
  arm, with breakpoints at 8p23 and 8q22, respectively, and a
  pericentric inversion with breakpoints at 8p22 and 8q13.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/derivative 1 [h/HumanChromosome8]
                (e/translocation nil [h/HumanChromosome8Bandp23]
                                 [h/HumanChromosome17Bandq21])
                (e/inversion nil h/HumanChromosome8Bandp22
                             h/HumanChromosome8Bandq13)
                (e/translocation nil [h/HumanChromosome8Bandq22]
                                 [h/HumanChromosome22Bandq12])))

;; example isoderivative chromosomes - isochromosome formation for one
;; of the arms of a derivative chromosome.

;; TODO - check recieving and providing break points!
;; (defclass k46_XX_ider!22!!q10!t!9_22!!q34_q11.2!
;;   :label "The 46,XX,ider(22)(q10)t(9;22)(q34;q11.2) karyotype"
;;   :comment "ISCN2009 pg 64 -> 'An isochromosome for the long arm of a
;;   derivative chromosome 22 generated by a t(9;22), i.e., an
;;   isochromosome for the long arm of a Ph chromosome.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (f/isoderivative 1 h/HumanChromosome22
;;                    h/HumanChromosome22Bandq10
;;                    (e/translocation 1 2 h/HumanChromosome9Bandq34
;;                                     h/HumanChromosome22Bandq11.2)))

;; ;; TODO
;; (defclass k46_XY_ider!9!!p10!ins!9_12!!p13_q13q22!
;;   :label "The 46,XY,ider(9)(p10)ins(9;12)(p13;q13q22) karyotype"
;;   :comment "ISCN2009 pg 65 -> 'An isochromosome for the short arm of
;;   the derivative chromosome 9 resulting from an insertion of the
;;   segment 12q13q22 at band 9p13.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XY)
;;   ;; (f/isoderivative 1 h/HumanChromosome9
;;   ;;                  h/HumanChromosome9Bandp10
;;   ;;                  (e/direct-insertion 1 h/HumanChromosome9Bandp13
;;   ;;                                      h/HumanChromosome12Bandq13
;;   ;;                                      h/HumanChromosome12Bandq22)))
;;   (f/isoderivative 1 h/HumanChromosome9 h/HumanChromosome9Bandp10
;;                    (owl-some e/hasEvent
;;                             (owl-and e/DirectInsertion
;;                                     (owl-some e/hasReceivingBreakPoint
;;                                              h/HumanChromosome9Bandp13)
;;                                     (owl-some e/hasProvidingBreakPoint
;;                                              h/HumanChromosome12Bandq13
;;                                              h/HumanChromosome12Bandq22)))))

;; example dicentric derivative chromosomes with additional abnormalities.

(defclass k45_XX_der!5_7!t!5_7!!q22_p13!t!3_7!!q21_q21!
  :label "The 45,XX,der(5;7)t(5;7)(q22;p13)t(3;7)(q21;q21) karyotype"
  :comment "ISCN2009 pg 65 -> 'A dicentric derivative
  chromosome. Breakage and reunion have occurred at band 5q22 in the
  long arm of chromosome 5 and at band 7p13 in the short arm of
  chromosome 7. In addition, the segment 3q21qter has been
  translocated onto the long arm of chromosome 7 at band 7q21.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/derivative 1 [h/HumanChromosome5 h/HumanChromosome7]
                (e/translocation nil [h/HumanChromosome5Bandq22]
                                 [h/HumanChromosome7Bandp13])
                (e/translocation nil [h/HumanChromosome3Bandq21]
                                 [h/HumanChromosome7Bandq21])))

(defclass k45_XY_der!5_7!t!3_5!!q21_q22!t!3_7!!q29_p13!
  :label "The 45,XY,der(5;7)t(3;5)(q21;q22)t(3;7)(q29;p13) karyotype"
  :comment "ISCN2009 pg 65 -> 'A dicentric derivative chromosome
  composed of chromosomes 5 and 7. The same acentric chromosome 3
  segemnt as in the previous example is inserted between the long arm
  of chromosome 5 and the short arm of chromosome 7 at band 7q32.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (f/derivative 1 [h/HumanChromosome5 h/HumanChromosome7]
                (e/translocation nil [h/HumanChromosome3Bandq21]
                                 [h/HumanChromosome5Bandq22])
                (e/translocation nil [h/HumanChromosome3Bandq29]
                                 [h/HumanChromosome7Bandp13])))


(defclass k45_XY_der!5_7!t!3_5!!q21_q22!t!3_7!!q29_p13!del!7!!q32!
  :label "The 45,XY,der(5;7)t(3;5)(q21;q22)t(3;7)(q29;p13)del(7)(q32) karyotype"
  :comment "ISCN2009 pg 65 -> 'The same dicentric derivative
  chromosomes as in the previous example but with a additional
  terminal deletion of the long arm of chromosome 7 at band 7q32.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (f/derivative 1 [h/HumanChromosome5 h/HumanChromosome7]
                (e/translocation nil [h/HumanChromosome3Bandq21]
                                 [h/HumanChromosome5Bandq22])
                (e/translocation nil [h/HumanChromosome3Bandq29]
                                 [h/HumanChromosome7Bandp13])
                (e/deletion nil h/HumanChromosome7Bandq32)))

;; TODO
;; (defclass k45_XX_der!8_8!!q10_q10!del!8!!q22!t!8_9!!q24.1_q12!
;;   :label "The 45,XX,der(8;8)(q10;q10)del(8)(q22)t(8;9)(q24.1;q12) karyotype"
;;   :comment "ISCN2009 pg 65 -> 'A derivative chromosome composed of the
;;   long arms of chromosome 8 with material from chromosome 9
;;   translocated to one arm at band 8q24.1 and a deletion at band 8q22
;;   in the other arm.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (f/derivative 1 h/HumanChromosome8Bandq10 h/HumanChromosome8Bandq10
;;                 (e/deletion 1 h/HumanChromosome8Bandq22)
;;                 (e/translocation 1 2 h/HumanChromosome8Bandq24.1
;;                                  h/HumanChromosome9Bandq12)))

;; REDEFINE
(defclass k47_XY_+der!?!t!?_9!!?_q22!
  :label "The 47,XY,+der(?)t(?;9)(?;q22) karyotype"
  :comment "ISCN2009 pg 65 -> 'The distal segment of the long arm of
  chromosome 9 from band 9q22 has been translocated to a
  centromere-containing derivative chromosome of unknown origin.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/addition 1 h/HumanChromosome)
  (f/derivative 1 [h/HumanChromosome]
                (e/translocation nil [h/HumanChromosomeBand]
                                 [h/HumanChromosome9Bandq22])))

;; REDEFINE
(defclass k47_XX_+der!?!t!?_9!!?_p13!ins!?_7!!?_q11.2q32!
  :label "The 47,XX,+der(?)t(?;9)(?;p13)ins(?;7)(?;q11.2q32) karyotype"
  :comment "ISCN2009 pg 65 -> 'A derivative chromosome of unknown
  origin onto which is translocated in its short arm the segment of
  chromosome 9 distal to band 9p13, and which also contains an
  insertion in the long arm of the chromosome 7 segment between bands
  7q11.2 and 7q32.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 1 h/HumanChromosome)
  (f/derivative 1 [h/HumanChromosome]
                (e/translocation nil [h/HumanChromosomeBand]
                                 [h/HumanChromosome9Bandp13])
                (e/insertion nil [h/HumanChromosomeBand]
                             [h/HumanChromosome7Bandq11.2
                              h/HumanChromosome7Bandq32])))

;; REDEFINE
(defclass k47_XX_+der!?!t!?_9!!?_p13!hsr!?!
  :label "The 47,XX,+der(?)t(?;9)(?;p13)hsr(?) karyotype"
  :comment "ISCN2009 pg 66 -> 'A derivative chromosome of unknown
  origin with the same translocation in its short arms as in the
  previous example, and a homogeneously staining region in the long
  arm.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 1 h/HumanChromosome)
  (f/derivative 1 [h/HumanChromosome]
                (e/translocation nil [h/HumanChromosomeBand]
                                 [h/HumanChromosome9Bandp13])
                (f/hsr nil h/HumanChromosomeBand)))

;; ;; 53,XX,...,+der(?)t(?;9)(?;q22),+r,+mar,dmin

(defclass
k46_XX_der!9!del!9!!p12!t!9_22!!q34_q11.2!_der!9!t!9_12!!p13_q22!inv!9!!q13q22!
  :label "The
  46,XX,der(9)del(9)(p12)t(9;22)(q34;q11.2),der(9)t(9;12)(p13;q22)inv(9)(q13q22)
  karyotype"
  :comment "ISCN2009 pg 66 -> 'One der(9) is the result of a deletion
  of the short arm and a translocation involving the long arm; the
  other der(9) is the result of a translocation affecting the short
  arm and a paracentric inversion in the long arm of the homologous
  chromosome 9. There are two normal chromosmoes 12, two normal
  chromosomes 22, but no normal chromosome 9.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/derivative 1 [h/HumanChromosome9]
                (e/deletion nil h/HumanChromosome9Bandp12)
                (e/translocation nil [h/HumanChromosome9Bandq34]
                                 [h/HumanChromosome22Bandq11.2]))
  (f/derivative 1 [h/HumanChromosome9]
                (e/translocation nil [h/HumanChromosome9Bandp13]
                                 [h/HumanChromosome12Bandq22])
                (e/inversion nil h/HumanChromosome9Bandq13
                             h/HumanChromosome9Bandq22)))

;; ;; 46,XX,der(1)t(1;3)(p34.3;q21),der(1*)t(1;3)(p34.3;q21)
;; ;; 46,XX,der(1)t(1;3)(p34.3;q21)[20]/46,XX,der(1*)t(1*;3)(p34.3;q21)[10]

;; REDEFINE
(defclass k47_XX_t!9_22!!q34_q11.2!_+der!22!t!9_22!
  :label "The 47,XX,t(9;22)(q34;q11.2),+der(22)t(9;22) karyotype"
  :comment "ISCN2009 pg 66 -> 'Karyotype with t(9;22) and and
  additional Ph chromosome. The breakpoints in the extra der(22) need
  not be repeated.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/translocation 1
                   [h/HumanChromosome9Bandq34]
                   [h/HumanChromosome22Bandq11.2])
  (e/addition 1 h/HumanChromosome22)
  (f/derivative 1 [h/HumanChromosome22]
                (e/translocation nil [h/HumanChromosome9Bandq34]
                                 [h/HumanChromosome22Bandq11.2])))

;; TODO
(defclass
k46_XX_der!1!t!1_3!!p32_q21!inv!1!!p22q21!t!1_11!!q25_q13!_der!3!t!1_3!_der!11!t!1_11!
  :label "The
  46,XX,der(1)t(1;3)(p32;q21)inv(1)(p22q21)t(1;11)(q25;q13),der(3)t(1;3),der(11)t(1;11)
  karyotype"
  :comment "ISCN2009 pg 66 -> 'A balanced complex rearrangement with
  three derivative chromosomes. The breakpoints of the t(1;3) and the
  t(1;11), which both contrivute to the der(1), are not repeated in
  the description of der(3) and der(11).'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/derivative 1 [h/HumanChromosome1]
                (e/translocation nil [h/HumanChromosome1Bandp32]
                                 [h/HumanChromosome3Bandq21])
                (e/inversion nil h/HumanChromosome1Bandp22
                             h/HumanChromosome1Bandq21)
                (e/translocation nil [h/HumanChromosome1Bandq25]
                                 [h/HumanChromosome11Bandq13]))
  (f/derivative 1 [h/HumanChromosome3]
                (e/translocation nil [h/HumanChromosome1Bandp32]
                                 [h/HumanChromosome3Bandq21]))
  (f/derivative 1 [h/HumanChromosome11]
                (e/translocation nil [h/HumanChromosome1Bandq25]
                                 [h/HumanChromosome11Bandq13])))


;; DICENTRIC CHROMOSOMES
;; example dicentric chromosomes
(defclass k45_XX_dic!13_13!!q14_q32!
  :label "The 45,XX,dic(13;13)(q14;q32) karyotype"
  :comment "ISCN2009 pg 67 -> 'Breakage and reunion have occurred at
  bands 13q14 and 13q32 on the two homologous chromosomes 13 to form a
  dicentric chromosome. There is no normal chromosome 13. If it can be
  shown that the dicentric chromosome has originated through breakage
  and reunion of sister chromatids, it may be designated, e.g.,
  dic(13)(q14q32).'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/dicentric 1
               h/HumanChromosome13Bandq14
               h/HumanChromosome13Bandq32))

(defclass k45_XX_dic!13_15!!q22_q24!
  :label "The 45,XX,dic(13;15)(q22;q24) karyotype"
  :comment "ISCN2009 pg 67 -> 'A dicentric chromosome with breaks and
  runion at bands 13q22 and 15q24. The missing chromosomes 13 and 15
  are not indicated since theye are replaced by the dicentric
  chromosome. The karyotype thus contains one normal chromosome 13,
  one normal chromosome 15, and the dic(13;15). The resulting net
  imbalance of this abnormality is loss of the segments distal to
  13q22 and 15q24.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/dicentric 1
               h/HumanChromosome13Bandq22
               h/HumanChromosome15Bandq24))

(defclass k46_XX_+13_dic!13_15!!q22_q24!
  :label "The 46,XX,+13,dic(13;15)(q22;q24) karyotype"
  :comment "ISCN2009 pg 67 -> 'A dicentric chromosome with breaks and
  reunion at bands 13q22 and 15q24 (same as above) has replaced one
  chromosome 13 and one chromosome 15. There are, however two normal
  chromosomes 13, i.e., an additional chromosome 13 in relation to the
  expected loss due to the dic(13;15). Consequently, the gain in
  indicated as +13. The karyotype thus contains two normal chromosomes
  13, one normal chromosome 15, and the dic(13;15). The resulting net
  imbalance is partial trisomy for the segment 13pterq22 and loss of
  the segemnt 15q24qter.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 1 h/HumanChromosome13)
  (f/dicentric 1
               h/HumanChromosome13Bandq22
               h/HumanChromosome15Bandq24))

(defclass k45_XY_dic!14_21!!p11.2_p11.2!
  :label "The 45,XY,dic(14;21)(p11.2;p11.2) karyotype"
  :comment "ISCN2009 pg 67 -> 'A dicentric chromosome with breaks and
  reunion at bands 14p11.2 and 21p11.2. The missing chromosomes 14 and
  21 are not indicated since they are replaced by the dicentric
  chromosome. The karyotype thus contains one normal chromosome 14,
  one normal chromosome 21, and the dic(14;21). The resulting net
  imbalance of this abnormality is loss of the segments distal to
  14p11.2 and 21p11.2.)'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (f/dicentric 1
               h/HumanChromosome14Bandp11.2
               h/HumanChromosome21Bandp11.2))

;; REDEFINE
(defclass k47_XY_+dic!17_?!!q22_?!
  :label "The 47,XY,+dic(17;?)(q22;?) karyotype"
  :comment "ISCN2009 pg 67 -> 'An additional dicentric chromosome
  composed of one chromosome 17 with a break at band 17q22 and an
  unknown chromosome with an intact centromere.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/addition 1 h/HumanChromosome17)
  (e/addition 1 h/HumanChromosome)
  (f/dicentric 1
               h/HumanChromosome17Bandq22
               h/HumanChromosomeBand))

;; example isodicentric chromosomes
(defclass k46_X_idic!Y!!q12!
  :label "The 46,X,idic(Y)(q12) karyotype"
  :comment "ISCN2009 pg 68 -> 'Breakage and reunion have occurred at
  band Yq12 on sister chromatids to form an isodicentric Y
  chromosome. Th resulting net imbalance is loss of the segment
  Yq12qter and gain of Ypterq12.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (f/isodicentric 1 h/HumanChromosomeYBandq12))

(defclass k46_XX_idic!21!!q22.3!
  :label "The 46,XX,idic(21)(q22.3) karyotype"
  :comment "ISCN2009 pg 68 -> 'An isodicentric with breakage and
  reunion at the terminal ends of two chromosomes 21. There are two
  copies of the long arm of chromosome 21, joined at q22.3, and one
  normal chromosome 21, indicated by the 46 count. Even though there
  are effectively three copies of the chromosome 21 long arm, ther
  normal chromosome 21 is not designated with a (+) sign.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/isodicentric 1 h/HumanChromosome21Bandq22.3))

;; REDIFINE
(defclass k47_XX_+idic!13!!q22!
  :label "The 47,XX,+idic(13)(q22) karyotype"
  :comment "ISCN2009 pg 68 -> 'An additional isodicentric chromosome
  13. There are two chromosomes 13 and the idic(13). Another example
  shown in Section 9.2.11.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 2 h/HumanChromosome13)
  (f/isodicentric 1 h/HumanChromosome13Bandq22))

;; REDEFINE
(defclass k47_XY_+idic!15!!q12!
  :label "The 47,XY,+idic(15)(q12) karyotype"
  :comment "ISCN2009 pg 68 -> 'An additional apparent isodicentric
  chromosome 15. There are two chromosome 15 and the
  idic(15)(q12). This rearrangement has historically been referred to
  as inv dup(15)(q12). However, because most result from recombination
  between homologues, dic(15;15)(q12;q12), (or psu dic, see below),
  would be more appropriate designation.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/addition 2 h/HumanChromosome15)
  (f/isodicentric 1 h/HumanChromosome15Bandq12))

;; REDEFINE

;; TOFIX - should break it however concepts are not closed -
;; inconsistent as disjoint AND equivalent to k47_XY_+idic!15!!q12!
(defclass k47_XY_+dic!15_15!!q12_q12!
  :label "The 47,XY,dic(15;15)(q12;q12) karyotype"
  :comment "ISCN2009 pg 68 -> 'An additional apparent isodicentric
  chromosome 15. There are two chromosome 15 and the
  idic(15)(q12). This rearrangement has historically been referred to
  as inv dup(15)(q12). However, because most result from recombination
  between homologues, dic(15;15)(q12;q12), (or psu dic, see below),
  would be more appropriate designation.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/addition 2 h/HumanChromosome15)
  (f/dicentric 1
               h/HumanChromosome15Bandq12
               h/HumanChromosome15Bandq12))

;; example pseudodicentric chromosome examples
(defclass k45_XX_psu_dic!15_13!!q12_q12!
  :label "The 45,XX,psu dic(15;13)(q12;q12) karyotype"
  :comment "ISCN2009 pg 68 -> 'A pseudodicentric chromosome has
  replaced one chromosome 13 and one chromosome 15. The karyotype thus
  contains one normal chromosome 13, one normal chromosome 15, and the
  psu dic(15;13). The centromere of the chromosome mentioned first,
  i.e., chromosome 15, is the active one.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/pseudo-dicentric 1
                      h/HumanChromosome15Bandq12
                      h/HumanChromosome15Bandq12))

(defclass k46_XX_psu_idic!20!!q11.2!
  :label "The 46,XX,psu idic(20)(q11.2) karyotype"
  :comment "ISCN2009 pg 68 -> 'A pseudodicentric chromosome has
  replaced one chromosome 20, resulting in three copies of
  20pterq11.2. The psu idic(20) has one active centromere.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/pseudo-dicentric 1
                      h/HumanChromosome15Bandq12
                      h/HumanChromosome15Bandq12))


;; DUPLICATIONS
;; NOTE: Only the detailed system will clarify the location of the
;; duplicated segment
(defclass k46_XX_dup!1!!q22q25!
  :label "The 46,XX,dup(1)(q22q25) karyotype"
  :comment "ISCN2009 pg 69 -> 'Direct duplication of the segment
  between bands 1q22 and 1q25.' 46,XX,dup(1)(pter->q25::q22->qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/duplication 1
                 h/HumanChromosome1Bandq22
                 h/HumanChromosome1Bandq25))

;; NOTE: Only the detailed system will clarify the location of the
;; duplicated segment
;; AKA pterq25q25q22q25qter or pterq22q25q22q22qter
(defclass k46_XY_dup!1!!q25q22!
  :label "The 46,XY,dup(1)(q25q22) karyotype"
  :comment "ISCN2009 pg 69 -> 'Inverse duplication of the segment
  between bands 1q22 and 1q25. Note that only the detailed system will
  clarify the location of the duplication segment.'
  46,XY,dup(1)(pter->q25::q25->q22::q25->qter) or
  46,XY,dup(1)(pter->q22::q25->q22::q22->qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/duplication 1
                 h/HumanChromosome1Bandq25
                 h/HumanChromosome1Bandq22))

;; NOTE: Only the detailed system will clarify the location of the
;; duplicated segment
;; AKA pterq25q25q22q25qter or pterq22q25q22q22qter
(defclass k46_XY_dup!1!!q25q22!Again
  :label "The 46,XY,dup(1)(q25q22) karyotype"
  :comment "ISCN2009 pg 69 -> 'Inverse duplication of the segment
  between bands 1q22 and 1q25. Note that only the detailed system will
  clarify the location of the duplication segment.'
  46,XY,dup(1)(pter->q25::q25->q22::q25->qter) or
  46,XY,dup(1)(pter->q22::q25->q22::q22->qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/direct-event 1 (owl-and e/Duplication
                             (owl-some e/hasBreakPoint
                                       h/HumanChromosome1Bandq25
                                       h/HumanChromosome1Bandq22)
                             (e/direct-event nil (e/inversion-pattern
                                                  h/HumanChromosome1Bandq25
                                                  h/HumanChromosome1Bandq22)))))

;; FISSION
(defclass k47_XY_-10_+fis!10!!p10!_+fis!10!!q10!
  :label "The 47,XY,-10,+fis(10)(p10),+fis(10)(q10) karyotype"
  :comment "ISCN2009 pg 69 -> 'Break in the centromere resulting in
  two derivative chromosomes composed of the short and long arms
  respectively. The breakpoints (:) are assigned to p10 and q10
  according to the morpohology of the derivative chromosomes.'
  47,XY,-10,+fis(10)(pter->p10:),+fis(10)(qter->q10:)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/deletion 1 h/HumanChromosome10)
  (e/fission 1 h/HumanChromosome10Bandp10)
  (e/fission 1 h/HumanChromosome10Bandq10))


;; FRAGILE SITES
(defclass k46_X_fra!X!!q27.3!
  :label "The 46,X,fra(X)(q27.3) karyotype"
  :comment "ISCN2009 pg 69 -> 'A fragile site in subband Xq27.3 on one
  X chromosome in a female.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/fragilesite 1 h/HumanChromosomeXBandq27.3))

(defclass k46_Y_fra!X!!q27.3!
  :label "The 46,Y,fra(X)(q27.3) karyotype"
  :comment "ISCN2009 pg 69 -> 'A fragile site in subband Xq27.3 on one
  X chromosome in a male.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (f/fragilesite 1 h/HumanChromosomeXBandq27.3))

(defclass k45_fra!X!!q27.3!
  :label "The 45,fra(X)(q27.3) karyotype"
  :comment "ISCN2009 pg 69 -> 'A fragile site in subband Xq27.3 on one
  X chromosome in a Turner syndrome patient'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XN)
            (e/deletion 1 h/HumanSexChromosome))) ;; aka 45,X
  (f/fragilesite 1 h/HumanChromosomeXBandq27.3))

(defclass k47_XY_fra!X!!q27.3!
  :label "The 47,XY,fra(X)(q27.3) karyotype"
  :comment "ISCN2009 pg 69 -> 'A fragile site in subband Xq27.3 on one
  X chromosome in a Klinefelter syndrome patient.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/addition 1 h/HumanChromosomeX))) ;; aka 47,XXY
  (f/fragilesite 1 h/HumanChromosomeXBandq27.3))


;; HOMOGENEOUSLY STAINING REGIONS
(defclass k46_XX_hsr!1!!p22!
  :label "The 46,XX,hsr(1)(p22) karyotype"
  :comment "ISCN2009 pg 70 -> 'A homogeneously staining region in band
  1p22.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/hsr 1 h/HumanChromosome1Bandp22))

;; examples with a chromosome that contains multiple hsr or one hsr
;; and another structural change

(defclass k46_XX_der!1!hsr!1!!p22!hsr!1!!q31!
  :label "The 46,XX,der(1)hsr(1)(p22)hsr(1)(q31) karyotype"
  :comment "ISCN2009 pg 70 -> 'Two homogenously stating regions in
  chromosome 1: one in band 1p22 in the short arm and the other in
  band 1q31 in the long arm.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/derivative 1 [h/HumanChromosome1]
                (f/hsr nil h/HumanChromosome1Bandp22)
                (f/hsr nil h/HumanChromosome1Bandq31)))

(defclass k46_XY_der!1!!del!1!!p21p33!hsr!1!!p21!
  :label "The 46,XY,der(1)(del(1)(p21p33)hsr(1)(p21) karyotype"
  :comment "ISCN2009 pg 70 -> 'The segment between bands 1p21 and 1p33
  is replaced by a homogeneously staining region that may be smaller
  or larger than the deleted segment. The hsr is by convention
  assigned to the poximal deletion breakpoint band.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (f/derivative 1 [h/HumanChromosome1]
                (e/deletion nil h/HumanChromosome1Bandp21
                            h/HumanChromosome1Bandp33)
                (f/hsr nil h/HumanChromosome1Bandp21)))

(defclass k46_XX_der!1!ins!1_7!!q21_p11.2p21!hsr!1_7!!q21_p11.2!
  :label "The 46,XX,der(1)ins(1;7)(q21;p11.2p21)hsr(1;7)(q21;p11.2) karyotype"
  :comment "ISCN2009 pg 70 -> 'Insertion of the segment 7p11.2p21 into
  the long arm of chromosome 1 with breakage and reunion at band
  1q21. The derivative chromosome also contains an hsr at the
  interface between the recipient and donor chromosomes. The hsr is
  located proximal to the segment inserted from chromosome 7.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/derivative 1 [h/HumanChromosome1]
                (e/insertion nil [h/HumanChromosome1Bandq21]
                             [h/HumanChromosome7Bandp11.2
                              h/HumanChromosome7Bandp21])
                (f/hsr nil h/HumanChromosome1Bandq21
                       h/HumanChromosome7Bandp11.2)))

(defclass k46_XX_der!1!ins!1_7!!q21_p11.2p21!hsr!1_7!!q21_p21!
  :label "The 46,XX,der(1)ins(1;7)(q21;p11.2p21)hsr(1;7)(q21;p21) karyotype"
  :comment "ISCN2009 pg 70 -> 'Insertion of the segment 7p11.2p21 into
  the long arm of chromosome 1 with breakage and reunion at band
  1q21. The derivative chromosome also contains hsr at the interface
  between the recipient and donor chromosomes. The hsr is located
  distal to the segment inserted from chromosome 7.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/derivative 1 [h/HumanChromosome1]
                (e/insertion nil [h/HumanChromosome1Bandq21]
                             [h/HumanChromosome7Bandp11.2
                              h/HumanChromosome7Bandp21])
                (f/hsr nil h/HumanChromosome1Bandq21
                       h/HumanChromosome7Bandp21)))


;; INSERTIONS
;; insertion within a chromosome
(defclass k46_XX_ins!2!!p13q21q31!
  :label "The 46,XX,ins(2)(p13q21q31) karyotype"
  :comment "ISCN2009 pg 71 -> 'Direct insertion, i.e., dir
  ins(2)(p13q21q31). The long-arm segment between bands 2q21 and 2q31
  has been inserted into the short arm at band 2p13. The original
  orientation of the inserted segment has been maintained in its new
  position, i.e., band 2q21 remains more proximal to the centromere
  than band 2q31.'
  46,XX,ins(2)(pter->p13::q31->q21::p13->q21::q31->qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/insertion 1
               [h/HumanChromosome2Bandp13
                h/HumanChromosome2Bandq21
                h/HumanChromosome2Bandq31]))

(defclass k46_XX_ins!2_2*!!p13q21q31!
  :label "The 46,XX,ins(2;2*)(p13;q21q31) karyotype"
  :comment "ISCN2009 pg 71 -> 'Direct insertion, i.e., dir
  ins(2;2*)(p13;q21q31). The long-arm segment between bands 2q21 and 2q31
  has been inserted into the short arm at band 2p13. The original
  orientation of the inserted segment has been maintained in its new
  position, i.e., band 2q21 remains more proximal to the centromere
  than band 2q31.'
  46,XX,ins(2)(pter->p13::q31->q21::p13->q21::q31->qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/insertion 1
               [h/HumanChromosome2Bandp13]
               [h/HumanChromosome2Bandq21
                h/HumanChromosome2Bandq31]))

(defclass k46_XY_ins!2!!p13q31q21!
  :label "The 46,XY,ins(2)(p13q31q21) karyotype"
  :comment "ISCN2009 pg 71 -> 'Inverted insertion, i.e., inv
  ins(2)(p13q21q31). The insertion is the same as in the previous
  example except that the inserted has been inverted, i.e. band 2q21
  of the inserted segment is now more distal to the centromere than
  band 2q31. The orientation of the bands within the segment has thus
  been reversed with respect to the centromere.'
  46,XY,ins(2)(pter->p13::q21->q31::p13->q21::q31->qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/insertion 1
               [h/HumanChromosome2Bandp13
                h/HumanChromosome2Bandq31
                h/HumanChromosome2Bandq21]))

;; insertion between two choromosomes
(defclass k46_XY_ins!5_2!!p14_q22q32!
  :label "The 46,XY,ins(5;2)(p14;q22q32) karyotype"
  :comment "ISCN2009 pg 71 -> 'Direct insertion, i.e., dir
  ins(5;2)(p14;q22q32). The long-arm segment between bands 2q22 and
  2q32 has been inserted into the short arm of chromosome 5 at at band
  5p14. The original orientation of the inserted segment has been
  maintained in its new position, i.e., band 2q22 remains more
  proximal to the centromere than band 2q32. Note that the recipient
  chromosome is specified first.'
  46,XY,ins(5;2)(5pter->5p14::2q32->2q22::5p14->5qter;2pter->2q22::2q32->2qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/insertion 1
               [h/HumanChromosome5Bandp14]
               [h/HumanChromosome2Bandq22
                h/HumanChromosome2Bandq32]))

(defclass k46_XY_ins!5_2!!p14_q32q22!
  :label "The 46,XY,ins(5;2)(p14;q32q22) karyotype"
  :comment "ISCN2009 pg 71 -> 'Inverted insertion, i.e., inv
  ins(5;2)(p14;q32q22). Breakage and reunion have occurred at the same
  bands as in the previous example, and the insertion is the same
  except that the inserted sergment has been inverted, i.e. band 2q22
  is now more distal to the centromere of the recipient chromosome
  than band 2q32.'
  46,XY,ins(5;2)(5pter->5p14::2q22->2q32::5p14->5qter;2pter->2q22::2q32->2qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/insertion 1
               [h/HumanChromosome5Bandp14]
               [h/HumanChromosome2Bandq32
                h/HumanChromosome2Bandq22]))

(defclass k46_XX_ins!5_2!!q31_p13p23!
  :label "The 46,XX,ins(5;2)(q31;p13p23) karyotype"
  :comment "ISCN2009 pg 71 -> 'A direct insertion of bands p13 to p23
  from chromosome 2 into band 5q31.'
  46,XX,ins(5;2)(5pter->5q31::2p13->2p23::5q31->5qter;2pter->2p23::2p13->2qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/insertion 1
               [h/HumanChromosome5Bandq31]
               [h/HumanChromosome2Bandp13
                h/HumanChromosome2Bandp23]))

(defclass k46_XX_ins!5_2!!q31_p23p13!
  :label "The 46,XX,ins(5;2)(q31;p23p13) karyotype"
  :comment "ISCN2009 pg 71 -> 'A insertion of bands p13 to p23 from
  chromosome 2 into band 5q31 in an inverted orientation.'
  46,XX,ins(5;2)(5pter->5q31::2p23->2p13::5q31->5qter->2p23::2p13->2qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/insertion 1
               [h/HumanChromosome5Bandq31]
               [h/HumanChromosome2Bandp23
                h/HumanChromosome2Bandp13]))


;; INVERSION
(defclass k46_XX_inv!3!!q21q26.2!
  :label "The 46,XX,inv(3)(q21q26.2) karyotype"
  :comment "ISCN2009 pg 72 -> 'Paracentric inversion in which breakage
  and union have occurred at bands 3q21 and 3q26.2.'
  46,XX,inv(3)(pter->q21::q26.2->q21::q26.2->qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/inversion 1
               h/HumanChromosome3Bandq21
               h/HumanChromosome3Bandq26.2))

(defclass k46_XY_inv!3!!p13q21!
  :label "The 46,XY,inv(3)(p13q21) karyotype"
  :comment "ISCN2009 pg 72 -> 'Pericentric inversion in which breakage
  and reunion have occurred at bands 3p13 and 3q21.'
  46,XY,inv(3)(pter->p13::q21->p13::q21->qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/inversion 1
               h/HumanChromosome3Bandp13
               h/HumanChromosome3Bandq21))


;; ISOCHROMOSOMES
(defclass k46_XX_i!17!!q10!
  :label "The 46,XX,i(17)(q10) karyotype"
  :comment "ISCN2009 pg 72 -> 'An isochromosome for the entire long
  arm of one chromosome 17 and consequently the breakpoint is assigned
  to 17q10. There is one normal chromosome 17. The shorted designation
  i(17q) may be used in text but not in the karyotype to describe this
  isochromosome.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/isochromosome 1 h/HumanChromosome17Bandq10))

(defclass k46_X_i!X!!q10!
  :label "The 46,X,i(X)(q10) karyotype"
  :comment "ISCN2009 pg 72 -> 'One normal X chromosome and an
  isochromosome for the long arm of one X chromosome.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/isochromosome 1 h/HumanChromosomeXBandq10))

(defclass k47_XY_i!X!!q10!
  :label "The 47,XY,i(X)(q10) karyotype"
  :comment "ISCN2009 pg 72 -> 'A male showing an isochromosome of the
  long arm of the X chromosome in addition to a normal X and Y.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/addition 1 h/HumanChromosomeX))) ;; aka 47,XXY
  (f/isochromosome 1 h/HumanChromosomeXBandq10))

(defclass k46_XX_idic!17!!p11.2!
  :label "The 46,XX,idic(17)(p11.2) karyotype"
  :comment "ISCN2009 pg 72 -> 'An isodicentric chromosome composed of
  the long arms of chromosome 17 and the short arm materials between
  the centromeresand the breakpoints in 17p11.2.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/isodicentric 1 h/HumanChromosome17Bandp11.2))

(defclass k46_XX_i!21!!q10!
  :label "The 46,XX,i(21)(q10) karyotype"
  :comment "ISCN2009 pg 72 -> 'An isochromosome of the long arm of
  chromosome 21 has replaced one chromosome 21. There are two copies
  of the long arm of chromosome 21 in the isochromosome and one normal
  copy of chromosome 21. Even though there are effectively three
  copies of the long arm of chromosome 21, the normal chromosome 21 is
  not designated with a (+) sign.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/isochromosome 1 h/HumanChromosome21Bandq10))


;; MARKER CHROMOSOMES
(defclass k47_XX_+mar
  :label "The 47,XX,+mar karyotype"
  :comment "ISCN2009 pg 73 -> 'On additional marker chromosome'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/marker 1))

(defclass k47_XX_t!12_16!!q13_p11.2!_+mar
  :label "The 47,XX,t(12;16)(q13;p11.2),+mar karyotype"
  :comment "ISCN2009 pg 73 -> 'One marker chromosome in addition to
  t(12;16).'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/translocation 1
                   [h/HumanChromosome12Bandq13]
                   [h/HumanChromosome16Bandp11.2])
  (f/marker 1))

(defclass k48_X_t!X_18!!p11.2_q11.2!_+2mar
  :label "The 48,X,t(X;18)(p11.2;q11.2),+2mar karyotype"
  :comment "ISCN2009 pg 73 -> 'Two marker chromosomes in addition to
  t(X;18).'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/translocation 1
                   [h/HumanChromosomeXBandp11.2]
                   [h/HumanChromosome18Bandq11.2])
  (f/marker 2))

;; 47~51,XY,t(11;22)(q24;q12),+1~5mar[cp10]
;; 48,XX,i(17)(q10,+mar1,+mar2[17]/51,XX,i(17)(q10),+mar1x3,+mar2,+mar3[13]

(defclass k47_XX_+der!?!t!?_15!!?_q22!
  :label "The 47,XX,+der(?)t(?;15)(?;q22) karyotype"
  :comment "ISCN2009 pg 73 -> 'The centromere of this abnormal
  chromosome is unknown and hence it is designated der(?), but part of
  the chromosome is composed of the chromosome 15 segemnt distal to
  band 15q22.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/derivative 1 [h/HumanChromosome]
                (e/translocation nil [h/HumanChromosomeBand]
                                 [h/HumanChromosome15Bandq22])))

;; 49,XX,...,+3mar,1dmin
;; 49,XY,...,+3mar,~14dmin
;; 49,XX,...,+3mar,0~34dmin


;; NEOCENTROMERES - ONLY EXISTS IN LONG ISCN STRINGS
;; 47,XX,+der(3)(qter->q28:)
;; 47,XX,der(3)(:p11->q11:),+neo(3)(pter->p11::q11->q26->neo->q26->qter)
;; 47,XX,+inv dup(10)(q25qter)


;; QUADRUPLICATIONS
;; NOTE: It is not possible to indicate the orientations of the
;; segments with the short system!
(defclass k46_XX_qdp!1!!q23q32!
  :label "The 46,XX,qdp(1)(q23q32) karyotype"
  :comment "ISCN2009 pg 75 -> 'Quadruplication of the segment between
  bands 1q23 and 1q32.'
  46,XX,qdp(1)(pter->q32::q23->q32::q23->q32::q23->qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/quadruplication 1
                     h/HumanChromosome1Bandq23.1
                     h/HumanChromosome1Bandq32.3))


;; RING CHROMOSOMES
;; example ring chromosomes derived from one chromosome
(defclass k46_XX_r!7!!p22q36!
  :label "The 46,XX,r(7)(p22q36) karyotype"
  :comment "ISCN2009 pg 75 -> 'Ring chromosome in which breakage and
  reunion have occurred at bands 7p22 and 7q36. The segments distal to
  these breakpoints have been deleted.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/ring 1
          h/HumanChromosome1Bandq23.1 h/HumanChromosome1Bandq32.3))

;; example ring chromosomes derived from more than one chromosome
;; monocentric ring chromosomes
;; (defclass k46_XX_der!1!r!1_3!!p36.1q23_q21q27!
;;   :label "The 46,XX,der(1)r(1;3)(p36.1q23;q21q27) karyotype"
;;   :comment "ISCN2009 pg 75 -> 'A ring composed of chromosome 1 with
;;   breakpoints in 1p36.1 and 1q23, and the acentric segment between
;;   bands 3q21 and 3q27 of chromosome 3.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   ;; (f/derivative 1 h/HumanChromosome1
;;   ;;               (f/ring
;;   ;;                h/HumanChromosome1Bandp36.1 h/HumanChromosome1Bandq23
;;   ;;                h/HumanChromosome3Bandq21 h/HumanChromosome3Bandq27)))
;;   (f/derivative 1 h/HumanChromosome1
;;                 (owl-some f/hasFeature
;;                          (owl-and f/RingChromosome
;;                                  (owl-and
;;                                   (owl-some e/hasProvidingBreakPoint
;;                                            h/HumanChromosome1Bandp36.1
;;                                            h/HumanChromosome1Bandq23)
;;                                   (owl-some e/hasReceivingBreakPoint
;;                                            h/HumanChromosome3Bandq21))
;;                                  (owl-and
;;                                   (owl-some e/hasProvidingBreakPoint
;;                                            h/HumanChromosome3Bandq21
;;                                            h/HumanChromosome3Bandq27)
;;                                   (owl-some e/hasReceivingBreakPoint
;;                                            h/HumanChromosome1Bandp36.1))))))

;; ;; TOFIX - ORDER IS IMPORTANT!
;; (defclass k46_XX_der!1!r!1_3!!p36.1q23_q27q21!
;;   :label "The 46,XX,der(1)r(1;3)(p36.1q23;q27q21) karyotype"
;;   :comment "ISCN2009 pg 75 -> 'A ring with the same breakpoints as in
;;   the previous example, but the orientation of the acentric segment is
;;   reversed.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   ;; (f/derivative 1 h/HumanChromosome1
;;   ;;               (f/ring 1
;;   ;;                       h/HumanChromosome1Bandp36.1 h/HumanChromosome1Bandq23
;;   ;;                       h/HumanChromosome3Bandq27 h/HumanChromosome3Bandq21)))
;;   (f/derivative 1 h/HumanChromosome1
;;                 (owl-some f/hasFeature
;;                          (owl-and f/RingChromosome
;;                                  (owl-and
;;                                   (owl-some e/hasProvidingBreakPoint
;;                                            h/HumanChromosome1Bandp36.1
;;                                            h/HumanChromosome1Bandq23)
;;                                   (owl-some e/hasReceivingBreakPoint
;;                                            h/HumanChromosome3Bandq27))
;;                                  (owl-and
;;                                   (owl-some e/hasProvidingBreakPoint
;;                                            h/HumanChromosome3Bandq21
;;                                            h/HumanChromosome3Bandq27)
;;                                   (owl-some e/hasReceivingBreakPoint
;;                                            h/HumanChromosome1Bandp36.1))))))

;; (defclass k46_XX_der!1!r!1_?!!p36.1q23_?!
;;   :label "The 46,XX,der(1)r(1;?)(p36.1q23;?) karyotype"
;;   :comment "ISCN2009 pg 75 -> 'A ring composed of chromosome 1 with
;;   breakpoints in 1p36.1 and 1q23, and an unknown acentric segment.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (f/derivative 1 h/HumanChromosome1
;;                 (f/ring 1 h/HumanChromosome1Bandp36.1
;;                         h/HumanChromosome1Bandq23 h/HumanChromosomeBand
;;                         h/HumanChromosomeBand)))

;; ;; REDEFINE
;; (defclass k47_XX_+der!?!r!?_3_5!!?_q21q26.2_q13q33!
;;   :label "The 47,XX,+der(?)r(?;3;5)(?;q21q26.2;q13q33) karyotype"
;;   :comment "ISCN2009 pg 75 -> 'In this ring the origin of the
;;   centromere is unknown, but the ring contains the acentric segments
;;   3q21 to 3q26.2 and 5q13 to 5q33.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/addition 1 h/HumanChromosome)
;;   ;; (f/derivative 1 h/HumanChromosome
;;   ;;               (f/ring 1
;;   ;;                       h/HumanChromosomeBand
;;   ;;                       h/HumanChromosome3Bandq21 h/HumanChromosome3Bandq26.2
;;   ;;                       h/HumanChromosome5Bandq13 h/HumanChromosome5Bandq33)))
;;   (f/derivative 1 h/HumanChromosome
;;                 (owl-some f/hasFeature
;;                          (owl-and f/RingChromosome
;;                                  (owl-and
;;                                   (owl-some e/hasProvidingBreakPoint
;;                                            h/HumanChromosomeBand)
;;                                   (owl-some e/hasReceivingBreakPoint
;;                                            h/HumanChromosome3Bandq21))
;;                                  (owl-and
;;                                   (owl-some e/hasProvidingBreakPoint
;;                                            h/HumanChromosome3Bandq21
;;                                            h/HumanChromosome3Bandq26.2)
;;                                   (owl-some e/hasReceivingBreakPoint
;;                                            h/HumanChromosome5Bandq13))
;;                                  (owl-and
;;                                   (owl-some e/hasProvidingBreakPoint
;;                                            h/HumanChromosome5Bandq13
;;                                            h/HumanChromosome5Bandq33)
;;                                   (owl-some e/hasReceivingBreakPoint
;;                                            h/HumanChromosomeBand))))))


;; dicentric ring chromosome
;; 47,XX,+dic r(1;3)(p36.1q32;p24q26.2)

;; tricentric ring chromosome
;; 47,XX,+trc r(1;3;12)(p36.1q32;q26.3p24;p12q23)

(defclass k49_XX_+1_+3_r!7!_+8
  :label "The 49,XX,+1,+3,r(7),+8 karyotype"
  :comment "ISCN2009 pg 76"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 1 h/HumanChromosome1)
  (e/addition 1 h/HumanChromosome3)
  (e/addition 1 h/HumanChromosome8)
  (f/ring 1 h/HumanChromosome7))

;; REDEFINE
(defclass k50_XX_+1_+3_+8_+r
  :label "The 50,XX,+1,+3,+8,+r karyotype"
  :comment "ISCN2009 pg 76"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 1 h/HumanChromosome1)
  (e/addition 1 h/HumanChromosome3)
  (e/addition 1 h/HumanChromosome8)
  ;; needs to be n = 4, in order to include the three other additions
  ;; - since n = 1 makes the ontology inconsistent.
  (e/addition 4 h/HumanChromosome)
  (f/ring 1 h/HumanChromosome))

;;REDEFINE
(defclass k50_XX_+1_+3_+8_+r_+mar
  :label "The 50,XX,+1,+3,+8,+r,+mar karyotype"
  :comment "ISCN2009 pg 76"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 1 h/HumanChromosome1)
  (e/addition 1 h/HumanChromosome3)
  (e/addition 1 h/HumanChromosome8)
  ;; needs to be n = 4, in order to include the three other additions
  ;; - since n = 1 makes the ontology inconsistent.
  (e/addition 4 h/HumanChromosome)
  (f/ring 1 h/HumanChromosome)
  (f/marker 1))

;; 53,XX,..,+r1,+r2
;; 53,XY,...,+5r


;; TELOMERIC ASSIOCIATIONS
;; TODO
(defclass k46_XX_tas!12_13!!q24.3_q34!
  :label "The 46,XX,tas(12;13)(q24.3;q34) karyotype"
  :comment "ISCN2009 pg 77 -> 'Association between the telomeric
  regions of the long arms of chromosomes 12 and 13.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/direct-feature
   1
   (owl-and f/TelomericAssociations
            (owl-and
             (owl-some e/hasProvidingBreakPoint
                       h/HumanChromosome12Bandq24.3)
             (owl-some e/hasReceivingBreakPoint
                       h/HumanChromosome13Bandq34)))))

;; (f/tas h/HumanChromosome12Bandq24.3 h/HumanChromosome13Bandq34)

;; TODO
(defclass k46_XY_tas!X_12_3!!q28_p13q24.3_q29!
  :label "The 46,XY,tas(X;12;3)(q28;p13q24.3;q29) karyotype"
  :comment "ISCN2009 pg 77 -> 'Association between the telomeric
  regions of Xq and 12p, and 12q and 3q.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (f/direct-feature
   1
   (owl-and f/TelomericAssociations
            (owl-and
             (owl-some e/hasProvidingBreakPoint
                       h/HumanChromosomeXBandq28)
             (owl-some e/hasReceivingBreakPoint
                       h/HumanChromosome12Bandp13))
            (owl-and
             (owl-some e/hasProvidingBreakPoint
                       h/HumanChromosome12Bandq24.3)
             (owl-some e/hasReceivingBreakPoint
                       h/HumanChromosome3Bandq29)))))

;; (f/tas h/HumanChromosomeXBandq28
;;        h/HumanChromosome12Bandp13 h/HumanChromosome12Bandq24.3
;;        h/HumanChromosome3Bandq29)

;; TODO
(defclass k46_XX_tas!1_X_12_7!!p36.3_q28p22.3_p13q24.3_p22!
  :label "The 46,XX,tas(1;X;12;7)(p36.3;q28p22.3;p13q24.3;p22) karyotype"
  :comment "ISCN2009 pg 77 -> 'Association between the telomeric
  regions of 1p and Xq, Xp and 12p, and 12q and 7p.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/direct-feature
   1
   (owl-and f/TelomericAssociations
            (owl-and
             (owl-some e/hasProvidingBreakPoint
                       h/HumanChromosome1Bandp36.3)
             (owl-some e/hasReceivingBreakPoint
                       h/HumanChromosomeXBandq28))
            (owl-and
             (owl-some e/hasProvidingBreakPoint
                       h/HumanChromosomeXBandp22.3)
             (owl-some e/hasReceivingBreakPoint
                       h/HumanChromosome12Bandp13))
            (owl-and
             (owl-some e/hasProvidingBreakPoint
                       h/HumanChromosome12Bandq24.3)
             (owl-some e/hasReceivingBreakPoint
                       h/HumanChromosome7Bandp22)))))

;; (f/tas h/HumanChromosome1Bandp36.3
;;        h/HumanChromosomeXBandq28 h/HumanChromosomeXBandp22.3
;;        h/HumanChromosome12Bandp13 h/HumanChromosome12Bandq24.3
;;        h/HumanChromosome7Bandp22)

;; TRANSLOCATIONS
;; reciprocal two-break rearrangement translocations
(defclass k46_XY_t!2_5!!q21_q31!
  :label "The 46,XY,t(2;5)(q21;q31) karyotype"
  :comment "ISCN2009 pg 77 -> 'Breakage and reunion have occurred at
  bands 2q21 and 5q31. The segments distal to these bands have been
  exchanged.'
  46,XY,t(2;5)(2pter->2q21::5q31->5qter;5pter->5q31::2q21->2qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/translocation 1
                   [h/HumanChromosome2Bandq21]
                   [h/HumanChromosome5Bandq31]))

(defclass k46_XY_t!2_5!!p12_q31!
  :label "The 46,XY,t(2;5)(p12;q31) karyotype"
  :comment "ISCN2009 pg 78 -> 'Breakage and reunion have occurred at
  bands 2p12 and 5q31. The segments distal to these bands have been
  exchanged.'
  46,XY,t(2;5)(5qter->5q31::2p12->2qter;5pter->5q31::2p12->2pter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/translocation 1
                   [h/HumanChromosome2Bandp12]
                   [h/HumanChromosome5Bandq31]))

(defclass k46_X_t!X_13!!q27_q12!
  :label "The 46,X,t(X;13)(q27;q12) karyotype"
  :comment "ISCN2009 pg 78 -> 'Breakage and reunion have occurred at
  bands Xq27 and 13q12. The segments distal to these bands have been
  exchanged. Since one of the chromosomes involved in the
  translocation is a sex chromosome, it is designated first. Note that
  the correct designation is 46,X,t(X;13) and not
  46,XX,t(X;13). Similarly, an identical translocation in a male
  should be designated 46,Y,t(X;13) and not 46,XY,t(X;13).'
  46,X,t(X;13)(Xpter->Xq27::13q12->13qter;13pter->13q12::Xq27->Xqter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/translocation 1
                   [h/HumanChromosomeXBandq27]
                   [h/HumanChromosome13Bandq12]))

;; ;; ERROR in book! - there is no resolution band for Y q11.2
;; (defclass k46_t!X_Y!!q22_q11.2!
;;   :label "The 46,t(X;Y)(q22;q11.2) karyotype"
;;   :comment "ISCN2009 pg 78 -> 'A reciprocal translocation between an X
;;   chromosome and a Y chromosome with breakpoints at bands Xq22 and
;;   Yq11.2.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XY)
;;   (e/translocation 1 h/HumanChromosomeXBandq22 h/HumanChromosomeYq11.2))

;; ;; ERROR in book! - there is no resolution band for Y q11.2
;; (defclass k46_t!X_18!!p11.2_q11.2!_t!Y_1!!q11.2_p31!
;;   :label "The 46,t(X;18)(p11.2;q11.2),t(Y;1)(q11.2;p31) karyotype"
;;   :comment "ISCN2009 pg 78 -> 'Two reciprocal translocations, each
;;   involving one sex chromosome. Breakage and reunion have occurred at
;;   bands Xp11.2 and 18q11.2 as well as as at bands Yq11.2 and
;;   1p31. Abnormalities of the X chromosome are listed before those of
;;   the Y chromosome.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XY)
;;   (e/translocation 1 h/HumanChromosomeXBand11.2 h/HumanChromosome18Bandq11.2)
;;   (e/translocation 1 h/HumanChromosomeYBand11.2 h/HumanChromosome1Bandp31))

;; reciprocal three-break rearrangement tranlocations
(defclass k46_XX_t!2_7_5!!p21_q22_q23!
  :label "The 46,XX,t(2;7;5)(p21;q22;q23) karyotype"
  :comment "ISCN2009 pg 78 -> 'The segment on chromosome 2 distal to
  2p21 has been translocated onto chromosome 7 at band 7q22, the
  segment on chromosome 7 distal to 7q22 has been translocated onto
  chromosome 5 at 5q23, and the segment of chromosome 5 distal to 5q23
  has been translocated onto chromosome 2 at 2p21.'
  46,XX,t(2;7;5)(5qter->5q23::2p21->2qter; 7pter7q22::2p21->2pter;
  5pter->5q23::7q22->7qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/translocation 1
                   [h/HumanChromosome2Bandp21]
                   [h/HumanChromosome7Bandq22]
                   [h/HumanChromosome5Bandq23]))

(defclass k46_X_t!X_22_1!!q24_q11.2_p33!
  :label "The 46,X,t(X;22;1)(q24;q11.2;p33) karyotype"
  :comment "ISCN2009 pg 78 -> 'The segment on one chromosome X distal
  to Xq24 has been translocated onto chromosome 22 at band 22q11.2,
  the segment distal to 22q11.2 has been translocated onto chromosome
  1 at 1p33, and the segment distal to 1p33 has been translocated onto
  the chromosome X at Xq24.' 46,X,t(X;22;1)(Xpter->Xq24::1p33_>1pter;
  22pter->22q11.2::Xq24->Xqter; 22qter->22q11.2::1p33->1qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/translocation 1
                   [h/HumanChromosomeXBandq24]
                   [h/HumanChromosome22Bandq11.2]
                   [h/HumanChromosome1Bandp33]))

;; translocation never occurs within the same chromosome therefore
;; assume chromosome 7 are different
(defclass k46_XX_t!3_7_7*!!q21_q22_p13!
  :label "The 46,XX,t(3;7;7*)(q21;q22;p13) karyotype"
  :comment "ISCN2009 pg 78 -> 'The segment on chromosome 2 distal to
  2q21 has been translocated onto chromosome 7 at band 7q22, the
  segment on chromosome 7 distal to 7q22 has been translocated onto
  the homologous chromosome 7 at band 7p13, and the segment distal to
  7p13 on the latter chromosome has been translocated onto chromosome
  2 at 2q21. Underlining is used only to emphasize that the
  chromosomes are homologous. However, this is usually not necessary
  since if the same chromosome 7 had been involved, the resulting
  chromosome 7 would have to be described as a derivative chromosome.'
  46,XX,t(2;7;7*)(2pter->2q21::7*p13->7*pter;
  7pter->7q22::2q21->2qter; 7qter->7q22::7*p13->7*qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/translocation 1
                   [h/HumanChromosome3Bandq21]
                   [h/HumanChromosome7Bandq22]
                   [h/HumanChromosome7Bandp13]))

;; reciprocal four-break and more complex rearrangemnt translocations
(defclass k46_XX_t!3_9_22_21!!p13_q34_q11.2_q21!
  :label "The 46,XX,t(3;9;22;21)(p13;q34;q11.2;q21) karyotype"
  :comment "ISCN2009 pg 79 -> 'The segment of chromosome 3 distal to
  3p13 has been translocated onto chromosome 9 at 9q34; the segment of
  chromosome 9 distal to 9q34 has been translocated into chromosome 22
  at 22q11.2; the segment of chromosome 22 distal to 22q11.2 has been
  translocated onto chromosome 21 at 21q21; and the segment of
  chromosome 21 distal to 21q21 has been translocated onto chromosome
  3 at 3p13.' 46,XX,t(3;9;22;21)(21qter->21q21::3p13->3qter;
  9pter->9q34::3p13->3pter; 22pter->22q11.2::9q34->9qter;
  21pter->21q21::22q11.2->22qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/translocation 1
                   [h/HumanChromosome3Bandp13]
                   [h/HumanChromosome9Bandq34]
                   [h/HumanChromosome22Bandq11.2]
                   [h/HumanChromosome21Bandq21]))

;; as this is a translocation restriction - assume chromosome 9 are distinct
(defclass k46_XX_t!3_9_9*_22!!p13_q22_q34_q11.2!
  :label "The 46,XX,t(3;9;9*;22)(p13;q22;q34;q11.2) karyotype"
  :comment "ISCN2009 pg 79 -> 'Four-break rearrangemnet involving the
  two homologous chromosomes 9. The segment on chromosome 3 distal to
  3p13 has been translocated onto chromosome 9 at band 9q22, the
  segment on chromosome 9 distal to 9q22 has been translocated onto
  the homologous chromosome 9 at 9q34, the segment on the latter
  chromosome 9 distal to 9q34 has been translocated onto chromosome 22
  at 22q11.2, and the segment on chromosome 22 distal to 22q11.2 has
  been translocated onto chromosmoe 3 at 3p13.'
  46,XX,t(3;9;9*;22)(22qter->22q11.2::3p13->3qter;
  9pter->9q22::3p13->3pter; 9*pter->9*q34::9q22->9qter;
  22pter->22q11.2::9*q34->9*ter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/translocation 1
                   [h/HumanChromosome3Bandp13]
                   [h/HumanChromosome9Bandq22]
                   [h/HumanChromosome9Bandq34]
                   [h/HumanChromosome22Bandq11.2]))

(defclass k46_XY_t!5_6!!q13q23_q15q23!
  :label "The 46,XY,t(5;6)(q13q23;q15q23) karyotype"
  :comment "ISCN2009 pg 79 -> 'Four-break rearrangement involving two
  chromosomes. The segment between bands 5q13 and 5q23 in chromosome 5
  and the segment between bands 6q15 and 6q23 in chromosome 6 have
  been exchanged.' 46,XY,t(5;6)(5pter->5q13::6q15->6q23::5q23->5qter;
  6pter->6q15::5q13->5q23::6q23->6qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/translocation 1
                   [h/HumanChromosome5Bandq13]
                   [h/HumanChromosome5Bandq23]
                   [h/HumanChromosome6Bandq15]
                   [h/HumanChromosome6Bandq23]))

;; TODO Order is important - the segment 14q21q24 is inverted!
(defclass k46_XX_t!5_14_9!!q13q23_q24q21_p12p23!
  :label "The 46,XX,t(5;14;9)(q13q23;q24q21;p12p23) karyotype"
  :comment "ISCN2009 pg 79 -> 'Reciprocal six-break translocation of
  three interstitial segments. The segment between bands 5q13 and 5q23
  on chromosome 5 has replaced teh segment between bands 14q21 and
  14q24 on chromosome 14; the segment 14q21q24 has replaced the
  segment between bands 9p12 and 9p23 on chromosome 9; and the segment
  9p12p23 has replaced the segment 5q13q23. The orientations of the
  segments in relation to the centromere are apparent from the order
  of the bands. Thus, the segment 14q21q24 is inverted.'
  46,XX,t(5;14;19)(5pter->5q13::9p12->9p23::5q23->5qter;
  14pter->14q21::5q13->5q23::14q24->14qter; +
  9ter->9p23::14q21->14q21->14q24::9p12->9qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  ;; (e/translocation 1
  ;;                  [h/HumanChromosome5Bandq13 h/HumanChromosome5Bandq23]
  ;;                  [h/HumanChromosome14Bandq24 h/HumanChromosome14Bandq21]
  ;;                  [h/HumanChromosome9Bandp12 h/HumanChromosome9Bandp23]))
  (exactly 1 e/hasDirectEvent
           (owl-and e/Translocation
                   (owl-and
                    (owl-some e/hasProvidingBreakPoint
                             h/HumanChromosome5Bandq13
                             h/HumanChromosome5Bandq23)
                    (owl-some e/hasReceivingBreakPoint
                             h/HumanChromosome14Bandq24
                             h/HumanChromosome14Bandq21))
                   (owl-and
                    (owl-some e/hasProvidingBreakPoint
                             h/HumanChromosome14Bandq24
                             h/HumanChromosome14Bandq21)
                    (owl-some e/hasReceivingBreakPoint
                             h/HumanChromosome9Bandp12
                             h/HumanChromosome9Bandp23))
                   (owl-and
                    (owl-some e/hasProvidingBreakPoint
                             h/HumanChromosome14Bandq24
                             h/HumanChromosome14Bandq21)
                    (owl-some e/hasReceivingBreakPoint
                             h/HumanChromosome5Bandq13
                             h/HumanChromosome5Bandq23))
                   (e/direct-event nil
                                   (e/inversion-pattern
                                    h/HumanChromosome14Bandq24
                                    h/HumanChromosome14Bandq21)))))

;; balanced whole-arm translocations
(defclass k46_XY_t!1_3!!p10_q10!
  :label "The 46,XY,t(1;3)(p10;q10) karyotype"
  :comment "ISCN2009 pg 80 -> 'Reciprocal whole-arm translocation in
  which the short arm of chromosome 1 has been fused at the centromere
  with the long arm of chromosome 3 and the long arm of chromosome 1
  has been fused with the short arm of chromosome 3.'
  46,XY,t(1;3)(1pter->1p10::3q10->3qter;3pter->3p10::1q10->1qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/translocation 1
                   [h/HumanChromosome1Bandp10]
                   [h/HumanChromosome3Bandq10]))

(defclass k46_XY_t!1_3!!p10_p10!
  :label "The 46,XY,t(1;3)(p10;p10) karyotype"
  :comment "ISCN2009 pg 80 -> 'Reciprocal whole-arm translocation in
  which the short arms of chromosomes 1 and 3 and the long arms of
  these chromosomes, respectively, have been fused at the
  centromeres.'
  46,XY,t(1;3)(1pter->1p10::3p10->3pter;1qter->1q10::3q10->3qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/translocation 1
                   [h/HumanChromosome1Bandp10]
                   [h/HumanChromosome3Bandp10]))

;; unbalanced whole-arm translocations
;; (defclass k45_XX_der!1_3!!p10_q10!
;;   :label "The 45,XX,der(1;3)(p10;q10) karyotype"
;;   :comment "ISCN2009 pg 80 -> 'A derivative chromosome chromosome
;;   consisting of the short arm of chromosome 1 and the long arm of
;;   chromosome 3. The missing chromosomes 1 and 3 are not indicated
;;   since they are replaced by the derivative chromosome. The karyotype
;;   thus contains one normal chromosome 1, one normal chromosome 3, and
;;   the der(1;3). The resulting net imbalance of this abnormality is
;;   monosomy for the long arm of chromosome 1 and monosomy for the short
;;   arm of chromosome 3.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (f/derivative 1 h/HumanChromosome1Bandp10 h/HumanChromosome3Bandq10))

;; (defclass k46_XX_+1_der!1_3!!p10_q10!
;;   :label "The 46,XX,+1,der(1;3)(p10;q10) karyotype"
;;   :comment "ISCN2009 pg 80 -> 'A derivative chromosome consisting of
;;   the short arm of chromosome 1 and the long arm of chromosome 3 (same
;;   as above) has replaced one chromosome 1 and one chromosome 3. There
;;   are, however, two normal chromosomes 1, i.e., an additional
;;   chromosome 1 in relation to the expected loss due to the
;;   der(1;3). Consequently, this gain is indicated as +1. The karyotype
;;   thus contains two normal chromosomes 1, one normal chromosome 3, and
;;   the der(1;3). The resulting net imbalance is trisomy for 1p and
;;   monosomy 3p.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/addition 1 h/HumanChromosome1)
;;   (f/derivative 1 h/HumanChromosome1Bandp10 h/HumanChromosome3Bandq10))

;; (defclass k46_XX_der!1_3!!p10_q10!_+3
;;   :label "The 46,XX,der(1;3)(p10;q10),+3 karyotype"
;;   :comment "ISCN2009 pg 80 -> 'A derivative chromosome consisting of
;;   the short arm of chromosome 1 and the long arm of chromosome 3 (same
;;   as above) has replaced one chromosome 1 and one chromosome 3. There
;;   are however, two normal chromosomes3, i.e., an additional chromosome
;;   3 in relation to the expected loss due to the
;;   der(1;3). Consequently, this gain is indicated as +3. The karyotype
;;   thus contains one normal chromosome 1, two normal chromosomes 3, and
;;   the der(1;3). The resulting net imbalance is monosomy for 1q and
;;   trisomy 3q.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/addition 1 h/HumanChromosome3)
;;   (f/derivative 1 h/HumanChromosome1Bandp10 h/HumanChromosome3Bandq10))

;; ;; REDEFINE
;; (defclass k47_XX_+der!1_3!!p10_q10!
;;   :label "The 47,XX,+der(1;3)(p10;q10) karyotype"
;;   :comment "ISCN2009 pg 81 -> 'An extra derivative chromosome
;;   consisting of the short arm of chromosome 1 and the long arm of
;;   chromosome 3 (same as above). There are thus two normal chromosomes
;;   1, two normal chromosomes 3, and the der(1;3). The resulting net
;;   imbalance is trisomy for 1p, and trisomy for 3q.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/addition 1 h/HumanChromosome1)
;;   (e/addition 1 h/HumanChromosome3)
;;   (f/derivative 1 h/HumanChromosome1Bandp10 h/HumanChromosome3Bandq10))

;; (defclass k44_XX_-1_der!1_3!!p10_q10!
;;   :label "The 44,XX,-1,der(1;3)(p10;q10) karyotype"
;;   :comment "ISCN2009 pg 81 -> 'A derivative chromosome consisting of
;;   the short arm of chromosome 1 and the long arm of chromosomes
;;   3 (same as above) has replaced one chromosome 1 and one chromosome
;;   3. There is, however, no normal chromosome 1, indicated as -1 in
;;   relation to the expected presence of one chromosome 1 due to the
;;   der(1;3). The karyotype thus contains no chromosome 1, one normal
;;   chromosome 3, and the der(1;3). The resulting net imbalance is
;;   nullisomy for 1q, monosomy for 1p, and monosomy for 3p.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/deletion 1 h/HumanChromosome1)
;;   (f/derivative 1 h/HumanChromosome1Bandp10 h/HumanChromosome3Bandq10))

;; ;; robertsonian translocations
;; (defclass k45_XX_der!13_21!!q10_q10!
;;   :label "The 45,XX,der(13;21)(q10;q10) karyotype"
;;   :comment "ISCN2009 pg 81 -> 'Breakage and reunion have occurred at
;;   band 13q10 and band 21q10 in the centromeres of chromosomes 13 and
;;   21. The derivative chromosome has replaced one chromosome 13 and one
;;   chromosome 21 and there is no need to indicate the missing
;;   chromosomes. The karyotype thus contains one normal chromosomes 13,
;;   one normal chromosome 21, and the der(13;21). The resulting net
;;   imbalance is loss of the short arms of chromosomes 13 and 21.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (f/derivative 1 h/HumanChromosome13Bandq10 h/HumanChromosome21Bandq10))

;; (defclass k45_XX_rob!13_21!!q10_q10!
;;   :label "The 45,XX,rob(13;21)(q10;q10) karyotype"
;;   :comment "ISCN2009 pg 81 -> 'Breakage and reunion have occurred at
;;   band 13q10 and band 21q10 in the centromeres of chromosomes 13 and
;;   21. The derivative chromosome has replaced one chromosome 13 and one
;;   chromosome 21 and there is no need to indicate the missing
;;   chromosomes. The karyotype thus contains one normal chromosomes 13,
;;   one normal chromosome 21, and the der(13;21). The resulting net
;;   imbalance is loss of the short arms of chromosomes 13 and 21.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (f/robertsonian 1 h/HumanChromosome13Bandq10 h/HumanChromosome21Bandq10))

;; (defclass k46_XX_der!13_21!!q10_q10!_+21
;;   :label "The 46,XX,der(13;21)(q10;q10),+21 karyotype"
;;   :comment "ISCN2009 pg 81 -> 'A derivative chromosome consisting of
;;   the long arm of chromosome 13 and the long arm of chromosome
;;   21 (same as above) has replaced one chromosome 13 and one
;;   chromosome 21. There are, however two normal chromosomes 21, i.e.,
;;   an additional chromosome 21 in relation to the expected loss due to
;;   the der(13;21). Consequently, this gain is indicated as +21. The
;;   karyotype thus contains one normal chromosome 13, two normal
;;   chromosomes 21, and the der(13;21). The resulting net imbalance is
;;   loss of the short arm of chromosome 13 and trisomy for the long arm
;;   of chromosome 21.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/addition 1 h/HumanChromosome21)
;;   (f/derivative 1 h/HumanChromosome13Bandq10 h/HumanChromosome21Bandq10))

;; (defclass k46_XX_rob!13_21!!q10_q10!_+21
;;   :label "The 46,XX,rob(13;21)(q10;q10),+21 karyotype"
;;   :comment "ISCN2009 pg 81 -> 'A derivative chromosome consisting of
;;   the long arm of chromosome 13 and the long arm of chromosome
;;   21 (same as above) has replaced one chromosome 13 and one
;;   chromosome 21. There are, however two normal chromosomes 21, i.e.,
;;   an additional chromosome 21 in relation to the expected loss due to
;;   the der(13;21). Consequently, this gain is indicated as +21. The
;;   karyotype thus contains one normal chromosome 13, two normal
;;   chromosomes 21, and the der(13;21). The resulting net imbalance is
;;   loss of the short arm of chromosome 13 and trisomy for the long arm
;;   of chromosome 21.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/addition 1 h/HumanChromosome21)
;;   (f/robertsonian 1 h/HumanChromosome13Bandq10 h/HumanChromosome21Bandq10))

;; (defclass k46_XX_+13_der!13_21!!q10_q10!
;;   :label "The 45,XX,der(13;21)(q10;q10) karyotype"
;;   :comment "ISCN2009 pg 81 -> 'A derivative chromosome consisting of
;;   the long arm of chromosome 13 and the long arm of chromosome
;;   21 (same as above) has replaced one chromosome 13 and one
;;   chromosome 21. There are, however two normal chromosomes 13, i.e.,
;;   an additional chromosome 13 in relation to the expected loss due to
;;   the der(13;21). Consequently, this gain is indicated as +13. The
;;   karyotype thus contains one normal chromosome 13, two normal
;;   chromosomes 21, and the der(13;21). The resulting net imbalance is
;;   loss of the short arm of chromosome 21 and trisomy for the long arm
;;   of chromosome 13.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/addition 1 h/HumanChromosome13)
;;   (f/derivative 1 h/HumanChromosome13Bandq10 h/HumanChromosome21Bandq10))

;; (defclass k46_XX_+13_rob!13_21!!q10_q10!
;;   :label "The 46,XX,+13,rob(13;21)(q10;q10) karyotype"
;;   :comment "ISCN2009 pg 81 -> 'A derivative chromosome consisting of
;;   the long arm of chromosome 13 and the long arm of chromosome
;;   21 (same as above) has replaced one chromosome 13 and one
;;   chromosome 21. There are, however two normal chromosomes 13, i.e.,
;;   an additional chromosome 13 in relation to the expected loss due to
;;   the der(13;21). Consequently, this gain is indicated as +13. The
;;   karyotype thus contains one normal chromosome 13, two normal
;;   chromosomes 21, and the der(13;21). The resulting net imbalance is
;;   loss of the short arm of chromosome 21 and trisomy for the long arm
;;   of chromosome 13.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/addition 1 h/HumanChromosome13)
;;   (f/robertsonian 1 h/HumanChromosome13Bandq10 h/HumanChromosome21Bandq10))

;; (defclass k46_XX_+21_der!21_21!!q10_q10!
;;   :label "The 46,XX,+21,der(21;21)(q10;q10) karyotype"
;;   :comment "ISCN2009 pg 82 -> 'A derivative chromosome composed of the
;;   long arms of chromosome 21. There are two copies of the long arm of
;;   chromosome 21 in the derivative chromosome and one normal chromosome
;;   21, indicated by the 46 count. The normal chromosome 21 is
;;   designated with a (+)'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/addition 1 h/HumanChromosome21)
;;   (f/derivative 1 h/HumanChromosome21Bandq10 h/HumanChromosome21Bandq10))

;; (defclass k46_XX_+21_rob!21_21!!q10_q10!
;;   :label "The 46,XX,+21,rob(21;21)(q10;q10) karyotype"
;;   :comment "ISCN2009 pg 82 -> 'A derivative chromosome composed of the
;;   long arms of chromosome 21. There are two copies of the long arm of
;;   chromosome 21 in the derivative chromosome and one normal chromosome
;;   21, indicated by the 46 count. The normal chromosome 21 is
;;   designated with a (+)'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/addition 1 h/HumanChromosome21)
;;   (f/robertsonian 1 h/HumanChromosome21Bandq10 h/HumanChromosome21Bandq10))

;; jumping translocations
;; 46,XX,t(4;7)(q35;q11.2)[6]/46,XX,t(1;7)(p36.3;q11.2)[4]/46,XX,t(7;9)(q11.2;p24)

;; TRICENTRIC CHROMOSOMES
;; TOFIX - hardcoded
(defclass k44_XX_trc!4_12_9!!q31.2_q22p13_q34!
  :label "The 44,XX,trc(4;12;9)(q31.2;q22p13;q34) karyotype"
  :comment "ISCN2009 pg 82 -> 'A tricentric chromosome in which band
  4q31.1 is fused with 12q222 and 12p13 is fused with 9q34.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (f/tricentric 1 h/HumanChromosome4Bandq31.2
                h/HumanChromosome12Bandq22 h/HumanChromosome12Bandp13
                h/HumanChromosome9Bandq34))


;; TRIPLICATIONS
;; NOTE: It is not possible to indicate the orientations of the
;; segments with the short system!
(defclass k46_XX_trp!1!!q21q32!
  :label "The 46,XX,trp(1)(q21q32) karyotype"
  :comment "ISCN2009 pg 83 -> 'Direct triplication of the segment
  between bands 1q21 and 1q32, one of several possible orientations of
  the triplications of this segment.'
  46,XX,trp(1)(pter->q32::q21->q32::q21->qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/triplication 1
                  h/HumanChromosome1Bandq21
                  h/HumanChromosome1Bandq32))

(defclass k46_XX_invtrp!1!!q32q21!
  :label "The 46,XX,inv trp(1)(q32q21) karyotype"
  :comment "ISCN2009 pg 83 -> 'Inverted triplication of the segment
  between bands 1q21 and 1q32.'
  46,XX,inv trp(1)(pter->q32::q32->q21::q21->qter)"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/triplication 1
                  h/HumanChromosome1Bandq32
                  h/HumanChromosome1Bandq21))


;; MULTIPLE COPIES OF REARRANGED CHROMOSOMES
(defclass k46_XX_del!6!!q13q23!x2
  :label "The 46,XX,del(6)(q13q23)x2 karyotype"
  :comment "ISCN2009 pg 83 -> 'Two deleted chromosomes 6 with
  breakpoints at bands 6q13 and 6q23, and no normal chromosomes
  6. Since the two abnormal chromosomes replace the two normal
  chromosomes, there is no need to indicate the missing normal
  chromosomes.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/deletion 2
              h/HumanChromosome6Bandq13
              h/HumanChromosome6Bandq23))

;; REDEFINE
(defclass k48_XX_+del!6!!q13q23!x2
  :label "The 48,XX,+del(6)(q13q23)x2 karyotype"
  :comment "ISCN2009 pg 83 -> 'Two normal chromosomes 6 plus two
  additional deleted chromosomes 6 with breakpoints at bands 6q13 and
  6q23.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 2 h/HumanChromosome6)
  (e/deletion 2
              h/HumanChromosome6Bandq13
              h/HumanChromosome6Bandq23))

;; REDEFINE
(defclass k47_XX_del!6!!q13q23!x2_+del!6!!q13q23!
  :label "The 47,XX,del(6)(q13q23)x2,+del(6)(q13q23) karyotype"
  :comment "ISCN2009 pg 83 -> 'There are three copies of a deleted
  chromosome 6 and no normal chromosome 6, i.e., two of the deleted
  chromosomes replace the two normal chromosomes 6. Note that the
  supernumerary deleted chromosome has to be preceded by a plus
  sign.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 1 h/HumanChromosome6)
  (e/deletion 3
              h/HumanChromosome6Bandq13
              h/HumanChromosome6Bandq23))

(defclass k48_XX_del!6!!q13q23!x2_+7_+7
  :label "The 48,XX,del(6)(q13q23)x2,+7,+7 karyotype"
  :comment "ISCN2009 pg 83 -> 'Two deleted chromosomes 6 replace the
  two normal chromosomes6; in addition, there are two extra
  chromosomes 7.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 2 h/HumanChromosome7)
  (e/deletion 2
              h/HumanChromosome6Bandq13
              h/HumanChromosome6Bandq23))

;; REDEFINE
(defclass k48_XX_t!8_14!!q24.1_q32!_+der!14!t!8_14!x2
  :label "The 48,XX,t(8;14)(q24.1;q32),+der(14)t(8;14)x2 karyotype"
  :comment "ISCN2009 pg 83 -> 'A balanced t(8;14) plus two additional
  copies of the derivative chromosome 14.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/translocation 1 [h/HumanChromosome8Bandq24.1]
                   [h/HumanChromosome14Bandq32])
  (e/addition 2 h/HumanChromosome14)
  (f/derivative 2 [h/HumanChromosome14]
                (e/translocation nil [h/HumanChromosome8Bandq24.1]
                                 [h/HumanChromosome14Bandq32])))

(defclass k92_XXXX_t!8_14!!q24.1_q32!x2
  :label "The 92,XXXX,t(8;14)(q24.1;q32)x2 karyotype"
  :comment "ISCN2009 pg 83 -> 'A tetraploid clone with two balanced
  t(8;14). The two derivative chromosomes 8 and 14 replace two normal
  chromosomes 8 and 14. Thus, there are two normal chromosomes 8 and
  14.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k92_XXXX)
  (e/translocation 2
                   [h/HumanChromosome8Bandq24.1]
                   [h/HumanChromosome14Bandq32]))

;; REDEFINE
(defclass k94_XXYY_t!8_14!!q24.1_q32!x2_+der!14!t!8_14!x2
  :label "The 94,XXYY,t(8;14)(q24.1;q32)x2,+der(14)t(8;14)x2 karyotype"
  :comment "ISCN2009 pg 83 -> 'A hypertetraploid clone with two
  balanced t(8;14) plus two additional copies of the derivative
  chromosome 14. There are two normal chromosomes 8 and 14.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k92_XXYY)
  (e/translocation 2 [h/HumanChromosome8Bandq24.1]
                   [h/HumanChromosome14Bandq32])
  (e/addition 2 h/HumanChromosome14)
  (f/derivative 2 [h/HumanChromosome14]
                (e/translocation nil [h/HumanChromosome8Bandq24.1]
                                 [h/HumanChromosome14Bandq32])))

;; REDEFINE
(defclass k93_XXYY_t!8_14!!q24.1q32!x2_der!14!t!8_14!x2_+der!14!t!8_14!
  :label "The
  93,XXYY,t(8;14)(q24.1;q32)x2,der(14)t(8;14)x2,+der(14)t(8;14)
  karyotype"
  :comment "ISCN2009 pg 83 -> 'A hypertetraploid clone with two
  balanced t(8;14) and three extra copies of the derivative chromosome
  14, i.e. there are in total five der(14), four of which replace the
  normal chromosomes 14; consequently there is no normal chromosome
  14.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k92_XXYY)
  (e/addition 1 h/HumanChromosome14)
  (e/translocation 2 [h/HumanChromosome8Bandq24.1]
                   [h/HumanChromosome14Bandq32])
  (f/derivative 2 [h/HumanChromosome14]
                (e/translocation nil [h/HumanChromosome8Bandq24.1]
                                 [h/HumanChromosome14Bandq32])))

;; REDEFINE
(defclass k94_XXYY_t!8_14!!q24.1_q32!x2_+14_der!14!t!8_14!x2_+der!14!t!8_14!
  :label "The
  94,XXYY,t(8;14)(q24.1;q32)x2,+14,der(14)t(8;14)x2,+der(14)t(8;14)
  karyotype"
  :comment "ISCN2009 pg 84 -> 'A hypertetraploid clone with two
  balanced t(8;14), three extra copies of the derivative chromosome
  14, and one normal chromosome 14.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k92_XXYY)
  (e/translocation 2 [h/HumanChromosome8Bandq24.1]
                   [h/HumanChromosome14Bandq32])
  (e/addition 2 h/HumanChromosome14)
  (f/derivative 2 [h/HumanChromosome14]
                (e/translocation nil [h/HumanChromosome8Bandq24.1]
                                 [h/HumanChromosome14Bandq32])))

(defclass k47_XX_+8_i!8!!q10!x2
  :label "The 47,XX,+8,i(8)(q10)x2 karyotype"
  :comment "ISCN2009 pg 83 -> 'Alternative descriptions of the same
  chromosome complement with one normal chromosome 8 and two copies of
  an isochromosome for the long arm of chromosome 8.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 2 h/HumanChromosome8)
  (f/isochromosome 2 h/HumanChromosome8Bandq10))

(defclass k47_XX_i!8!!q10!_+i!8!!q10!
  :label "The 47,XX,i(8)(q10),+i(8)(q10) karyotype"
  :comment "ISCN2009 pg 83 -> 'Alternative descriptions of the same
  chromosome complement with one normal chromosome 8 and two copies of
  an isochromosome for the long arm of chromosome 8.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/addition 2 h/HumanChromosome8)
  (f/isochromosome 2 h/HumanChromosome8Bandq10))

;; OTHER EXAMPLES

;; example numerical constitutional allosomal abnormal karyotypes
(defclass k45_Y
  :label "The 45,Y karyotype"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/deletion 1 h/HumanChromosomeX))))
(defclass k46_YY
  :label "The 46,YY karyotype"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/deletion 1 h/HumanChromosomeX)
            (e/addition 1 h/HumanChromosomeY))))
(defclass k47_YYY
  :label "The 47,YYY karyotype"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/deletion 1 h/HumanChromosomeX)
            (e/addition 2 h/HumanChromosomeY))))
(defclass k48_XXXX
  :label "The 48,XXXX karyotype"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XX)
            (e/addition 2 h/HumanChromosomeX))))
(defclass k48_XXYY
  :label "The 48,XXYY karyotype"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/addition 1 h/HumanChromosomeX)
            (e/addition 1 h/HumanChromosomeY))))
(defclass k48_XYYY
  :label "The 48,XYYY karyotype"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/addition 2 h/HumanChromosomeY))))
(defclass k48_YYYY
  :label "The 48,YYYY karyotype"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/deletion 1 h/HumanChromosomeX)
            (e/addition 3 h/HumanChromosomeY))))
(defclass k49_XXXXX
  :label "The 49,XXXXX karyotype"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XX)
            (e/addition 3 h/HumanChromosomeX))))
(defclass k49_XXXXY
  :label "The 49,XXXXY karyotype"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/addition 3 h/HumanChromosomeX))))
(defclass k49_XXXYY
  :label "The 49,XXXYY karyotype"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/addition 2 h/HumanChromosomeX)
            (e/addition 1 h/HumanChromosomeY))))
(defclass k49_XXYYY
  :label "The 49,XXYYY karyotype"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/addition 1 h/HumanChromosomeX)
            (e/addition 2 h/HumanChromosomeY))))
(defclass k49_XYYYY
  :label "The 49,XYYYY karyotype"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/addition 3 h/HumanChromosomeY))))
(defclass k49_YYYYY
  :label "The 49,YYYYY karyotype"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/deletion 1 h/HumanChromosomeX)
            (e/addition 4 h/HumanChromosomeY))))

;; more examples from the ISCN2009
(defclass k46_XX_inv!2!!p21q31!
  :label "The 46,XX,inv(2)(p21q31) karyotype"
  :comment "ISCN2009 pg 42"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/inversion 1
               h/HumanChromosome2Bandp21
               h/HumanChromosome2Bandq31))

(defclass k46_XX_inv!2!!p13p23!
  :label "The 46,XX,inv(2)(p13p23) karyotype"
  :comment "ISCN2009 pg 42"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/inversion 1
               h/HumanChromosome2Bandp13
               h/HumanChromosome2Bandp23))

(defclass k46_XY_t!12_16!!q13_p11.1!
  :label "The 46,XY,t(12;16)(q13;p11.1) karyotype"
  :comment "ISCN2009 pg 43"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/translocation 1
                   [h/HumanChromosome12Bandq13]
                   [h/HumanChromosome16Bandp11.1]))

(defclass k46_X_t!X_18!!p11.1_q11.1!
  :label "The 46,X,t(X;18)(p11.1;q11.1) karyotype"
  :comment "ISCN2009 pg 43"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/translocation 1
                   [h/HumanChromosomeXBandp11.1]
                   [h/HumanChromosome18Bandq11.1]))

(defclass k46_X_ins!5_X!!p14_q21q25!
  :label "The 46,X,ins(5;X)(p14;q21q25) karyotype"
  :comment "ISCN2009 pg 43"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/insertion 1
               [h/HumanChromosome5Bandp14]
               [h/HumanChromosomeXBandq21
                h/HumanChromosomeXBandq25]))

(defclass k46_XX_ins!2!!q13p13p23!
  :label "The 46,XX,ins(2)(q13p13p23) karyotype"
  :comment "ISCN2009 pg 43 -> 'Direct insertion of the short-arm
  segment between bands 2p13 and 2p23 into the long arm at band
  2q13.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/insertion 1
               [h/HumanChromosome2Bandq13]
               [h/HumanChromosome2Bandp13
                h/HumanChromosome2Bandp23]))

(defclass k46_XX_ins!2!!q13p23p13!
  :label "The 46,XX,ins(2)(q13p23p13) karyotype"
  :comment "ISCN2009 pg 43 -> 'Inverted insertion of the short-arm
  segment between bands 2p13 and 2p23 into the long arm at band
  2q13. Because the insertion is inverted, band 2p23 is now proximal
  and band 2p13 distal to the centromere.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/insertion 1
               [h/HumanChromosome2Bandq13]
               [h/HumanChromosome2Bandp23
                h/HumanChromosome2Bandp13]))

(defclass k46_XX_t!9_22_17!!q34_q11.2_q22!
  :label "The 46,XX,t(9;22;17)(q34;q11.2;q22) karyotype"
  :comment "ISCN2009 pg 43 -> 'The segment of chromosome 9 distal to
  9q34 has been translocated onto chromosome 22 at band 22q11.2, the
  segment of chromosome 22 distal to 22q11.2 has been translocated
  onto chromosome 17 at 17q22, and the segment of chromosome 17 distal
  has been translocated onto chromosome 9 at 9q34.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/translocation 1
                   [h/HumanChromosome9Bandq34]
                   [h/HumanChromosome22Bandq11.2]
                   [h/HumanChromosome17Bandq22]))

(defclass k46_XY_t!X_15_18!!p11.1_p11.1_q11.1!
  :label "The 46,XY,t(X;15;18)(p11.1;p11.1;q11.1) karyotype"
  :comment "ISCN2009 pg 43 -> 'The segment of the X chromosome distal
  to Xp11.1 hass been translocated onto chromosome15 at band 15p11.1,
  the segment of chromosome 15 distal to 15p11.1 has been translocated
  onto chromosome 18 at 18q11.1, and the segment of chromosome 18
  distal to 18q11.2 has been translocated to Xp11.1.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/translocation 1
                   [h/HumanChromosomeXBandp11.1]
                   [h/HumanChromosome15Bandp11.1]
                   [h/HumanChromosome18Bandq11.1]))

(defclass k46_XX_t!3_9_22_21!!p13_q34_q11.2_q21!
  :label "The 46,XX,t(3;9;22;21)(p13;q34;q11.2;q21) karyotype"
  :comment "ISCN2009 pg 44 -> 'The segment of chromosome 3 distal to
  3p13 has been teanslocated onto chromosome 9 at 9q34, the segment of
  chromosome 9 distal to 9q34 has been translocated onto chromosome 22
  at 22q11.2, the segment of chromosome 22 distal to 22q11.2 has been
  translocated onto chromosome 21 at 21q21 and the segment of
  chromsome 21 distal to 21q21 has been translocated onto chromosome 3
  3p13.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/translocation 1
                   [h/HumanChromosome3Bandp13]
                   [h/HumanChromosome9Bandq34]
                   [h/HumanChromosome22Bandq11.2]
                   [h/HumanChromosome21Bandq21]))

(defclass k46_XY_t!5_6!!q13q23_q15q23!
  :label "The 46,XY,t(5;6)(q13q23;q15q23) karyotype"
  :comment "ISCN2009 pg 44 -> 'Reciprocal translocation of two
  interstitial segments. The segments between bands 5q13 and 5q23 of
  chromosome 5 and between 6q15 and 6q23 of choromosome 6 have been
  exchanged.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/translocation 1
                   [h/HumanChromosome5Bandq13 h/HumanChromosome5Bandq23]
                   [h/HumanChromosome6Bandq15 h/HumanChromosome6Bandq23]))

;; pg 49
;; 45,XX,-?21
;; 47,XX,+?8
;; 46,XX,del(1)(q2?)
;; 46,XY,del(1)(q2?3)
;; 46,XX,del(1)(q?2)
;; 46,XY,del(1)(q?23)

(defclass k46_XX_del!1!!q?!
  :label "The 46,XX,del(1)(q?) karyotype"
  :comment "ISCN2009 pg 49 -> 'The break is in the long arm of
  chromosome 1, but neither the region nor the band can be
  identified. This aberration is often described as 1q-, a symbol that
  may be useful in text but should not be used in karyotype
  nomenclature.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/deletion 1 h/HumanChromosome1Bandq))

;; pg 49
;; 46,XX,?del(1)(q23)
;; 46,XX,der(1)?t(1;3)(p22;q13)

(defclass k47_X_t!X_13!!q27_q12!_inv!10!!p13q22!_+21
  :label "The 47,X,t(X;13)(q27;q12),inv(10)(p13q22),+21 karyotype"
  :comment "ISCN2009 pg 52 -> 'The sex chromosome abnormality is
  presented first, followed by the autosomal abnormalities in
  chromosome number order, irrespective of whether the aberrations are
  numerical or structural.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/translocation 1
                   [h/HumanChromosomeXBandq27]
                   [h/HumanChromosome13Bandq12])
  (e/inversion 1
               h/HumanChromosome10Bandp13
               h/HumanChromosome10Bandq22)
  (e/addition 1 h/HumanChromosome21))

(defclass k47_Y_t!X_13!!q27_q12!_inv!10!!p13q22!_+21
  :label "The 47,Y,t(X;13)(q27;q12),inv(10)(p13q22),+21 karyotype"
  :comment "ISCN2009 pg 52 -> 'The same karyotype as in the previous
  example in a male.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XY)
  (e/translocation 1
                   [h/HumanChromosomeXBandq27]
                   [h/HumanChromosome13Bandq12])
  (e/inversion 1
               h/HumanChromosome10Bandp13
               h/HumanChromosome10Bandq22)
  (e/addition 1 h/HumanChromosome21))

;; ;; ERROR in book! - there is no resolution band for Y q11.2
;; (defclass k46_t!X_18!!p11.1_q11.2!_t!Y_1!!q11.2_p13!
;;   :label "The 46,t(X;18)(p11.1;q11.2),t(Y;1)(q11.2;p13) karyotype"
;;   :comment "ISCN2009 pg 52 -> 'The abnormality involving the X
;;   chromosome is listed before that of the Y chromosome.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XY)
;;   (e/translocation 1 2 h/HumanChromosomeXBandp11.1 h/HumanChromosome18Bandq11.2)
;;   (e/translocation 1 2 h/HumanChromosomeYBandq11.2 h/HumanChromosome1Bandp13)
;;   )

;; ;; ERROR in book! - there is no resolution band for Y q11.2
;; (defclass k48_X_t!Y_12!!q11.2_p12!_del!6!!q11!_+8_t!9_22!!q34_q11.2!_+17_-21_+22
;;   :label "The
;;   48,X,t(Y;12)(q11.2;p12),del(6)(q11),+8,t(9;22)(q34;q11.2),+17,-21,+22
;;   karyotype"
;;   :comment "ISCN2009 pg 52 -> 'The translocation involving the Y
;;   chromosome is presented first, followed by all autosomal
;;   abnormalities in strict chromosome number order.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XY)
;;   (e/translocation 1 2 h/HumanChromosomeYBandq11.2 h/HumanChromosome12Bandp12)
;;   (e/deletion 1 h/HumanChromosome6Bandq11)
;;   (e/addition 1 h/HumanChromosome8)
;;   (e/translocation 1 2 h/HumanChromosome9Bandq34 h/HumanChromosome22Bandq11.2)
;;   (e/addition 1 h/HumanChromosome17)
;;   (e/deletion 1 h/HumanChromosome21)
;;   (e/addition 1 h/HumanChromosome22)
;; )

(defclass k49_X_inv!X!!p21q26!_+3_inv!3!!q21q26.2!_+7_+10_-20_del!20!!q11.2!_+21
  :label "The
  49,X,inv(X)(p21q26),+3,inv(3)(q21q26.2),+7,+10,-20,del(20)(q11.2),+21
  karyotype"
  :comment "ISCN2009 pg 52 -> 'The inversion of the X chromosome is
  listed first, The extra chromosome 3 is presented before the
  inversion of chromosome 3 and the monosomy 20 before the deletion of
  chromosome 20.'"
  :subclass ISCNExampleKaryotype
  (owl-some b/derivedFrom b/k46_XX)
  (e/inversion 1
               h/HumanChromosomeXBandp21
               h/HumanChromosomeXBandq26)
  (e/addition 1 h/HumanChromosome3)
  (e/inversion 1
               h/HumanChromosome3Bandq21
               h/HumanChromosome3Bandq26.2)
  (e/addition 1 h/HumanChromosome7)
  (e/addition 1 h/HumanChromosome10)
  (e/deletion 1 h/HumanChromosome20)
  (e/deletion 1 h/HumanChromosome20Bandq11.2)
  (e/addition 1 h/HumanChromosome21))

;; REDEFINE
;; (defclass
;; k50_XX_+1_del!1!!p13!_+dup!1!!q21q32!_+inv!1!!p31q41!_+8_r!10!!p12q25!_-21
;;   :label "The
;;   50,XX,+1,del(1)(p13),+dup(1)(q21q32),+inv(1)(p31q41),+8,r(10)(p12q25),-21
;;   karyotype"
;;   :comment "ISCN2009 pg 52 -> 'There are four abnormalities
;;   involving diffrent copies of chromosome 1. The numerical change
;;   is presented first, followed by the structural aberrations listed
;;   in alphabetical order: del, dup, inv.'"
;;   :subclass ISCNExampleKaryotype
;;   (owl-some b/derivedFrom b/k46_XX)
;;   (e/addition 3 h/HumanChromosome1)
;;   (e/deletion 1 h/HumanChromosome1Bandp13)
;;   (e/direct-duplication 1 h/HumanChromosome1Bandq21 h/HumanChromosome1Bandq32)
;;   (e/inversion 1 h/HumanChromosome1Bandp31 h/HumanChromosome1Bandq41)
;;   (e/addition 1 h/HumanChromosome8)
;;   (f/ring 1 h/HumanChromosome10Bandp12 h/HumanChromosome10Bandq25)
;;   (e/deletion 1 h/HumanChromosome21))

;; 46,XX,der(8)ins(8;?)(p23;?)del(8)(q22)

) ;; ends as-disjoint

;; ;; implement closure axiom on each ISCNExampleKaryotype
;; (doseq [clazz (direct-subclasses iscnexamples ISCNExampleKaryotype)]
;;   (let [parents (superclasses iscnexamples clazz)
;;         restrictions (filter
;;                       #(instance?
;;                         org.semanticweb.owlapi.model.OWLRestriction %) parents)
;;         events (filter
;;               #(= (.getProperty %) e/hasDirectEvent) restrictions)
;;         axioms (map #(.getFiller %) events)]

;;     (if (> (count axioms) 0)
;;       (refine clazz
;;               :subclass (owl-only e/hasDirectEvent (apply owl-or axioms))))))

;; TODO
(defdproperty hasMosiac)

;; mos 47,XXY[10]/46,XY[20]
(defclass kmos_47_XXY!10!_46_XY!20!
  :label "The mos 47,XXY[10]/46,XY[20] karyotype"
  :comment "ISCN2009 pg 56 -> 'TODO'"
  :subclass ISCNExampleKaryotype
  (owl-and
   k47_XXY
   (data-has-value hasMosiac (literal 10)))
  (owl-and
   b/k46_XY
   (data-has-value hasMosiac (literal 20))))

;; (println "TOTAL" (count (direct-subclasses iscnexamples ISCNExampleKaryotype)))
