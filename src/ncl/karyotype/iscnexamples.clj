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

(ns ncl.karyotype.iscnexamples
  (:use [tawny.owl])
  (:require [tawny [reasoner :as r]]
            [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]
            [ncl.karyotype [events :as e]]
            [ncl.karyotype [features :as f]]
            [ncl.karyotype [named :as n]]
            )
  )

(defontology iscnexamples
  :iri "http://ncl.ac.uk/karyotype/iscnexamples"
  :prefix "iex:"
  )

;; import all ncl.karyotype axioms
(owlimport k/karyotype)
(owlimport h/human)
(owlimport e/events)
(owlimport n/named)

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
  (owlsome n/derivedFrom n/k23_X)
  (e/addition 1 h/HumanChromosome4)
  (e/addition 1 h/HumanChromosome6)
  (e/addition 1 h/HumanChromosome21))
;; ;; example triploid karyotype
;; (defclass k71_XXX_+8_+10
;;   :label "The 71,XXX,+8,+10 karyotype"
;;   :comment "ISCN2009 pg 55 -> 'A near-triploid karyotype with four
;;   copies of chromosomes 8 and 10, and three copies of all other
;;   chromosomes.'"
;;   :subclass ISCNExampleKaryotype
;;   (owlsome n/derivedFrom #TODO)
;;   (e/addition 1 h/HumanChromosome8)
;;   (e/addition 1 h/HumanChromosome10))
;; ;; example tetraploid karyotype
;; (defclass k89_XXYY_-1_-3_-5_+8_-21
;;   :label "The 89,XXYY,-1,-3,-5,+8,-21 karyotype"
;;   :comment "ISCN2009 pg 55 -> 'A near-tetraploid karyotype with three
;;   copies of chromosomes 1, 3, 5, and 21, five copies of chromosome 8,
;;   and four copies of all other autosomes.'"
;;   :subclass ISCNExampleKaryotype
;;   (owlsome n/derivedFrom #TODO)
;;   (e/addition 1 h/HumanChromosome8)
;;   (e/deletion 1 h/HumanChromosome1)
;;   (e/deletion 1 h/HumanChromosome3)
;;   (e/deletion 1 h/HumanChromosome5)
;;   (e/deletion 1 h/HumanChromosome21))


;; example constitutional sex chromosome abnormalities
(defclass k45_X
  :label "The 45,X karyotype"
  :comment "ISCN2009 pg 56 -> 'A karyotype with one X
  chromosome (Turner syndrome).'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XN) (e/deletion 1 h/HumanAllosome))))

(defclass k47_XXY
  :label "The 47,XXY karyotype"
  :comment "ISCN2009 pg 56 -> 'A karyotype with two X
  chromosomes and one Y chromosome (Klinefelter syndrome).'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (e/addition 1 h/HumanChromosomeX))))

(defclass k47_XXX
  :label "The 47,XXX karyotype"
  :comment "ISCN2009 pg 56 -> 'A karyotype with three X
  chromosomes.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XX) (e/addition 1 h/HumanChromosomeX))))

(defclass k47_XYY
  :label "The 47,XYY karyotype"
  :comment "ISCN2009 pg 56 -> 'A karyotype with one X
  chromosome and two Y chromosomes.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (e/addition 1 h/HumanChromosomeY))))

(defclass k48_XXXY
  :label "The 48,XXXY karyotype"
  :comment "ISCN2009 pg 56 -> 'A karyotype with three X
  chromosomes and one Y chromosome.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (e/addition 1 h/HumanChromosomeX))))

;; example acquired sex chromosome abnormalities
(defclass k47_XX_+X
  :label "The 47,XX,+X karyotype"
  :comment "ISCN2009 pg 56 -> 'A tumor karyotype in a female with an
  additional X chromosome.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/addition 1 h/HumanChromosomeX))
(defclass k45_X_-X
  :label "The 45,X,-X karyotype"
  :comment "ISCN2009 pg 56 -> 'A tumor karyotype in a female with loss
  of one X chromosome.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/deletion 1 h/HumanChromosomeX))
(defclass k45_X_-Y
  :label "The 45,X,-Y karyotype"
  :comment "ISCN2009 pg 56 -> 'A tumor karyotype in a male with loss
  of the Y chromosome.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (e/deletion 1 h/HumanChromosomeY))
(defclass k45_Y_-X
  :label "The 45,Y,-X karyotype"
  :comment "ISCN2009 pg 57 -> 'A tumor karyotype in a male with loss
  of the X chromosome.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (e/deletion 1 h/HumanChromosomeX))
(defclass k48_XY_+X_+Y
  :label "The 48,XY,+X,+Y karyotype"
  :comment "ISCN2009 pg 57 -> 'A tumor karyotype in a male with one
  additional X and one additional Y chromosome.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (e/addition 1 h/HumanChromosomeX)
  (e/addition 1 h/HumanChromosomeY))

;; example acquired chromosome abnormalities with a constitutional sex chromosome abnormalities
(defclass k48_XXYc_+X
  :label "The 48,XXYc,+X karyotype"
  :comment "ISCN2009 pg 57 -> 'Tumor cells with an acquired additional
  X chromosome in a patient with Klinefelter syndrome.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (e/addition 1 h/HumanChromosomeX))) ;;aka 47,XXY
  (e/addition 1 h/HumanChromosomeX))
(defclass k46_Xc_+X
  :label "The 46,Xc,+X karyotype"
  :comment "ISCN2009 pg 57 -> 'Tumor cells with an acquired additional
  X chromosome in a patient with Turner syndrome.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XN) (e/deletion 1 h/HumanAutosome))) ;;aka 45,X
  (e/addition 1 h/HumanChromosomeX))
(defclass k46_XXYc_-X
  :label "The 46,XXYc,-X karyotype"
  :comment "ISCN2009 pg 57 -> 'Tumor cells with an acquired loss of
  one X chromosome in a patient with Klinefelter syndrome.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (e/addition 1 h/HumanChromosomeX))) ;;aka 47,XXY    
  (e/deletion 1 h/HumanChromosomeX))
(defclass k44_Xc_-X
  :label "The 44,Xc,-X karyotype"
  :comment "ISCN2009 pg 57 -> 'Tumor cells with an acquired loss of
  the X chromosome in a patient with Turner syndrome.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XN) (e/deletion 1 h/HumanAllosome))) ;;aka 45,X 
  (e/deletion 1 h/HumanChromosomeX))
(defclass k46_Xc_+21
  :label "The 46,Xc,+21 karyotype"
  :comment "ISCN2009 pg 57 -> 'Tumor cells with an acquired eaxtra
  chromosome 21 in a patient with Turner syndrome.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XN) (e/deletion 1 h/HumanAllosome))) ;;aka 45,X
  (e/addition 1 h/HumanChromosome21))

;; example acquired autosomal abnormalities
(defclass k47_XX_+21
  :label "The 47,XX,+21 karyotype"
  :comment "ISCN2009 pg 57 -> 'A karyotype with trisomy 21.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/addition 1 h/HumanChromosome21))
(defclass k48_XX_+13_+21
  :label "The 48,XX,+13,+21 karyotype"
  :comment "ISCN2009 pg 57 -> 'A karyotype with trisomy 13 and trisomy
  21.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/addition 1 h/HumanChromosome13)
  (e/addition 1 h/HumanChromosome21))
(defclass k45_XX_-22
  :label "The 45,XX,-22 karyotype"
  :comment "ISCN2009 pg 57 -> 'A karyotype with monosomy 22.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/deletion 1 h/HumanChromosome22))
(defclass k46_XX_+8_-21
  :label "The 46,XX,+8,-21 karyotype"
  :comment "ISCN2009 pg 58 -> 'A karyotype with trisomy 8 and monosomy
  21.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/addition 1 h/HumanChromosome8)
  (e/deletion 1 h/HumanChromosome21))

;; example acquired chromosome abnormalities with a constitutional anomaly
(defclass k48_XY_+21c_+21
  :label "The 48,XY,+21c,+21 karyotype"
  :comment "ISCN2009 pg 58 -> 'An acquired extra chromosome 21 in a
  patient with Down syndrome.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (e/addition 1 h/HumanChromosome21))) ;;aka 47,XY,+21
  (e/addition 1 h/HumanChromosome21))
(defclass k46_XY_+21c_-21
  :label "The 46,XY,+21c,-21 karyotype"
  :comment "ISCN2009 pg 58 -> 'Acquired loss of one chromosome 21 in a
  patient with Down syndrome.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (e/addition 1 h/HumanChromosome21))) ;;aka 47,XY,+21 
  (e/deletion 1 h/HumanChromosome21))


;; Chapter 9 -> Structural Chromosome Rearrangements

;; #TODO
;; 69,XXX,del(7)(p11.2)
;; 69,XXY,del(7)(q22),inv(7)(p13q22),t(7;14)(p15;q11.1)
;; 70,XXX,+del(7)(p11.2)
;; 92,XXYY,del(7)(p11.2),t(7;14)(p15;q11.1)
;; 92,XXYY,del(7)(p11.2),del(7)(q22),del(7)(q34)
;; 46,XX,inv(3)(q21q26.2) karyotype also described later
;; 45,XX,dic(13;15)(q22;q24)

(defclass k46_Y_t!X_8!!p22.3_q24.1!
  :label "The 46,Y,t(X;8)(p22.3;q24.1) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (e/translocation 1 2 h/HumanChromosomeXBandp22.3 h/HumanChromosome8Bandq24.1))

;; 46,XY,der(1)t(1;3)(p22;q13.1)
;; 45,XY,-10,der(10)t(10;17)(q22;p12)              


;; ADDITIONAL MATERIAL OF UNKNOWN ORIGIN
(defclass k46_XX_add!19!!p13.3!
  :label "The 46,XX,add(19)(p13.3) karyotype"
  :comment "ISCN2009 pg 60 -> 'Additional material attached to band
  19p13.3, but neither the origin of the extra segment nor the type of
  arrangement is known.' #CHECK replaces the segment 19pterp13.3"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/addition 1 h/HumanChromosome19Bandp13.3))

(defclass k46_XY_add!12!!q13!
  :label "The 46,XY,add(12)(q13) karyotype"
  :comment "ISCN2009 pg 60 -> 'Additional material of unknown origin
  replaces the segment 12q13qter.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (e/addition 1 h/HumanChromosome12Bandq13))

;; 46,XX,der(5)add(5)(p15.3)add(5)(q23)


;; DELETIONS
(defclass k46_XX_del!5!!q13!
  :label "The 46,XX,del(5)(q13) karyotype"
  :comment "ISCN2009 pg 61 -> 'Terminal deletion with a break (:) in
  band 5q13. The remaining chromosome consistes of the entire short
  arm of chromomsome 5 and the long arm lying between the centromere
  and band 5q13.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/deletion 1 h/HumanChromosome5Bandq13))

(defclass k46_XX_del!5!!q13q33!
  :label "The 46,XX,del(5)(q13q33) karyotype"
  :comment "ISCN2009 pg 61 -> 'Interstitial deletion with breakage and
  reunion (::) of bands 5q13 and 5q33. The segment lying between these
  bands has been deleted.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/deletion 1 h/HumanChromosome5Bandq13 h/HumanChromosome5Bandq33))

(defclass k46_XX_del!5!!q13q13!
  :label "The 46,XX,del(5)(q13q13) karyotype"
  :comment "ISCN2009 pg 61 -> 'Interstitial deletion of a small
  segment within band 5q13, i.e., both breakpoints are in band 5q13.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/deletion 1 h/HumanChromosome5Bandq13 h/HumanChromosome5Bandq13))

(defclass k46_Y_del!X!!p21p21!
  :label "The 46,Y,del(X)(p21p21) karyotype"
  :comment "ISCN2009 pg 61 -> 'Interstitial deletion of a small
  segment within band Xp21'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (e/deletion 1 h/HumanChromosomeXBandp21 h/HumanChromosomeXBandp21))

;; DERIVATIVE CHROMOSOMES

;; example derivative chromosome generated by more than one rearrangement within a chromosome
(defclass k46_XY_der!9!del!9!!p12!del!9!!q31!
  :label "The 46,XY,der(9)del(9)(p12)del(9)(q31) karyotype"
  :comment "ISCN2009 pg 62 -> 'A derivative chromosome 9 rsulting from
  terminal deletions in both the short and long arms with breakpoints
  in bands 9p12 and 9q31.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (f/derivative 1 h/HumanChromosome9 
                (e/deletion 1 h/HumanChromosome9Bandp12) 
                (e/deletion 1 h/HumanChromosome9Bandq31)))
(defclass k46_XY_der!9!inv!9!!p13p23!del!9!!q22q33!
  :label "The 46,XY,der(9)inv(9)(p13p23)del(9)(q22q33) karyotype"
  :comment "ISCN2009 pg 62 -> 'A derivative chromosome 9 rsulting from
  ab inversion in the short arm with breakpoints in 9p13 and 9p23, and
  an interstitial deletion of the long arm with breakpoints in 9q22
  and 9q33.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (f/derivative 1 h/HumanChromosome9 
                (e/inversion 1 h/HumanChromosome9Bandp13 h/HumanChromosome9Bandp23) 
                (e/deletion 1 h/HumanChromosome9Bandq22 h/HumanChromosome9Bandq33)))
(defclass k46_XX_der!7!add!7!!p22!add!7!!q22!
  :label "The 46,XX,der(7)add(7)(p22)add(7)(q22) karyotype"
  :comment "ISCN2009 pg 62 -> 'A derivative chromosome 7 with
  additional material of unknown origin attached at band
  7p22. Similarly, additional material of unknown origin is attached
  to 7q22, replacing the segment 7q22qter.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (f/derivative 1 h/HumanChromosome7 
                (e/addition 1 h/HumanChromosome7Bandp22) 
                (e/addition 1 h/HumanChromosome7Bandq22)))
(defclass k46_XX_der!5!add!5!!p15.1!del!5!!q13!
  :label "The 46,XX,der(5)add(5)(p15.1)del(5)(q13) karyotype"
  :comment "ISCN2009 pg 62 -> 'A derivative chromosome 5 with
  additional material of unknown origin attached at 5p15.1 and a
  terminal deletion of the long arm distal to band 5q13.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (f/derivative 1 h/HumanChromosome5 
                (e/addition 1 h/HumanChromosome5Bandp15.1) 
                (e/addition 1 h/HumanChromosome5Bandq13)))

;; example derivative chromosomes resulting from one rearrangement involving two or more chromosomes
(defclass k46_Y_der!X!t!X_8!!p22.3_q24.1!
  :label "The 46,Y,der(X)t(X;8)(p22.3;q24.1) karyotype"
  :comment "ISCN2009 pg 63 -> 'A male showing a derivative X
  chromosome derived from a tranlocation between Xp22.3 and 8q24.1.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (f/derivative 1 h/HumanChromosomeX
                (e/translocation 1 2 h/HumanChromosomeXBandp22.3 h/HumanChromosome8Bandq24.1)))
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
  (owlsome n/derivedFrom n/k46_XX)
  (f/derivative 1 h/HumanChromosome1
                (e/translocation 1 2 h/HumanChromosome1Bandp22 h/HumanChromosome3Bandq13.1)))
(defclass k45_XY_der!1!t!1_3!!p22_q13.1!_-3
  :label "The 45,XY,der(1)t(1;3)(p22;q13.1),-3 karyotype"
  :comment "ISCN2009 pg 63 -> 'The derivative chromosome 1 (same as
  above) replaces a normal chromosome 1, but there is only one normal
  chromsome 3. One can presume that it is the der(3) resulting from
  the t(1;3) that has been lost, but the karyotype cannot make
  explicit such assumptions.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (f/derivative 1 h/HumanChromosome1 
                (e/translocation 1 2 h/HumanChromosome1Bandp22 h/HumanChromosome3Bandq13.1))
  (e/deletion 1 h/HumanChromosome3))

;; example derivative chromosome generated by more than one rearrangement involving two or more chromosome

(defclass k46_XX_der!1!t!1_3!!p32_q21!t!1_11!!q25_q13!
  :label "The 46,XX,der(1)t(1;3)(p32;q21)t(1;11)(q25;q13) karyotype"
  :comment "ISCN2009 pg 63 -> 'A derivative chromosome 1 generated by
  two translocations, one involving the short arm with a breakpoint in
  1p32 and the other involving the long arm with a breakpoint in
  1q25.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (f/derivative 1 h/HumanChromosome1 
                (e/translocation 1 2 h/HumanChromosome1Bandp32 h/HumanChromosome3Bandq21)
                (e/translocation 1 2 h/HumanChromosome1Bandq25 h/HumanChromosome11Bandq13)))
(defclass k46_XY_der!1!t!1_3!!p32_q21!t!3_7!!q28_q11.2!
  :label "The 46,XY,der(1)t(1;3)(p32;q21)t(3;7)(q28;q11.2) karyotype"
  :comment "ISCN2009 pg 63 -> 'A derivative chromosome 1 resulting
  from a translocation of the chromosome 3 segment distal to 3q21 onto
  1p32, and a translocation of the segment 7q11.2qter to band 3q28 of
  the chromosome 3 segment attached to chromosome 1.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (f/derivative 1 h/HumanChromosome1 
                (e/translocation 1 2 h/HumanChromosome1Bandp32 h/HumanChromosome3Bandq21)
                (e/translocation 1 2 h/HumanChromosome3Bandq28 h/HumanChromosome7Bandq11.2)))
(defclass k46_XY_der!1!t!1_3!!p32_q21!dup!1!!q25q42!
  :label "The 46,XY,der(1)t(1;3)(p32;q21)dup(1)(q25q42) karyotype"
  :comment "ISCN2009 pg 63 -> 'A derivative chromosome 1 resulting
  from a t(1;3) with a breakpoint in 1p32 and a duplication of the
  long arm segment 1q25q42.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (f/derivative 1 h/HumanChromosome1 
                (e/translocation 1 2 h/HumanChromosome1Bandp32 h/HumanChromosome3Bandq21)
                (e/direct_duplication 1 h/HumanChromosome1Bandq25 h/HumanChromosome1Bandq42))) ;; #CHECK

;; 46,XY,der(9)del(9)(p12)t(9;13)(q34;q11)
;; 46,XX,der(1)t(1;11)(p32;q13)t(1;3)(q25;q21)


;; DICENTRIC CHROMOSOMES


;; DUPLICATIONS
;; NOTE: Only the detailed system will clarify the location of the duplicated segment
(defclass k46_XX_dup!1!!q22q25!
  :label "The 46,XX,dup(1)(q22q25) karyotype"
  :comment "ISCN2009 pg 69 -> 'Direct duplication of the segment
  between bands 1q22 and 1q25.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/direct_duplication 1 h/HumanChromosome1Bandq22 h/HumanChromosome1Bandq25))

;; NOTE: Only the detailed system will clarify the location of the duplicated segment
;; AKA pterq25q25q22q25qter or pterq22q25q22q22qter
(defclass k46_XY_dup!1!!q25q22!
  :label "The 46,XY,dup(1)(q25q22) karyotype"
  :comment "ISCN2009 pg 69 -> 'Inverse duplication of the segment
  between bands 1q22 and 1q25. Note that only the detailed system will
  clarify the location of the duplication segment.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (e/inverse_duplication 1 h/HumanChromosome1Bandq25 h/HumanChromosome1Bandq22))


;; FISSION
(defclass k47_XY_-10_+fis!10!!p10!_+fis!10!!q10!
  :label "The 47,XY,-10,+fis(10)(p10),+fis(10)(q10) karyotype"
  :comment "ISCN2009 pg 69 -> 'Break in the centromere resulting in
  two derivative chromosomes composed of the short and long arms
  respectively. The breakpoints (:) are assigned to p10 and q10
  according to the morpohology of the derivative chromosomes.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (flatten (e/fission 1 h/HumanChromosome10))) ;; TODO: include 'addition' event


;; FRAGILE SITES
;; HOMOGENEOUSLY STAINING REGIONS


;; INSERTIONS

;; insertion within a chromosome
(defclass k46_XX_ins!2!!p13q21q31!
  :label "The 46,XX,ins(2)(p13q21q31) karyotype"
  :comment "ISCN2009 pg 71 -> 'Direct insertion, i.e., dir
  ins(2)(p13q21q31). The long-arm segment between bands 2q21 and 2q31
  has been inserted into the short arm at band 2p13. The original
  orientation of the inserted segment has been maintained in its new
  position, i.e., band 2q21 remains more proximal to the centromere
  than band 2q31.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/direct_insertion 1 h/HumanChromosome2Bandp13 h/HumanChromosome2Bandq21 h/HumanChromosome2Bandq31))

(defclass k46_XY_ins!2!!p13q31q21!
  :label "The 46,XY,ins(2)(p13q31q21) karyotype"
  :comment "ISCN2009 pg 71 -> 'Inverted insertion, i.e., inv
  ins(2)(p13q21q31). The insertion is the same as in the previous
  example except that the inserted has been inverted, i.e. band 2q21
  of the inserted segment is now more distal to the centromere than
  band 2q31. The orientation of the bands within the segment has thus
  been reversed with respect to the centromere.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (e/inverse_insertion 1 h/HumanChromosome2Bandp13 h/HumanChromosome2Bandq31 h/HumanChromosome2Bandq21))

;; insertion between two choromosomes
(defclass k46_XY_ins!5_2!!p14_q22q32!
  :label "The 46,XY,ins(5;2)(p14;q22q32) karyotype"
  :comment "ISCN2009 pg 71 -> 'Direct insertion, i.e., dir
  ins(5;2)(p14;q22q32). The long-arm segment between bands 2q22 and
  2q32 has been inserted into the short arm of chromosome 5 at at band
  5p14. The original orientation of the inserted segment has been
  maintained in its new position, i.e., band 2q22 remains more
  proximal to the centromere than band 2q32. Note that the recipient
  chromosome is specified first.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (e/direct_insertion 1 h/HumanChromosome5Bandp14 h/HumanChromosome2Bandq22 h/HumanChromosome2Bandq32))

(defclass k46_XY_ins!5_2!!p14_q32q22!
  :label "The 46,XY,ins(5;2)(p14;q32q22) karyotype"
  :comment "ISCN2009 pg 71 -> 'Inverted insertion, i.e., inv
  ins(5;2)(p14;q32q22). Breakage and reunion have occurred at the same
  bands as in the previous example, and the insertion is the same
  except that the inserted sergment has been inverted, i.e. band 2q22
  is now more distal to the centromere of the recipient chromosome
  than band 2q32.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (e/inverse_insertion 1 h/HumanChromosome5Bandp14 h/HumanChromosome2Bandq32 h/HumanChromosome2Bandq22))

(defclass k46_XX_ins!5_2!!q31_p13p23!
  :label "The 46,XX,ins(5;2)(q31;p13p23) karyotype"
  :comment "ISCN2009 pg 71 -> 'A direct insertion of bands p13 to p23
  from chromosome 2 into band 5q31.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/direct_insertion 1 h/HumanChromosome5Bandq31 h/HumanChromosome2Bandp13 h/HumanChromosome2Bandp23))

(defclass k46_XX_ins!5_2!!q31_p23p13!
  :label "The 46,XX,ins(5;2)(q31;p23p13) karyotype"
  :comment "ISCN2009 pg 71 -> 'A insertion of bands p13 to p23 from
  chromosome 2 into band 5q31 in an inverted orientation.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/inverse_insertion 1 h/HumanChromosome5Bandq31 h/HumanChromosome2Bandp23 h/HumanChromosome2Bandp13))


;; INVERSION
(defclass k46_XX_inv!3!!q21q26.2!
  :label "The 46,XX,inv(3)(q21q26.2) karyotype"
  :comment "ISCN2009 pg 72 -> 'Paracentric inversion in which breakage
  and union have occurred at bands 3q21 and 3q26.2.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/inversion 1 h/HumanChromosome3Bandq21 h/HumanChromosome3Bandq26.2))


(defclass k46_XY_inv!3!!p13q21!
  :label "The 46,XY,inv(3)(p13q21) karyotype"
  :comment "ISCN2009 pg 72 -> 'Pericentric inversion in which breakage
  and reunion have occurred at bands 3p13 and 3q21.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (e/inversion 1 h/HumanChromosome3Bandp13 h/HumanChromosome3Bandq21))


;; ISOCHROMOSOMES
;; MARKER CHROMOSOMES
;; NEOCENTROMERES


;; QUADRUPLICATIONS
;; NOTE: It is not possible to indicate the orientations of the segments with the short system!
(defclass k46_XX_qdp!1!!q23q32!
  :label "The 46,XX,qdp(1)(q23q32) karyotype"
  :comment "ISCN2009 pg 75 -> 'Quadruplication of the segment between
  bands 1q23 and 1q32.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/quadruplication 1 h/HumanChromosome1Bandq23.1 h/HumanChromosome1Bandq32.3))


;; RING CHROMOSOMES
;; TELOMERIC ASSIOCIATIONS


;; TRANSLOCATIONS
;; reciprocal two-break rearrangement translocations
(defclass k46_XY_t!2_5!!q21_q31!
  :label "The 46,XY,t(2;5)(q21;q31) karyotype"
  :comment "ISCN2009 pg 77 -> 'Breakage and reunion have occurred at
  bands 2q21 and 5q31. The segments distal to these bands have been
  exchanged.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (e/translocation 1 2 h/HumanChromosome2Bandq21 h/HumanChromosome5Bandq31))

(defclass k46_XY_t!2_5!!p12_q31!
  :label "The 46,XY,t(2;5)(p12;q31) karyotype"
  :comment "ISCN2009 pg 78 -> 'Breakage and reunion have occurred at
  bands 2p12 and 5q31. The segments distal to these bands have been
  exchanged.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (e/translocation 1 2 h/HumanChromosome2Bandp12 h/HumanChromosome5Bandq31))

(defclass k46_X_t!X_13!!q27_q12!
  :label "The 46,X,t(X;13)(q27;q12) karyotype"
  :comment "ISCN2009 pg 78 -> 'Breakage and reunion have occurred at
  bands Xq27 and 13q12. The segments distal to these bands have been
  exchanged. Since one of the chromosomes involved in the
  translocation is a sex chromosome, it is designated first. Note that
  the correct designation is 46,X,t(X;13) and not
  46,XX,t(X;13). Similarly, an identical translocation in a male
  should be designated 46,Y,t(X;13) and not 46,XY,t(X;13).'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/translocation 1 2 h/HumanChromosomeXBandq27 h/HumanChromosome13Bandq12))

;; ;; ERROR in book! - there is no resolution band for Y q11.2
;; (defclass k46_t!X_Y!!q22_q11.2!
;;   :label "The 46,t(X;Y)(q22;q11.2) karyotype"
;;   :comment "ISCN2009 pg 78 -> 'A reciprocal translocation between an X
;;   chromosome and a Y chromosome with breakpoints at bands Xq22 and
;;   Yq11.2.'"
;;   :subclass ISCNExampleKaryotype
;;   (owlsome n/derivedFrom n/k46_XY)
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
;;   (owlsome n/derivedFrom n/k46_XY)
;;   (e/translocation 1 h/HumanChromosomeXBand11.2 h/HumanChromosome18Bandq11.2)
;;   (e/translocation 1 h/HumanChromosomeYBand11.2 h/HumanChromosome1Bandp31))

;; reciprocal three-break rearrangement tranlocations
(defclass k46_XX_t!2_7_5!!p21_q22_q23!
  :label "The 46,XX,t(2;7;5)(p21;q22;q23) karyotype"
  :comment "ISCN2009 pg 78 -> 'The segment on chromosome 2 distal to
  2p21 has been translocated onto chromosome 7 at band 7q22, the
  segment on chromosome 7 distal to 7q22 has been translocated onto
  chromosome 5 at 5q23, and the segment of chromosome 5 distal to 5q23
  has been translocated onto chromosome 2 at 2p21.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/translocation 1 3 h/HumanChromosome2Bandp21 h/HumanChromosome7Bandq22 h/HumanChromosome5Bandq23))

(defclass k46_X_t!X_22_1!!q24_q11.2_p33!
  :label "The 46,X,t(X;22;1)(q24;q11.2;p33) karyotype"
  :comment "ISCN2009 pg 78 -> 'The segment on one chromosome X distal
  to Xq24 has been translocated onto chromosome 22 at band 22q11.2,
  the segment distal to 22q11.2 has been translocated onto chromosome
  1 at 1p33, and the segment distal to 1p33 has been translocated onto
  the chromosome X at Xq24.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/translocation 1 3 h/HumanChromosomeXBandq24 h/HumanChromosome22Bandq11.2 h/HumanChromosome1Bandp33))

;; TODO How do I model that 2 distinct Chromosomes 7 are involved?
;; (defclass k46_XX_t!3_7_7*!!q21_q22_p13!
;;   :label "The 46,XX,t(3;7;7*)(q21;q22;p13) karyotype"
;;   :comment "ISCN2009 pg 78 -> 'The segment on chromosome 2 distal to
;;   2q21 has been translocated onto chromosome 7 at band 7q22, the
;;   segment on chromosome 7 distal to 7q22 has been translocated onto
;;   the homologous chromosome 7 at band 7p13, and the segment distal to
;;   7p13 on the latter chromosome has been translocated onto chromosome
;;   2 at 2q21. Underlining is used only to emphasize that the
;;   chromosomes are homologous. However, this is usually not necessary
;;   since if the same chromosome 7 had been involved, the resulting
;;   chromosome 7 would have to be described as a derivative
;;   chromosome.'"
;;   :subclass ISCNExampleKaryotype
;;   (owlsome n/derivedFrom n/k46_XX)
;;   ;; (e/translocation 1 h/HumanChromosome3Bandq21 h/HumanChromosome7Bandq22 h/HumanChromosome7*Bandp13)
;;   )

;; reciprocal four-break and more complex rearrangemnt translocations
(defclass k46_XX_t!3_9_22_21!!p13_q34_q11.2_q21!
  :label "The 46,XX,t(3;9;22;21)(p13;q34;q11.2;q21) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/translocation 1 4 h/HumanChromosome3Bandp13 h/HumanChromosome9Bandq34 h/HumanChromosome22Bandq11.2 h/HumanChromosome21Bandq21))

;; TODO How do I model that 2 distinct Chromosomes 7 are involved?
;; (defclass k46_XX_t!3_9_9*_22!!p13_q22_q34_q11.2!
;;   :label "The 46,XX,t(3;9;9*;22)(p13;q22;q34;q11.2) karyotype"
;;   :subclass ISCNExampleKaryotype
;;   (owlsome n/derivedFrom n/k46_XX)
;;   ;; (e/translocation 1 h/HumanChromosome3Bandp13 h/HumanChromosome9Bandq22 h/HumanChromosome9*Bandq34 h/HumanChromosome22Bandq11.2)
;;   )

(defclass k46_XY_t!5_6!!q13q23_q15q23!
  :label "The 46,XY,t(5;6)(q13q23;q15q23) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (e/translocation 1 2 h/HumanChromosome5Bandq13 h/HumanChromosome5Bandq23 h/HumanChromosome6Bandq15 h/HumanChromosome6Bandq23))

(defclass k46_XX_t!5_14_9!!q13q23_q24q21_p12p23!
  :label "The 46,XX,t(5;14;9)(q13q23;q24q21;p12p23) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/translocation 1 3 h/HumanChromosome5Bandq13 h/HumanChromosome5Bandq23 h/HumanChromosome14Bandq24 h/HumanChromosome14Bandq21 h/HumanChromosome9Bandp12 h/HumanChromosome9Bandp23))

;; balanced whole-arm translocations
;; (defclass k46_XY_t!1_3!!p10_q10!
;;   :label "The 46,XY,t(1;3)(p10;q10) karyotype"
;;   :subclass ISCNExampleKaryotype
;;   (owlsome n/derivedFrom n/k46_XY)
;;   ;; (e/translocation 1 2 h/HumanChromosome1Banp10 h/HumanChromosome3Bandq10)
;;   )

;; (defclass k46_XY_t!1_3!!p10_p10!
;;   :label "The 46,XY,t(1;3)(p10;q10) karyotype"
;;   :subclass ISCNExampleKaryotype
;;   (owlsome n/derivedFrom n/k46_XY)
;;   ;; (e/translocation 1 2 h/HumanChromosome1Banp10 h/HumanChromosome3Bandp10)
;;   )

;; unbalanced whole-arm translocations
;; 45,XX,der(1;3)(p10;q10)
;; 46,XX,+1,der(1;3)(p10;q10)
;; 46,XX,der(1;3)(p10;q10),+3
;; 47,XX,+der(1;3)(p10;q10)
;; 44,XY,-1,der(1;3)(p10;q10)

;; robertsonian translocations
;; jumping translocations


;; TRICENTRIC CHROMOSOMES


;; TRIPLICATIONS
;; NOTE: It is not possible to indicate the orientations of the segments with the short system!
(defclass k46_XX_trp!1!!q21q32!
  :label "The 46,XX,trp(1)(q21q32) karyotype"
  :comment "ISCN2009 pg 83 -> 'Direct triplication of the segment
  between bands 1q21 and 1q32, one of several possible orientations of
  the triplications of this segment.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/DirectTriplication (owlsome e/hasBreakPoint h/HumanChromosome1Bandq23.1 h/HumanChromosome1Bandq32.3))))

(defclass k46_XX_invtrp!1!!q32q21!
  :label "The 46,XX,inv trp(1)(q32q21) karyotype"
  :comment "ISCN2009 pg 83 -> 'Inverted triplication of the segment between bands 1q21 and 1q32.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/InverseTriplication (owlsome e/hasBreakPoint h/HumanChromosome1Bandq23.1 h/HumanChromosome1Bandq32.3))))


;; MULTIPLE COPIES OF REARRANGED CHROMOSOMES


;; OTHER EXAMPLES

;; example numerical constitutional allosomal abnormal karyotypes
(defclass k45_Y
  :label "The 45,Y karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (e/deletion 1 h/HumanChromosomeX))))
(defclass k46_YY
  :label "The 46,YY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (e/deletion 1 h/HumanChromosomeX) (e/addition 1 h/HumanChromosomeY))))
(defclass k47_YYY
  :label "The 47,YYY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (e/deletion 1 h/HumanChromosomeX) (e/addition 2 h/HumanChromosomeY))))
(defclass k48_XXXX
  :label "The 48,XXXX karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XX) (e/addition 2 h/HumanChromosomeX))))
(defclass k48_XXYY
  :label "The 48,XXYY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (e/addition 1 h/HumanChromosomeX) (e/addition 1 h/HumanChromosomeY))))
(defclass k48_XYYY
  :label "The 48,XYYY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (e/addition 2 h/HumanChromosomeY))))
(defclass k48_YYYY
  :label "The 48,YYYY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (e/deletion 1 h/HumanChromosomeX) (e/addition 3 h/HumanChromosomeY))))
(defclass k49_XXXXX
  :label "The 49,XXXXX karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XX) (e/addition 3 h/HumanChromosomeX))))
(defclass k49_XXXXY
  :label "The 49,XXXXY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (e/addition 3 h/HumanChromosomeX))))
(defclass k49_XXXYY
  :label "The 49,XXXYY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (e/addition 2 h/HumanChromosomeX) (e/addition 1 h/HumanChromosomeY))))
(defclass k49_XXYYY
  :label "The 49,XXYYY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (e/addition 1 h/HumanChromosomeX) (e/addition 2 h/HumanChromosomeY))))
(defclass k49_XYYYY
  :label "The 49,XYYYY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (e/addition 3 h/HumanChromosomeY))))
(defclass k49_YYYYY
  :label "The 49,YYYYY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (e/deletion 1 h/HumanChromosomeX) (e/addition 4 h/HumanChromosomeY))))

;; more examples from the ISCN2009
(defclass k46_XX_inv!2!!p21q31!
  :label "The 46,XX,inv(2)(p21q31) karyotype"
  :comment "ISCN2009 pg 42"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/inversion 1 h/HumanChromosome2Bandp21 h/HumanChromosome2Bandq31))

(defclass k46_XX_inv!2!!p13p23!
  :label "The 46,XX,inv(2)(p13p23) karyotype"
  :comment "ISCN2009 pg 42"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/inversion 1 h/HumanChromosome2Bandp13 h/HumanChromosome2Bandp23))

(defclass k46_XY_t!12_16!!q13_p11.1!
  :label "The 46,XY,t(12;16)(q13;p11.1) karyotype"
  :comment "ISCN2009 pg 43"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (e/translocation 1 2 h/HumanChromosome12Bandq13 h/HumanChromosome16Bandp11.1))

(defclass k46_X_t!X_18!!p11.1_q11.1!
  :label "The 46,X,t(X;18)(p11.1;q11.1) karyotype"
  :comment "ISCN2009 pg 43"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XN)
  (e/translocation 1 2 h/HumanChromosomeXBandp11.1 h/HumanChromosome18Bandq11.1))

(defclass k46_X_ins!5_X!!p14_q21q25!
  :label "The 46,X,ins(5;X)(p14;q21q25) karyotype"
  :comment "ISCN2009 pg 43"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/direct_insertion 1 h/HumanChromosome5Bandp14 h/HumanChromosomeXBandq21 h/HumanChromosomeXBandq25))

(defclass k46_XX_ins!2!!q13p13p23!
  :label "The 46,XX,ins(2)(q13p13p23) karyotype"
  :comment "ISCN2009 pg 43 -> 'Direct insertion of the short-arm
  segment between bands 2p13 and 2p23 into the long arm at band
  2q13.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/direct_insertion 1 h/HumanChromosome2Bandq13 h/HumanChromosome2Bandp13 h/HumanChromosome2Bandp23))

(defclass k46_XX_ins!2!!q13p23p13!
  :label "The 46,XX,ins(2)(q13p23p13) karyotype"
  :comment "ISCN2009 pg 43 -> 'Inverted insertion of the short-arm
  segment between bands 2p13 and 2p23 into the long arm at band
  2q13. Because the insertion is inverted, band 2p23 is now proximal
  and band 2p13 distal to the centromere.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/inverse_insertion 1 h/HumanChromosome2Bandq13 h/HumanChromosome2Bandp23 h/HumanChromosome2Bandp13))

(defclass k46_XX_t!9_22_17!!q34_q11.2_q22!
  :label "The 46,XX,t(9;22;17)(q34;q11.2;q22) karyotype"
  :comment "ISCN2009 pg 43 -> 'The segment of chromosome 9 distal to
  9q34 has been translocated onto chromosome 22 at band 22q11.2, the
  segment of chromosome 22 distal to 22q11.2 has been translocated
  onto chromosome 17 at 17q22, and the segment of chromosome 17 distal
  has been translocated onto chromosome 9 at 9q34.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/translocation 1 3 h/HumanChromosome9Bandq34 h/HumanChromosome22Bandq11.2 h/HumanChromosome17Bandq22))

(defclass k46_XY_t!X_15_18!!p11.1_p11.1_q11.1!
  :label "The 46,XY,t(X;15;18)(p11.1;p11.1;q11.1) karyotype"
  :comment "ISCN2009 pg 43 -> 'The segment of the X chromosome distal
  to Xp11.1 hass been translocated onto chromosome15 at band 15p11.1,
  the segment of chromosome 15 distal to 15p11.1 has been translocated
  onto chromosome 18 at 18q11.1, and the segment of chromosome 18
  distal to 18q11.2 has been translocated to Xp11.1.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (e/translocation 1 3 h/HumanChromosomeXBandp11.1 h/HumanChromosome15Bandp11.1 h/HumanChromosome18Bandq11.1))

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
  (owlsome n/derivedFrom n/k46_XX)
  (e/translocation 1 4 h/HumanChromosome3Bandp13 h/HumanChromosome9Bandq34 h/HumanChromosome22Bandq11.2 h/HumanChromosome21Bandq21))

(defclass k46_XY_t!5_6!!q13q23_q15q23!
  :label "The 46,XY,t(5;6)(q13q23;q15q23) karyotype"
  :comment "ISCN2009 pg 44 -> 'Reciprocal translocation of two
  interstitial segments. The segments between bands 5q13 and 5q23 of
  chromosome 5 and between 6q15 and 6q23 of choromosome 6 have been
  exchanged.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (e/translocation 1 2 h/HumanChromosome5Bandq13 h/HumanChromosome5Bandq23 h/HumanChromosome6Bandq15 h/HumanChromosome6Bandq23))

(defclass k47_X_t!X_13!!q27_q12!_inv!10!!p13q22!_+21
  :label "The 47,X,t(X;13)(q27;q12),inv(10)(p13q22),+21 karyotype"
  :comment "ISCN2009 pg 52 -> 'The sex chromosome abnormality is
  presented first, followed by the autosomal abnormalities in
  chromosome number order, irrespective of whether the aberrations are
  numerical or structural.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/translocation 1 2 h/HumanChromosomeXBandq27 h/HumanChromosome13Bandq12)
  (e/inversion 1 h/HumanChromosome10Bandp13 h/HumanChromosome10Bandq22)
  (e/addition 1 h/HumanChromosome21))

;; 47,Y,t(X;13)(q27;q12),inv(10)(p13q22),+21

;; ;; ERROR in book! - there is no resolution band for Y q11.2
;; (defclass k46_t!X_18!!p11.1_q11.2!_t!Y_1!!q11.2_p13!
;;   :label "The 46,t(X;18)(p11.1;q11.2),t(Y;1)(q11.2;p13) karyotype"
;;   :comment "ISCN2009 pg 52 -> 'The abnormality involving the X
;;   chromosome is listed before that of the Y chromosome.'"
;;   :subclass ISCNExampleKaryotype
;;   (owlsome n/derivedFrom n/k46_XY)
;;   (e/translocation 1 2 h/HumanChromosomeXBandp11.1 h/HumanChromosome18Bandq11.2)
;;   (e/translocation 1 2 h/HumanChromosomeYBandq11.2 h/HumanChromosome1Bandp13)
;;   )

;; ;; ERROR in book! - there is no resolution band for Y q11.2
;; (defclass k48_X_t!Y_12!!q11.2_p12!_del!6!!q11!_+8_t!9_22!!q34_q11.2!_+17_-21_+22
;;   :label "The 48,X,t(Y;12)(q11.2;p12),del(6)(q11),+8,t(9;22)(q34;q11.2),+17,-21,+22 karyotype"
;;   :comment "ISCN2009 pg 52 -> 'The translocation involving the Y
;;   chromosome is presented first, followed by all autosomal
;;   abnormalities in strict chromosome number order.'"
;;   :subclass ISCNExampleKaryotype
;;   (owlsome n/derivedFrom n/k46_XY)
;;   (e/translocation 1 2 h/HumanChromosomeYBandq11.2 h/HumanChromosome12Bandp12)
;;   (e/deletion 1 h/HumanChromosome6Bandq11)
;;   (e/addition 1 h/HumanChromosome8)
;;   (e/translocation 1 2 h/HumanChromosome9Bandq34 h/HumanChromosome22Bandq11.2)
;;   (e/addition 1 h/HumanChromosome17)
;;   (e/deletion 1 h/HumanChromosome21)
;;   (e/addition 1 h/HumanChromosome22)
;; )

(defclass k49_X_inv!X!!p21q26!_+3_inv!3!!q21q26.2!_+7_+10_-20_del!20!!q11.2!_+21
  :label "The 49,X,inv(X)(p21q26),+3,inv(3)(q21q26.2),+7,+10,-20,del(20)(q11.2),+21  karyotype"
  :comment "ISCN2009 pg 52 -> 'The inversion of the X chromosome is
  listed first, The extra chromosome 3 is presented before the
  inversion of chromosome 3 and the monosomy 20 before the deletion of
  chromosome 20.'"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (e/inversion 1 h/HumanChromosomeXBandp21 h/HumanChromosomeXBandq26)
  (e/addition 1 h/HumanChromosome3)
  (e/inversion 1 h/HumanChromosome3Bandq21 h/HumanChromosome3Bandq26.2)
  (e/addition 1 h/HumanChromosome7)
  (e/addition 1 h/HumanChromosome10)
  (e/deletion 1 h/HumanChromosome20)
  (e/deletion 1 h/HumanChromosome20Bandq11.2)
  (e/addition 1 h/HumanChromosome21))

;; (defclass k50_XX_+1_del!1!!p13!_+dup!1!!q21q32!_+inv!1!!p31q41!_+8_r!10!!p12q25!_-21
;;   :label "The 50,XX,+1,del(1)(p13),+dup(1)(q21q32),+inv(1)(p31q41),+8,r(10)(p12q25),-21 karyotype"
;;   :comment "ISCN2009 pg 52 -> 'There are four abnormalities
;;   involving diffrent copies of chromosome 1. The numerical change
;;   is presented first, followed by the structural aberrations listed
;;   in alphabetical order: del, dup, inv.'"
;;   :subclass ISCNExampleKaryotype
;;   (owlsome n/derivedFrom n/k46_XX)
;;   (e/addition 1 h/HumanChromosome1)
;;   (e/deletion 1 h/HumanChromosome1Bandp13)
;;   ;; (e/addition 1 (e/duplication 1 h/HumanChromosome1Bandq21 h/HumanChromosome1Bandq32))
;;   ;; (e/addition 1 (e/inverion 1 h/HumanChromosome1Bandp31 h/HumanChromosome1Bandq41))
;;   (e/addition 1 h/HumanChromosome8)
;;   ;; TODO (exactly 1 f/hasFeature (owland f/RingChromosome h/HumanChromosome10 (e/hasBreakPoint h/HumanChromosome10p12 h/HumanChromosome10q25))) 
;;   (e/deletion 1 h/HumanChromosome21))
;; )

) ;; ends as-disjoint

;; ;; MACROS

;; ;; PARSE KARYOTYPE STRING
;; input: string
;; output: karyotype
;; (defmacro parseKaryotypeString[s]
;;   (defclass <TODO>
;;     :label (str "The" s "karyotype")
;;     :subclass ISCNExampleKaryotype
;;     (owlsome n/derivedFrom <TODO>)
;;     <TODO>))

;; ;; PARSE KARYOTYPE
;; input: karyotype
;; output: string
;; (defmacro parseKaryotype[k] ())
