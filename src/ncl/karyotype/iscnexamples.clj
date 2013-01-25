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

;; Define the namedKaryotypes

;; TODO Need to include disjoint axioms

;; example numerical allosomal abnormal karyotypes
(defclass k45_X
  :label "The 45,X karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XN)
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanAllosome)))
(defclass k45_Y
  :label "The 45,Y karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosomeX)))
(defclass k46_YY
  :label "The 46,YY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosomeX))
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosomeY)))
(defclass k47_XXX
  :label "The 47,XXX karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosomeX)))
(defclass k47_XXY
  :label "The 47,XXY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosomeX)))
(defclass k47_XYY
  :label "The 47,XYY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosomeY)))
(defclass k47_YYY
  :label "The 47,YYY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 2 e/hasEvent (owland e/Addition h/HumanChromosomeY))
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosomeX)))
(defclass k48_XXXX
  :label "The 48,XXXX karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 2 e/hasEvent (owland e/Addition h/HumanChromosomeX)))
(defclass k48_XXXY
  :label "The 48,XXXY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 2 e/hasEvent (owland e/Addition h/HumanChromosomeX)))
(defclass k48_XXYY
  :label "The 48,XXYY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosomeX))
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosomeY)))
(defclass k48_XYYY
  :label "The 48,XYYY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 2 e/hasEvent (owland e/Addition h/HumanChromosomeY)))
(defclass k48_YYYY
  :label "The 48,YYYY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 3 e/hasEvent (owland e/Addition h/HumanChromosomeY))
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosomeX)))
(defclass k49_XXXXX
  :label "The 49,XXXXX karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 3 e/hasEvent (owland e/Addition h/HumanChromosomeX)))
(defclass k49_XXXXY
  :label "The 49,XXXXY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 3 e/hasEvent (owland e/Addition h/HumanChromosomeX)))
(defclass k49_XXXYY
  :label "The 49,XXXYY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosomeY))
  (exactly 2 e/hasEvent (owland e/Addition h/HumanChromosomeX)))
(defclass k49_XXYYY
  :label "The 49,XXYYY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 2 e/hasEvent (owland e/Addition h/HumanChromosomeY))
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosomeX)))
(defclass k49_XYYYY
  :label "The 49,XYYYY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 3 e/hasEvent (owland e/Addition h/HumanChromosomeY)))
(defclass k49_YYYYY
  :label "The 49,YYYYY karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 4 e/hasEvent (owland e/Addition h/HumanChromosomeY))
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosomeX)))
(defclass k47_XX_+X
  :label "The 47,XX,+X karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosomeX)))
(defclass k45_X_-X
  :label "The 45,X,-X karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosomeX)))
(defclass k45_X_-Y
  :label "The 45,X,-Y karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosomeY)))
(defclass k45_Y_-X
  :label "The 45,Y,-X karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosomeX)))
(defclass k48_XY_+X_+Y
  :label "The 48,XY,+X,+Y karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosomeX))
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosomeY)))


;; example ploidy karyotypes
;; example haploid karyotype
(defclass k26_X_+4_+6_+21
  :label "The 26,X,+4,+6,+21 karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k23_X)
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome4))
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome6))
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome21)))
;; example triploid karyotype
;; TODO n/derivedFrom
(defclass k71_XXX_+8_+10
  :label "The 71,XXX,+8,+10 karyotype"
  :subclass ISCNExampleKaryotype
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome8))
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome10)))
;; example tetraploid karyotype
;; TODO n/derivedFrom
(defclass k89_XXYY_-1_-3_-5_+8_-21
  :label "The 89,XXYY,-1,-3,-5,+8,-21 karyotype"
  :subclass ISCNExampleKaryotype
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome8))
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosome1))
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosome3))
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosome5))
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosome21)))



;; example numerical autosomal abnormal karyotypes
(defclass k45_XX_-22
  :label "The 45,XX,-22 karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosome22)))
(defclass k46_XX_+8_-21
  :label "The 46,XX,+8,-21 karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome8))
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosome21)))
(defclass k47_XX_+21
  :label "The 47,XX,+21 karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome21)))
(defclass k48_XX_+13_+21
  :label "The 48,XX,+13,+21 karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome13))
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome21)))



;; example conditional karyotypes
(defclass k48_XXYc_+X
  :label "The 48,XXYc,+X karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosomeX)))) ;;aka 47,XXY
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosomeX)))
(defclass k46_Xc_+X
  :label "The 46,Xc,+X karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XN) (exactly 1 e/hasEvent (owland e/Deletion h/HumanAutosome)))) ;;aka 45,X
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosomeX)))
(defclass k46_XXYc_-X
  :label "The 46,XXYc,-X karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosomeX)))) ;;aka 47,XXY    
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosomeX)))
(defclass k44_Xc_-X
  :label "The 44,Xc,-X karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XN) (exactly 1 e/hasEvent (owland e/Deletion h/HumanAllosome)))) ;;aka 45,X 
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosomeX)))
(defclass k46_Xc_+21
  :label "The 46,Xc,+21 karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XN) (exactly 1 e/hasEvent (owland e/Deletion h/HumanAllosome)))) ;;aka 45,X
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome21)))
(defclass k48_XY_+21c_+21
  :label "The 48,XY,+21c,+21 karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome21)))) ;;aka 47,XY,+21
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome21)))
(defclass k46_XY_+21c_-21
  :label "The 46,XY,+21c,-21 karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom (owland (owlsome n/derivedFrom n/k46_XY) (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome21)))) ;;aka 47,XY,+21 
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosome21)))



;; example structural abnormal karyotypes

;; ADDITION
(defclass k46_XX_add!19!!p13.3!
  :label "The 46,XX,add(19)(p13.3) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Addition (owlsome e/hasBreakPoint h/HumanChromosome19Bandp13.3))))

(defclass k46_XY_add!12!!q13!
  :label "The 46,XY,add(12)(q13) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent (owland e/Addition (owlsome e/hasBreakPoint h/HumanChromosome12Bandq13.3)))) ;;TOFIX q13 == 13.3

;; DELETION
(defclass k46_XX_del!5!!q13!
  :label "The 46,XX,del(5)(q13) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Deletion (owlsome e/hasBreakPoint h/HumanChromosome5Bandq13.3 h/HumanChromosome5BandqTer)))) ;;TOFIX q13 == q13.3

(defclass k46_XX_del!5!!q13q33!
  :label "The 46,XX,del(5)(q13q33) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Deletion (owlsome e/hasBreakPoint h/HumanChromosome5Bandq13.3 h/HumanChromosome5Bandq33.3)))) ;;TOFIX q13 == q13.3 && q33 == q33.3

(defclass k46_XX_del!5!!q13q13!
  :label "The 46,XX,del(5)(q13q13) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Deletion (owlsome e/hasBreakPoint h/HumanChromosome5Bandq13.3)))) ;;TOFIX q13 == q13.3

(defclass k46_Y_del!X!!p21p21!
  :label "The 46,Y,del(X)(p21p21) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent (owland e/Deletion (owlsome e/hasBreakPoint h/HumanChromosomeXBandp21.3)))) ;;TOFIX p21 == p21.3

;; DUPLICATIONS
(defclass k46_XX_dup!1!!q22q25!
  :label "The 46,XX,dup(1)(q22q25) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/DirectDuplication (owlsome e/hasBreakPoint h/HumanChromosome1Bandq22 h/HumanChromosome1Bandq25.3)))) ;;TOFIX q25 == q25.3

;; TOFIX: Only the detailed system will clarify the location of the duplicated segment
;; AKA pterq25q25q22q25qter or pterq22q25q22q22qter
(defclass k46_XY_dup!1!!q25q22!
  :label "The 46,XY,dup(1)(q25q22) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent (owland e/InverseDuplication (owlsome e/hasBreakPoint h/HumanChromosome1Bandq25.3 h/HumanChromosome1Bandq22)))) ;;TOFIX q25 == q25.3

;; FISSION
;; QUERY: p10 && q10 == Cen?
(defclass k47_XY_-10_+fis!10!!p10!_+fis!10!!q10!
  :label "The 47,XY,-10,+fis(10)(p10),+fis(10)(q10) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosome10))
  (exactly 1 e/hasEvent (owland e/Fission (owlsome e/hasBreakPoint h/HumanChromosome10BandpTer h/HumanChromosome10BandCen)))
  (exactly 1 e/hasEvent (owland e/Fission (owlsome e/hasBreakPoint h/HumanChromosome10BandqTer h/HumanChromosome10BandCen))))

;; INVERSION
(defclass k46_XX_inv!2!!p21q31!
  :label "The 46,XX,inv(2)(p21q31) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Inversion (owlsome e/hasBreakPoint h/HumanChromosome2Bandp21 h/HumanChromosome2Bandq31))))

(defclass k46_XX_inv!2!!p13p23!
  :label "The 46,XX,inv(2)(p13p23) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Inversion (owlsome e/hasBreakPoint h/HumanChromosome2Bandp13 h/HumanChromosome2Bandp23))))

(defclass k46_XX_inv!3!!q21q26.2!
  :label "The 46,XX,inv(3)(q21q26.2) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Inversion (owlsome e/hasBreakPoint h/HumanChromosome3Bandq21 h/HumanChromosome3Bandq26.2))))

(defclass k46_XY_inv!3!!p13q21!
  :label "The 46,XY,inv(3)(p13q21) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent (owland e/Inversion (owlsome e/hasBreakPoint h/HumanChromosome3Bandp13 h/HumanChromosome3Bandq21))))

;; INSERTIONS
(defclass k46_X_ins!5_X!!p14_q21q25!
  :label "The 46,X,ins(5;X)(p14;q21q25) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/DirectInsertion (owlsome e/hasReceivingBreakPoint h/HumanChromosome5Bandp14) (owlsome e/hasProvidingBreakPoint h/HumanChromosomeXBandq21.3 h/HumanChromosomeXBandq25)))) ;; TOFIX q21 == q21.3

(defclass k46_XX_ins!2!!q13p13p23!
  :label "The 46,XX,ins(2)(q13p13p23) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/InverseInsertion (owlsome e/hasReceivingBreakPoint h/HumanChromosome2Bandq13) (owlsome e/hasProvidingBreakPoint h/HumanChromosome2Bandp13 h/HumanChromosome2Bandp23))))

(defclass k46_XX_ins!2!!q13p23p13!
  :label "The 46,XX,ins(2)(q13p23p13) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/DirectInsertion (owlsome e/hasReceivingBreakPoint h/HumanChromosome2Bandq13) (owlsome e/hasProvidingBreakPoint h/HumanChromosome2Bandp23 h/HumanChromosome2Bandp13))))

(defclass k46_XX_ins!2!!p13q21q31!
  :label "The 46,XX,ins(2)(p13q21q31) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/DirectInsertion (owlsome e/hasReceivingBreakPoint h/HumanChromosome2Bandp13) (owlsome e/hasProvidingBreakPoint h/HumanChromosome2Bandq21.3 h/HumanChromosome2Bandq31)))) ;; TOFIX q21 == q21.3

(defclass k46_XY_ins!2!!p13q31q21!
  :label "The 46,XY,ins(2)(p13q31q21) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent (owland e/InverseInsertion (owlsome e/hasReceivingBreakPoint h/HumanChromosome2Bandp13) (owlsome e/hasProvidingBreakPoint h/HumanChromosome2Bandq31 h/HumanChromosome2Bandq21.3)))) ;; TOFIX q21 == q21.3

(defclass k46_XY_ins!5_2!!p14_q22q32!
  :label "The 46,XY,ins(5;2)(p14;q22q32) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent (owland e/DirectInsertion (owlsome e/hasReceivingBreakPoint h/HumanChromosome5Bandp14) (owlsome e/hasProvidingBreakPoint h/HumanChromosome2Bandq22 h/HumanChromosome2Bandq32.3)))) ;; TOFIX q32 == q32.3

(defclass k46_XY_ins!5_2!!p14_q32q22!
  :label "The 46,XY,ins(5;2)(p14;q32q22) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent (owland e/InverseInsertion (owlsome e/hasReceivingBreakPoint h/HumanChromosome5Bandp14) (owlsome e/hasProvidingBreakPoint h/HumanChromosome2Bandq32.3 h/HumanChromosome2Bandq22)))) ;;TOFIX q32 == q32.3

(defclass k46_XX_ins!5_2!!q31_p13p23!
  :label "The 46,XX,ins(5;2)(q31;p13p23) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/DirectInsertion (owlsome e/hasReceivingBreakPoint h/HumanChromosome5Bandq31.3) (owlsome e/hasProvidingBreakPoint h/HumanChromosome2Bandp13 h/HumanChromosome2Bandp23)))) ;;TOFIX q31 == q31.3

(defclass k46_XX_ins!5_2!!q31_p23p13!
  :label "The 46,XX,ins(5;2)(q31;p23p13) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/InverseInsertion (owlsome e/hasReceivingBreakPoint h/HumanChromosome5Bandq31.3) (owlsome e/hasProvidingBreakPoint h/HumanChromosome2Bandp23 h/HumanChromosome2Bandp13)))) ;;TOFIX q31 == q31.3

;; TRANSLOCATIONS
(defclass k46_XY_t!2_5!!q21_q31!
  :label "The 46,XY,t(2;5)(q21;q31) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent
           (owland e/Translocation
                   (owlsome e/hasBreakPoint h/HumanChromosome2Bandq21.3 h/HumanChromosome2BandqTer h/HumanChromosome5Bandq31.3 h/HumanChromosome5BandqTer)))) ;;TOFIX q21 == q21.3 && q31 == q31.3

(defclass k46_XY_t!2_5!!p12_q31!
  :label "The 46,XY,t(2;5)(p12;q31) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent
           (owland e/Translocation
                   (owlsome e/hasBreakPoint h/HumanChromosome2BandpTer h/HumanChromosome2Bandp12 h/HumanChromosome5Bandq31.3 h/HumanChromosome5BandqTer)))) ;;TOFIX q31 == q31.1

(defclass k46_X_t!X_13!!q27_q12!
  :label "The 46,X,t(X;13)(q27;q12) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent
           (owland e/Translocation
                   (owlsome e/hasBreakPoint h/HumanChromosomeXBandq27 h/HumanChromosomeXBandqTer h/HumanChromosome13Bandq12.3 h/HumanChromosome13BandqTer)))) ;;TOFIX q12 = q12.3

(defclass k46_t!X_Y!!q22_q11.2!
  :label "The 46,t(X;Y)(q22;q11.2) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent
           (owland e/Translocation
                   (owlsome e/hasBreakPoint h/HumanChromosomeXBandq22.3 h/HumanChromosomeXBandqTer h/HumanChromosomeYBandq11.23 h/HumanChromosomeYBandqTer)))) ;;TOFIX q22 == q22.3 && q11.2 == q11.23

(defclass k46_t!X_18!!p11.2_q11.2!_t!Y_1!!q11.2_p31!
  :label "The 46,t(X;Y)(q22;q11.2) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent
           (owland e/Translocation
                   (owlsome e/hasBreakPoint h/HumanChromosomeXBandp11.23 h/HumanChromosomeXBandpTer h/HumanChromosome18Bandq11.2 h/HumanChromosome18BandqTer))) ;;TOFIX p11.2 == p11.23
  (exactly 1 e/hasEvent
           (owland e/Translocation
                   (owlsome e/hasBreakPoint h/HumanChromosomeYBandq11.23 h/HumanChromosomeYBandqTer h/HumanChromosome1Bandp31.3 h/HumanChromosome1BandpTer)))) ;;TOFIX q11.2 == q11.23 && p31 == p31.3

;; CORRECT?
(defclass k46_XX_t!2_7_5!!p21_q22_q23!
  :label "The 46,XX,t(2;7;5)(p21;q22;q23) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Translocation (owland
         (owlsome e/hasProvidingBreakPoint h/HumanChromosome2Bandp21)
         (owlsome e/hasReceivingBreakPoint h/HumanChromosome7Bandq22))
        (owland
         (owlsome e/hasProvidingBreakPoint h/HumanChromosome7Bandq22)
         (owlsome e/hasReceivingBreakPoint h/HumanChromosome5Bandq23.3))
        (owland
         (owlsome e/hasProvidingBreakPoint h/HumanChromosome5Bandq23.3)
         (owlsome e/hasReceivingBreakPoint h/HumanChromosome2Bandp21))))) ;;TOFIX q23 == q23.3

;; CORRECT?
(defclass k46_X_t!X_22_1!!q24_q11.2_p33!
  :label "The 46,X,t(X;22;1)(q24;q11.2;p33) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Translocation (owland
         (owlsome e/hasProvidingBreakPoint h/HumanChromosomeXBandq24)
         (owlsome e/hasReceivingBreakPoint h/HumanChromosome22Bandq11.2))
        (owland
         (owlsome e/hasProvidingBreakPoint h/HumanChromosome22Bandq11.2)
         (owlsome e/hasReceivingBreakPoint h/HumanChromosome1Bandp33))
        (owland
         (owlsome e/hasProvidingBreakPoint h/HumanChromosome1Bandp33)
         (owlsome e/hasReceivingBreakPoint h/HumanChromosomeXBandq24)))))

;; TODO How do I model that 2 distinct Chromosomes 7 are involved?
;; 46,XX,t(3;7;7*)(q21;q22;p13)

;; CORRECT?
(defclass k46_XX_t!3_9_22_21!!p13_q34_q11.2_q21!
  :label "The 46,XX,t(3;9;22;21)(p13;q34;q11.2;q21) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent
           (owland e/Translocation
                   (owland
                    (owlsome e/hasProvidingBreakPoint h/HumanChromosome3Bandp13)
                    (owlsome e/hasReceivingBreakPoint h/HumanChromosome9Bandq34.3))
                   (owland
                    (owlsome e/hasProvidingBreakPoint h/HumanChromosome9Bandq34.3) 
                    (owlsome e/hasReceivingBreakPoint h/HumanChromosome22Bandq11.2))
                   (owland
                    (owlsome e/hasProvidingBreakPoint h/HumanChromosome22Bandq11.2)
                    (owlsome e/hasReceivingBreakPoint h/HumanChromosome21Bandq21))
                   (owland
                    (owlsome e/hasProvidingBreakPoint h/HumanChromosome21Bandq21)
                    (owlsome e/hasReceivingBreakPoint h/HumanChromosome3Bandp13))))) ;;TOFIX q34 == q34.3

;; TODO How do I model that 2 distinct Chromosomes 7 are involved?
;; 46,XX,t(3;9;9*;22)(p13;q22;q34;q11.2)

;; CORRECT?
(defclass k46_XY_t!5_6!!q13q23_q15q23!
  :label "The 46,XY,t(5;6)(q13q23;q15q23) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent
           (owland e/Translocation
                   (owlsome e/hasBreakPoint h/HumanChromosome5Bandq13.3 h/HumanChromosome5Bandq23.3 h/HumanChromosome6Bandq15 h/HumanChromosome6Bandq23.3)))) ;;TOFIX q13 == q13.3 && q23 == q23.3 (x2)

;; CORRECT?
(defclass k46_XX_t!5_14_9!!q13q23_q24q21_p12p23!
  :label "The 46,XX,t(5;14;9)(q13q23;q24q21;p12p23) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent
           (owland e/Translocation
                   (owland
                    (owlsome e/hasProvidingBreakPoint h/HumanChromosome5Bandq13.3)
                    (owlsome e/hasProvidingBreakPoint h/HumanChromosome5Bandq23.3)
                    (owlsome e/hasReceivingBreakPoint h/HumanChromosome14Bandq24.3)
                    (owlsome e/hasReceivingBreakPoint h/HumanChromosome14Bandq21))
                   (owland
                    (owlsome e/hasProvidingBreakPoint h/HumanChromosome14Bandq24.3)
                    (owlsome e/hasProvidingBreakPoint h/HumanChromosome14Bandq21)
                    (owlsome e/hasReceivingBreakPoint h/HumanChromosome9Bandp12)
                    (owlsome e/hasReceivingBreakPoint h/HumanChromosome9Bandp23))
                   (owland
                    (owlsome e/hasProvidingBreakPoint h/HumanChromosome9Bandp12)
                    (owlsome e/hasProvidingBreakPoint h/HumanChromosome9Bandp23)
                    (owlsome e/hasReceivingBreakPoint h/HumanChromosome5Bandq13.3)
                    (owlsome e/hasReceivingBreakPoint h/HumanChromosome5Bandq23.3))))) ;;TOFIX q13 == q13.3 && q23 == q23.3 && q24 = q24.3

;; CORRECT?
(defclass k46_XY_t!1_3!!p10_q10!
  :label "The 46,XY,t(1;3)(p10;q10) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent
           (owland e/Translocation
                   (owlsome e/hasBreakPoint h/HumanChromosome1BandCen h/HumanChromosome1BandpTer h/HumanChromosome3BandCen h/HumanChromosome3BandqTer)))) 

;; CORRECT?
(defclass k46_XY_t!1_3!!p10_p10!
  :label "The 46,XY,t(1;3)(p10;p10) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent
           (owland e/Translocation
                   (owlsome e/hasBreakPoint h/HumanChromosome1BandCen h/HumanChromosome1BandpTer h/HumanChromosome3BandCen h/HumanChromosome3BandpTer)))) 

;; TODO hasEvent definitions for the following karyotypes
(defclass k46_XY_t!12_16!!q13_p11.1!
  :label "The 46,XY,t(12;16)(q13;p11.1) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  ;;(exactly 1 e/hasEvent e/Translocation)
  )
(defclass k46_X_t!X_18!!p11.1_q11.1!
  :label "The 46,X,t(X;18)(p11.1;q11.1) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XN)
  ;; (exactly 1 e/hasEvent e/Translocation)
  )
(defclass k46_XX_t!9_22_17!!q34_q11.2_q22!
  :label "The 46,XX,t(9;22;17)(q34;q11.2;q22) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  ;; (exactly 1 e/hasEvent e/Translocation)
  )
(defclass k46_XY_t!X_15_18!!p11.1_p11.1_q11.1!
  :label "The 46,XY,t(X;15;18)(p11.1;p11.1;q11.1) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  ;; (exactly 1 e/hasEvent e/Translocation)
  )
(defclass k46_XX_t!3_9_22_21!!p13_q34_q11.2_q21!
  :label "The 46,XX,t(3;9;22;21)(p13;q34;q11.2;q21) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  ;; (exactly 1 e/hasEvent e/Translocation)
  )
(defclass k46_XY_t!5_6!!q13q23_q15q23!
  :label "The 46,XY,t(5;6)(q13q23;q15q23) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  ;; (exactly 1 e/hasEvent e/Translocation)
  )
(defclass k46_t!X_18!!p11.1_q11.2!_t!Y_1!!q11.2_p13!
  :label "The 46,t(X;18)(p11.1;q11.2),t(Y;1)(q11.2;p13) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  ;; (exactly 1 e/hasEvent e/Translocation)
  ;; (exactly 1 e/hasEvent e/Translocation)
  )
(defclass k46_Y_t!X_8!!p22.3_q24.1!
  :label "The 46,Y,t(X;8)(p22.3;q24.1) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  ;; (exactly 1 e/hasEvent e/Translocation)
  )

;; QUADRUPLICATIONS
;; TOFIX: It is not possible to indicate the orientations of the segments with the short system!
(defclass k46_XX_qdp!1!!q23q32!
  :label "The 46,XX,qdp(1)(q23q32) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Quadruplication (owlsome e/hasBreakPoint h/HumanChromosome1Bandq23.1 h/HumanChromosome1Bandq32.3))))

;; TRIPLICATIONS
;; TOFIX: It is not possible to indicate the orientations of the segments with the short system!
(defclass k46_XX_trp!1!!q21q32!
  :label "The 46,XX,trp(1)(q21q32) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/DirectTriplication (owlsome e/hasBreakPoint h/HumanChromosome1Bandq23.1 h/HumanChromosome1Bandq32.3))))

(defclass k46_XX_invtrp!1!!q32q21!
  :label "The 46,XX,inv trp(1)(q32q21) karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/InverseTriplication (owlsome e/hasBreakPoint h/HumanChromosome1Bandq23.1 h/HumanChromosome1Bandq32.3))))

;; MIXTURE
(defclass k47_X_t!X_13!!q27_q12!_inv!10!!p13q22!_+21
  :label "The 47,X,t(X;13)(q27;q12),inv(10)(p13q22),+21 karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome21))
  ;; (exactly 1 e/hasEvent e/Translocation)
  ;; (exactly 1 e/hasEvent e/Inversion)
  )

(defclass k48_X_t!Y_12!!q11.2_p12!_del!6!!q11!_+8_t!9_22!!q34_q11.2!_+17_-21_+22
  :label "The 48,X,t(Y;12)(q11.2;p12),del(6)(q11),+8,t(9;22)(q34;q11.2),+17,-21,+22 karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XY)
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome8))
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome17))
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome22))
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosome21))
  (exactly 1 e/hasEvent (owland e/Deletion (owlsome e/hasBreakPoint h/HumanChromosome6Bandq11)))
  ;; (exactly 1 e/hasEvent e/Translocation)
  ;; (exactly 1 e/hasEvent e/Translocation)
)

(defclass k49_X_inv!X!!p21q26!_+3_inv!3!!q21q26.2!_+7_+10_-20_del!20!!q11.2!_+21
  :label "The 49,X,inv(X)(p21q26),+3,inv(3)(q21q26.2),+7,+10,-20,del(20)(q11.2),+21  karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome3))
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome7))
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome10))
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome21))
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosome20))
  ;; (exactly 2 e/hasEvent e/Inversion)
  ;; (exactly 1 e/hasEvent e/Deletion) 
  )

(defclass k50_XX_+1_del!1!!p13!_+dup!1!!q21q32!_+inv!1!!p31q41!_+8_r!10!!p12q25!_-21
  :label "The 50,XX,+1,del(1)(p13),+dup(1)(q21q32),+inv(1)(p31q41),+8,r(10)(p12q25),-21 karyotype"
  :subclass ISCNExampleKaryotype
  (owlsome n/derivedFrom n/k46_XX)
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome1))
  (exactly 1 e/hasEvent (owland e/Addition h/HumanChromosome8))
  (exactly 1 e/hasEvent (owland e/Deletion h/HumanChromosome21))
  ;; (exactly 1 e/hasEvent e/Deletion) 
  ;; (exactly 1 e/hasEvent e/Duplication) Addition and Duplication
  ;; (exactly 1 e/hasEvent e/Inversion) Addition and Inversion
  ;; r(10)?
  )



;; NOT HUMAN?
;; 69,XXX,del(7)(p11.2)
;; 69,XXY,del(7)(q22),inv(7)(p13q22),t(7;14)(p15;q11.1)
;; 70,XXX,+del(7)(p11.2)
;; 92,XXYY,del(7)(p11.2),t(7;14)(p15;q11.1)
;; 92,XXYY,del(7)(p11.2),del(7)(q22),del(7)(q34)

;; DICENTRIC
;; 45,XX,dic(13;15)(q22;q24)

;; DERIVATIVES
;; 46,Y,der(X)t(X;8)(p22.3;q24.1)
;; 46,XX,der(1)t(1;3)(p22;q13.1)
;; 45,XY,der(1)t(1;3)(p22;q13.1),-3
;; 46,XX,der(1)t(1;3)(p32;q21)t(1;11)(q25;q13)
;; 46,XY,der(1)t(1;3)(p32;q21)t(3;7)(q28;q11.2)
;; 46,XX,der(1)t(1;11)(p32;q13)t(1;3)(q25;q21)
;; 46,XY,der(1)t(t1;3)(p22;q13.1)
;; 45,XY,-10,der(10)t(10;17)(q22;p12)              

;; 46,XY,der(1)t(1;3)(p32;q21)dup(1)(q25q42)
;; 46,XY,der(9)del(9)(p12)t(9;13)(q34;q11)
;; 46,XY,der(9)del(9)(p12)del(9)(q31)
;; 46,XY,der(9)inv(9)(p13p23)del(9)(q22q33)
;; 46,XX,der(7)add(7)(p22)add(7)(q22)
;; 46,XX,der(5)add(5)(p15.1)del(5)(q13)
;; 46,XX,der(5)add(5)(p15.3)add(5)(q23)

;; 46,XX,rec(6)dup(6p)inv(6)(p22.2q25.2)mat



