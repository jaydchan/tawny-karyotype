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
  (:use [owl.owl])
  (:require [owl [reasoner :as r]]
            [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]
            [ncl.karyotype [events :as e]]
            )
  )

(defontology named
  :iri "http://ncl.ac.uk/karyotype/named"
  :prefix "nmd:"
  )

;;import karyotype, human and event axioms
;;(owlimport k/karyotype)
;;(owlimport h/human)
;;(owlimport e/event)

(defclass NamedKaryotype
  :subclass k/Karyotype
  )

(defclass BaseKaryotype
  :subclass k/Karyotype
  )

;; define object properties
(defoproperty derivedFrom
  ;; :range k/Karyotype
  :domain NamedKaryotype
  )

(defoproperty hasLoss
  :range k/Chromosome
  :domain NamedKaryotype
  )

(defoproperty hasAddition
  :range k/Chromosome
  :domain NamedKaryotype
  )

;; define all the baseKaryotypes
 ;;we have to pass these in as strings because they start with
 ;;integers which brings up an NumberFormatException therefore we
 ;;could use :name "46_XX" or :label "The 46,XX karyotype"
;; define all haploid base karyotypes
(defclass k23_N
  :label "The 23,N karyotype"
  :subclass BaseKaryotype
  )
(defclass k23_X
  :label "The 23,X karyotype"
  :subclass k23_N
  )
(defclass k23_Y
  :label "The 23,Y karyotype"
  :subclass k23_N
  )
;; define all diploid base karyotypes
(defclass k46_XN
  :label "The 46,XN karyotype"
  :subclass BaseKaryotype
  )
(defclass k46_XX
  :label "The 46,XX karyotype"
  :subclass k46_XN
  )
(defclass k46_XY
  :label "The 46,XY karyotype"
  :subclass k46_XN
  )
;; TODO
;; define all triploid base karyotypes
(defclass k69_N
  :label "The 69,N karyotype"
  :subclass BaseKaryotype
  )
(defclass k69_X
  :label "The 69,X karyotype"
  :subclass k69_N
  )
(defclass k69_Y
  :label "The 69,Y karyotype"
  :subclass k69_N
  )
;; TODO
;; define all tetraploid base karyotypes
(defclass k92_N
  :label "The 92,N karyotype"
  :subclass BaseKaryotype
  )
(defclass k92_X
  :label "The 92,X karyotype"
  :subclass k92_N
  )
(defclass k92_Y
  :label "The 92,Y karyotype"
  :subclass k92_N
  )

;; TODO Check disjoints as 47,XXX and 47,XX,+X are the same karyotype!
;; define the namedKaryotypes
(as-disjoint-subclasses
 NamedKaryotype
 ;; example numerical autosomal abnormal karyotypes
 (defclass k45_X
   :label "The 45,X karyotype"
   :subclass (owlsome derivedFrom k46_XN)
    (exactly 1 hasLoss h/HumanAutosome)
   )
 (defclass k45_Y
   :label "The 45,Y karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    (exactly 1 hasAddition h/HumanChromosomeY)
   )
 (defclass k46_YY
   :label "The 46,YY karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    (exactly 1 hasLoss h/HumanChromosomeX)
    (exactly 1 hasAddition h/HumanChromosomeY)
   )
 (defclass k47_XXX
   :label "The 47,XXX karyotype"
   :subclass (owlsome derivedFrom k46_XX)
    (exactly 1 hasAddition h/HumanChromosomeX)
   )
 (defclass k47_XXY
   :label "The 47,XXY karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    (exactly 1 hasAddition h/HumanChromosomeX)
   )
 (defclass k47_XYY
   :label "The 47,XYY karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    (exactly 1 hasAddition h/HumanChromosomeY)
   )
 (defclass k47_YYY
   :label "The 47,YYY karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    (exactly 2 hasAddition h/HumanChromosomeY)
    (exactly 1 hasLoss h/HumanChromosomeX)
   )
 (defclass k48_XXXX
   :label "The 48,XXXX karyotype"
   :subclass (owlsome derivedFrom k46_XX)
    (exactly 2 hasAddition h/HumanChromosomeX)
   )
 (defclass k48_XXXY
   :label "The 48,XXXY karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    (exactly 2 hasAddition h/HumanChromosomeX)
   )
 (defclass k48_XXYY
   :label "The 48,XXYY karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    (exactly 1 hasAddition h/HumanChromosomeX)
    (exactly 1 hasAddition h/HumanChromosomeY)
   )
 (defclass k48_XYYY
   :label "The 48,XYYY karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    (exactly 2 hasAddition h/HumanChromosomeY)
   )
 (defclass k48_YYYY
   :label "The 48,YYYY karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    (exactly 3 hasAddition h/HumanChromosomeY)
    (exactly 1 hasLoss h/HumanChromosomeX)
   )
 (defclass k49_XXXXX
   :label "The 49,XXXXX karyotype"
   :subclass (owlsome derivedFrom k46_XX)
    (exactly 3 hasAddition h/HumanChromosomeX)
   )
 (defclass k49_XXXXY
   :label "The 49,XXXXY karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    (exactly 3 hasAddition h/HumanChromosomeX)
   )
 (defclass k49_XXXYY
   :label "The 49,XXXYY karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    (exactly 1 hasAddition h/HumanChromosomeY)
    (exactly 2 hasAddition h/HumanChromosomeX)
   )
 (defclass k49_XXYYY
   :label "The 49,XXYYY karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    (exactly 2 hasAddition h/HumanChromosomeY)
    (exactly 1 hasAddition h/HumanChromosomeX)
   )
 (defclass k49_XYYYY
   :label "The 49,XYYYY karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    (exactly 3 hasAddition h/HumanChromosomeY)
   )
 (defclass k49_YYYYY
   :label "The 49,YYYYY karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    (exactly 4 hasAddition h/HumanChromosomeY)
    (exactly 1 hasLoss h/HumanChromosomeX)
    )
  (defclass k47_XX_+X
   :label "The 47,XX,+X karyotype"
   :subclass (owlsome derivedFrom k46_XX)
    (exactly 1 hasAddition h/HumanChromosomeX)
   )
 (defclass k45_X_-X
   :label "The 45,X,-X karyotype"
   :subclass (owlsome derivedFrom k46_XX)
    (exactly 1 hasLoss h/HumanChromosomeX)
   )
 (defclass k45_X_-Y
   :label "The 45,X,-Y karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    (exactly 1 hasLoss h/HumanChromosomeY)
   )
 (defclass k45_Y_-X
   :label "The 45,Y,-X karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    (exactly 1 hasLoss h/HumanChromosomeX)
   )
 (defclass k48_XY_+X_+Y
   :label "The 48,XY,+X,+Y karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    (exactly 1 hasAddition h/HumanChromosomeX)
    (exactly 1 hasAddition h/HumanChromosomeY)
   )
 ;; example haploid karyotype
 (defclass k26_X_+4_+6_+21
   :label "The 26,X,+4,+6,+21 karyotype"
   :subclass (owlsome derivedFrom k23_X)
    (exactly 1 hasAddition h/HumanChromosome4)
    (exactly 1 hasAddition h/HumanChromosome6)
    (exactly 1 hasAddition h/HumanChromosome21)
    )
 ;; example triploid karyotype
 ;; TODO derivedFrom
 (defclass k71_XXX_+8_+10
   :label "The 71,XXX,+8,+10 karyotype"
   :subclass (exactly 1 hasAddition h/HumanChromosome8)
    (exactly 1 hasAddition h/HumanChromosome10)
    )
 ;; example tetraploid karyotype
 ;; TODO derivedFrom
 (defclass k89_XXYY_-1_-3_-5_+8_-21
   :label "The 89,XXYY,-1,-3,-5,+8,-21 karyotype"
   :subclass (exactly 1 hasAddition h/HumanChromosome8)
    (exactly 1 hasLoss h/HumanChromosome1)
    (exactly 1 hasLoss h/HumanChromosome3)
    (exactly 1 hasLoss h/HumanChromosome5)
    (exactly 1 hasLoss h/HumanChromosome21)
    )
 ;; example numerical allosomal abnormal karyotypes
  (defclass k45_XX_-22
   :label "The 45,XX,-22 karyotype"
   :subclass (owlsome derivedFrom k46_XX)
    (exactly 1 hasLoss h/HumanChromosome22)
   )
 (defclass k46_XX_+8_-21
   :label "The 46,XX,+8,-21 karyotype"
   :subclass (owlsome derivedFrom k46_XX)
    (exactly 1 hasAddition h/HumanChromosome8)
    (exactly 1 hasLoss h/HumanChromosome21)
    )
  (defclass k47_XX_+21
   :label "The 47,XX,+21 karyotype"
   :subclass (owlsome derivedFrom k46_XX)
    (exactly 1 hasAddition h/HumanChromosome21)
   )
 (defclass k48_XX_+13_+21
   :label "The 48,XX,+13,+21 karyotype"
   :subclass (owlsome derivedFrom k46_XX)
    (exactly 1 hasAddition h/HumanChromosome13)
    (exactly 1 hasAddition h/HumanChromosome21)
    )
 ;; example conditional karyotypes
 (defclass k48_XXYc_+X
   :label "The 48,XXYc,+X karyotype"
   :subclass
    (owlsome derivedFrom (owland (owlsome derivedFrom k46_XY) (exactly 1 hasAddition h/HumanChromosomeX))) ;;aka 47,XXY
    (exactly 1 hasAddition h/HumanChromosomeX)
   )
 (defclass k46_Xc_+X
   :label "The 46,Xc,+X karyotype"
   :subclass
    (owlsome derivedFrom (owland (owlsome derivedFrom k46_XN) (exactly 1 hasLoss h/HumanAutosome))) ;;aka 45,X
    (exactly 1 hasAddition h/HumanChromosomeX)
   )
 (defclass k46_XXYc_-X
   :label "The 46,XXYc,-X karyotype"
   :subclass
    (owlsome derivedFrom (owland (owlsome derivedFrom k46_XY) (exactly 1 hasAddition h/HumanChromosomeX))) ;;aka 47,XXY    
    (exactly 1 hasLoss h/HumanChromosomeX)
   )
 (defclass k44_Xc_-X
   :label "The 44,Xc,-X karyotype"
   :subclass
    (owlsome derivedFrom (owland (owlsome derivedFrom k46_XN) (exactly 1 hasLoss h/HumanAutosome))) ;;aka 45,X 
    (exactly 1 hasLoss h/HumanChromosomeX)
   )
 (defclass k46_Xc_+21
   :label "The 46,Xc,+21 karyotype"
   :subclass
    (owlsome derivedFrom (owland (owlsome derivedFrom k46_XN) (exactly 1 hasLoss h/HumanAutosome))) ;;aka 45,X
    (exactly 1 hasAddition h/HumanChromosome21)
   )
  (defclass k48_XY_+21c_+21
   :label "The 48,XY,+21c,+21 karyotype"
   :subclass
    (owlsome derivedFrom (owland (owlsome derivedFrom k46_XY) (exactly 1 hasAddition h/HumanChromosome21))) ;;aka 47,XY,+21
    (exactly 1 hasAddition h/HumanChromosome21)
   )
 (defclass k46_XY_+21c_-21
   :label "The 46,XY,+21c,-21 karyotype"
   :subclass
    (owlsome derivedFrom (owland (owlsome derivedFrom k46_XY) (exactly 1 hasAddition h/HumanChromosome21))) ;;aka 47,XY,+21 
    (exactly 1 hasLoss h/HumanChromosome21)
   )
 ;; example strutural abnormal karyotypes
 ;; TODO
 (defclass k46_XX_inv!2!!p21q31!
   :label "The 46,XX,inv(2)(p21q31) karyotype"
   :subclass (owlsome derivedFrom k46_XX)
   ;; ERROR: owl.owl.AxiomedEntity cannot be cast to clojure.lang.IFn
   ;;(exactly 1 e/hasEvent (owland (e/Inversion) (owlsome e/hasBreakPoint h/HumanChromosome2Band2p21.3))) ;;h/HumanChromosome2Band2p21 == h/HumanChromosome2Band2p21.3 ;;h/HumanChromosome2Band2q31 == h/HumanChromosome2Band2q31.3
    ;;(owlsome effects [h/HumanChromosome2Band2p13 All bands between the two breakpoints h/HumanChromosome2Band2p23])
   )
 (defclass k46_XX_inv!2!!p13p23!
   :label "The 46,XX,inv(2)(p13p23) karyotype"
   :subclass (owlsome derivedFrom k46_XX)
    ;; (exactly 1 e/hasEvent (owland (e/Inversion) (owlsome e/hasBreakPoint [h/HumanChromosome2Band2p13.3 h/HumanChromosome2Band2p23.3])))
   )
 (defclass k46_XY_t!12_16!!q13_p11.1!
   :label "The 46,XY,t(12;16)(q13;p11.1) karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    ;; (exactly 1 e/hasEvent e/Translocation)
   )
 (defclass k46_X_t!X_18!!p11.1_q11.1!
   :label "The 46,X,t(X;18)(p11.1;q11.1) karyotype"
   :subclass (owlsome derivedFrom k46_XN)
    ;; (exactly 1 e/hasEvent e/Translocation)
   )
 (defclass k46_X_ins!5_X!!p14_q21q25!
   :label "The 46,X,ins(5;X)(p14;q21q25) karyotype"
   :subclass (owlsome derivedFrom k46_XN)
    ;; (exactly 1 e/hasEvent e/Insertion)
   )
 (defclass k46_XY_ins!5_2!!p14_q22q32!
   :label "The 46,XY,ins(5;2)(p14;q22q32) karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    ;; (exactly 1 e/hasEvent e/Insertion)
   )
 (defclass k46_XX_ins!2!!q13p13p23!
   :label "The 46,XX,ins(2)(q13p13p23) karyotype"
   :subclass (owlsome derivedFrom k46_XX)
    ;; (exactly 1 e/hasEvent e/Insertion)
   )
 (defclass k46_XX_ins!2!!q13p23p13!
   :label "The 46,XX,ins(2)(q13p23p13) karyotype"
   :subclass (owlsome derivedFrom k46_XX)
    ;; (exactly 1 e/hasEvent e/Insertion)
   )
 (defclass k46_XX_t!9_22_17!!q34_q11.2_q22!
   :label "The 46,XX,t(9;22;17)(q34;q11.2;q22) karyotype"
   :subclass (owlsome derivedFrom k46_XX)
    ;; (exactly 1 e/hasEvent e/Translocation)
   )
 (defclass k46_XY_t!X_15_18!!p11.1_p11.1_q11.1!
   :label "The 46,XY,t(X;15;18)(p11.1;p11.1;q11.1) karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    ; (exactly 1 e/hasEvent e/Translocation)
   )
 (defclass k46_XX_t!3_9_22_21!!p13_q34_q11.2_q21!
   :label "The 46,XX,t(3;9;22;21)(p13;q34;q11.2;q21) karyotype"
   :subclass (owlsome derivedFrom k46_XX)
    ;; (exactly 1 e/hasEvent e/Translocation)
   )
 (defclass k46_XY_t!5_6!!q13q23_q15q23!
   :label "The 46,XY,t(5;6)(q13q23;q15q23) karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    ;; (exactly 1 e/hasEvent e/Translocation)
   )
 (defclass k47_X_t!X_13!!q27_q12!_inv!10!!p13q22!_+21
   :label "The 47,X,t(X;13)(q27;q12),inv(10)(p13q22),+21 karyotype"
   :subclass (owlsome derivedFrom k46_XX)
    (exactly 1 hasAddition h/HumanChromosome21)
    ;; (exactly 1 e/hasEvent e/Translocation)
    ;; (exactly 1 e/hasEvent e/Inversion)
   )
 (defclass k46_t!X_18!!p11.1_q11.2!_t!Y_1!!q11.2_p13!
   :label "The 46,t(X;18)(p11.1;q11.2),t(Y;1)(q11.2;p13) karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    ;; (exactly 2 e/hasEvent e/Translocation)
   )
 (defclass k48_X_t!Y_12!!q11.2_p12!_del!6!!q11!_+8_t!9_22!!q34_q11.2!_+17_-21_+22
   :label "The 48,X,t(Y;12)(q11.2;p12),del(6)(q11),+8,t(9;22)(q34;q11.2),+17,-21,+22 karyotype"
   :subclass (owlsome derivedFrom k46_XY)
    (exactly 1 hasAddition h/HumanChromosome8)
    (exactly 1 hasAddition h/HumanChromosome17)
    (exactly 1 hasAddition h/HumanChromosome22)
    (exactly 1 hasLoss h/HumanChromosome21)
    ;; (exactly 1 e/hasEvent e/Translocation)
    ;; (exactly 1 e/hasEvent e/Deletion)
    ;; (exactly 1 e/hasEvent e/Translocation)
   )
 (defclass k49_X_inv!X!!p21q26!_+3_inv!3!!q21q26.2!_+7_+10_-20_del!20!!q11.2!_+21
   :label "The 49,X,inv(X)(p21q26),+3,inv(3)(q21q26.2),+7,+10,-20,del(20)(q11.2),+21  karyotype"
   :subclass (owlsome derivedFrom k46_XX)
    (exactly 1 hasAddition h/HumanChromosome3)
    (exactly 1 hasAddition h/HumanChromosome7)
    (exactly 1 hasAddition h/HumanChromosome10)
    (exactly 1 hasAddition h/HumanChromosome21)
    (exactly 1 hasLoss h/HumanChromosome20)
    ;; (exactly 2 e/hasEvent e/Inversion)
    ;; (exactly 1 e/hasEvent e/Deletion) 
   )
 ;; TODO r(10)
 (defclass k50_XX_+1_del!1!!p13!_+dup!1!!q21q32!_+inv!1!!p31q41!_+8_r!10!!p12q25!_-21
   :label "The 50,XX,+1,del(1)(p13),+dup(1)(q21q32),+inv(1)(p31q41),+8,r(10)(p12q25),-21 karyotype"
   :subclass (owlsome derivedFrom k46_XX)
    (exactly 3 hasAddition h/HumanChromosome1)
    (exactly 1 hasAddition h/HumanChromosome8)
    (exactly 1 hasLoss h/HumanChromosome21)
    ;; (exactly 1 e/hasEvent e/Deletion)
    ;; (exactly 1 e/hasEvent e/Duplication)
    ;; (exactly 1 e/hasEvent e/Inversion)
   )
 )

