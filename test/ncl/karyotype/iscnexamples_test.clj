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

(ns ncl.karyotype.iscnexamples_test
  (:use [clojure.test])
  (:require
   [ncl.karyotype.iscnexamples :as i]
   [ncl.karyotype.named :as n]
   [tawny.owl :as o]
   [tawny.reasoner :as r]))

(defn ontology-reasoner-fixture [tests]
  (r/reasoner-factory :hermit)
  (o/ontology-to-namespace i/iscnexamples)
  (binding [r/*reasoner-progress-monitor*
            (atom
            r/reasoner-progress-monitor-silent)]
    (tests)))

(use-fixtures :once ontology-reasoner-fixture)

;; to run: M-x 'lein' 'test'

(deftest Basic
  (is (r/consistent?))
  (is (r/coherent?)))

;; Ploidy Level
;; Haploid
(deftest HaploidKaryotype
  (is
   (r/isuperclass? i/k26_X_+4_+6_+21 n/HaploidKaryotype))
  )

;; Diploid
(deftest DiploidKaryotype
  (is (r/isuperclass? i/k44_Xc_-X n/DiploidKaryotype))
  (is (r/isuperclass? i/k45_X n/DiploidKaryotype))
  (is (r/isuperclass? i/k46_X_t!X_18!!p11.1_q11.1! n/DiploidKaryotype))
  (is (r/isuperclass? i/k46_Xc_+X n/DiploidKaryotype))
  )

;; Triploid
(deftest TriploidKaryotype
  (is (r/isuperclass? i/k69_XXX_del!7!!p11.2! n/TriploidKaryotype))
  (is (r/isuperclass?
       i/k69_XXY_del!7!!q22!_inv!7!!p13q22!_t!7_14!!p15_q11.1!
       n/TriploidKaryotype))
  (is (r/isuperclass? i/k70_XXX_+del!7!!p11.2! n/TriploidKaryotype))
  (is (r/isuperclass? i/k71_XXX_+8_+10 n/TriploidKaryotype))
  )

;; Tetraploid
(deftest TetraploidKaryotype
  (is (r/isuperclass? i/k89_XXYY_-1_-3_-5_+8_-21 n/TetraploidKaryotype))
  (is (r/isuperclass? i/k92_XXXX_t!8_14!!q24.1_q32!x2 n/TetraploidKaryotype))
  (is (r/isuperclass? i/k92_XXYY_del!7!!p11.2!_t!7_14!!p15_q11.1!
                      n/TetraploidKaryotype))
  (is (r/isuperclass? i/k92_XXYY_del!7!!p11.2!_del!7!!q22!_del!7!!q34!
                      n/TetraploidKaryotype))
  )

;; Gender
;; TODO Test conditional karyotypes
(deftest FemaleKaryotype
  (is (r/isuperclass?
       i/k49_X_inv!X!!p21q26!_+3_inv!3!!q21q26.2!_+7_+10_-20_del!20!!q11.2!_+21
       n/FemaleKaryotype))
  (is (r/isuperclass? i/k45_X_-X n/FemaleKaryotype))
  (is (r/isuperclass? i/k45_XX_-22 n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_X_ins!5_X!!p14_q21q25! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_X_ins!5_X!!p14_q21q25! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_X_t!X_13!!q27_q12! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_X_t!X_22_1!!q24_q11.2_p33! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_+8_-21 n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_add!19!!p13.3! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_del!5!!q13! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_del!5!!q13q33! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_del!5!!q13q13! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_del!5!!q?! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_del!6!!q13q23!x2 n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_dup!1!!q22q25! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_ins!1_?!!p22_?! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_ins!2!!p13q21q31! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_ins!2!!q13p13p23! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_ins!2!!q13p23p13! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_ins!5_2!!q31_p13p23! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_ins!5_2!!q31_p23p13! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_ins!5_?!!q13_?! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_ins!5_?!!q13_?! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_inv!2!!p21q31! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_inv!2!!p13p23! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_inv!3!!q21q26.2! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_qdp!1!!q23q32! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_t!2_7_5!!p21_q22_q23! n/FemaleKaryotype))
  (is
   (r/isuperclass? i/k46_XX_t!3_9_22_21!!p13_q34_q11.2_q21! n/FemaleKaryotype))
  (is
   (r/isuperclass? i/k46_XX_t!5_14_9!!q13q23_q24q21_p12p23! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_t!9_22_17!!q34_q11.2_q22! n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_trp!1!!q21q32! n/FemaleKaryotype))
  (is (r/isuperclass? i/k47_X_t!X_13!!q27_q12!_inv!10!!p13q22!_+21
                      n/FemaleKaryotype))
  (is (r/isuperclass? i/k47_XX_+21 n/FemaleKaryotype))
  (is (r/isuperclass? i/k47_XX_+X n/FemaleKaryotype))
  (is (r/isuperclass? i/k46_XX_del!6!!q13q23!x2 n/FemaleKaryotype))
  (is (r/isuperclass? i/k47_XXX n/FemaleKaryotype))
  (is (r/isuperclass? i/k48_XX_+13_+21 n/FemaleKaryotype))
  (is (r/isuperclass? i/k48_XX_+del!6!!q13q23!x2 n/FemaleKaryotype))
  (is (r/isuperclass? i/k48_XX_del!6!!q13q23!x2_+7_+7 n/FemaleKaryotype))
  (is (r/isuperclass? i/k48_XXXX n/FemaleKaryotype))
  (is (r/isuperclass? i/k49_XXXXX n/FemaleKaryotype))

  ;; FAILS due to the current definition of Female
  ;; (is (r/isuperclass? i/k45_X n/FemaleKaryotype))
  )

(deftest MaleKaryotype
  (is (r/isuperclass? i/k45_Y n/MaleKaryotype))
  (is (r/isuperclass? i/k46_XXYc_-X n/MaleKaryotype))
  )

;; Numerical Abnormalities
(deftest NumericalAbnormalKaryotype
  )

;; TOFIX
;; TODO - Need to decide on definition of abnormal - abnormal for
;; karyotype or abnormal for a human
(deftest NumericalAbnormalKaryotypeAllosomalGain
  (is (r/isuperclass? i/k46_Xc_+X n/NumericalAbnormalKaryotypeAllosomalGain))
  ;; (is (r/isuperclass? i/k46_YY n/NumericalAbnormalKaryotypeAllosomalGain))
  (is (r/isuperclass? i/k47_XX_+X n/NumericalAbnormalKaryotypeAllosomalGain))
  ;; (is (r/isuperclass? i/k47_XXX n/NumericalAbnormalKaryotypeAllosomalGain))
  ;; (is (r/isuperclass? i/k47_XYY n/NumericalAbnormalKaryotypeAllosomalGain))
  ;; (is (r/isuperclass? i/k47_YYY n/NumericalAbnormalKaryotypeAllosomalGain))
  ;; (is (r/isuperclass? i/k48_XXXX n/NumericalAbnormalKaryotypeAllosomalGain))
  ;; (is (r/isuperclass? i/k48_XYYY n/NumericalAbnormalKaryotypeAllosomalGain))
  ;; (is (r/isuperclass? i/k48_YYYY n/NumericalAbnormalKaryotypeAllosomalGain))
  ;; (is (r/isuperclass? i/k49_XXXXX n/NumericalAbnormalKaryotypeAllosomalGain))
  ;; (is (r/isuperclass? i/k49_XYYYY n/NumericalAbnormalKaryotypeAllosomalGain))
  ;; (is (r/isuperclass? i/k49_YYYYY n/NumericalAbnormalKaryotypeAllosomalGain))
  )

;; TOFIX
(deftest NumericalAbnormalKaryotypeAllosomalLoss
  (is (r/isuperclass? i/k44_Xc_-X n/NumericalAbnormalKaryotypeAllosomalLoss))
  (is (r/isuperclass? i/k45_X_-X n/NumericalAbnormalKaryotypeAllosomalLoss))
  (is (r/isuperclass? i/k45_X_-Y n/NumericalAbnormalKaryotypeAllosomalLoss))
  ;; (is (r/isuperclass? i/k45_Y n/NumericalAbnormalKaryotypeAllosomalLoss))
  (is (r/isuperclass? i/k45_Y_-X n/NumericalAbnormalKaryotypeAllosomalLoss))
  ;; (is (r/isuperclass? i/k46_YY n/NumericalAbnormalKaryotypeAllosomalLoss))
  (is (r/isuperclass? i/k46_XXYc_-X n/NumericalAbnormalKaryotypeAllosomalLoss))
  ;; (is (r/isuperclass? i/k47_YYY n/NumericalAbnormalKaryotypeAllosomalLoss))
  ;; (is (r/isuperclass? i/k48_YYYY n/NumericalAbnormalKaryotypeAllosomalLoss))
  ;; (is (r/isuperclass? i/k49_YYYYY n/NumericalAbnormalKaryotypeAllosomalLoss))
  )

(deftest NumericalAbnormalKaryotypeAutosomalGain
  (is
   (r/isuperclass? i/k26_X_+4_+6_+21 n/NumericalAbnormalKaryotypeAutosomalGain))
  (is (r/isuperclass? i/k47_XX_del!6!!q13q23!x2_+del!6!!q13q23!
                      n/NumericalAbnormalKaryotypeAutosomalGain))
  (is (r/isuperclass? i/k48_XX_+del!6!!q13q23!x2
                      n/NumericalAbnormalKaryotypeAutosomalGain))
  (is (r/isuperclass? i/k48_XX_del!6!!q13q23!x2_+7_+7
                      n/NumericalAbnormalKaryotypeAutosomalGain))
  (is (r/isuperclass? i/k70_XXX_+del!7!!p11.2!
                      n/NumericalAbnormalKaryotypeAutosomalGain))
  (is
   (r/isuperclass? i/k71_XXX_+8_+10 n/NumericalAbnormalKaryotypeAutosomalGain))
  (is (r/isuperclass? i/k89_XXYY_-1_-3_-5_+8_-21
                      n/NumericalAbnormalKaryotypeAutosomalGain))
  )

;; TOFIX
(deftest NumericalAbnormalKaryotypeAutosomalLoss
  (is (r/isuperclass? i/k45_XX_-22 n/NumericalAbnormalKaryotypeAutosomalLoss))
  (is
   (r/isuperclass? i/k46_XX_+8_-21 n/NumericalAbnormalKaryotypeAutosomalLoss))
  (is
   (r/isuperclass? i/k46_XY_+21c_-21 n/NumericalAbnormalKaryotypeAutosomalLoss))
  (is (r/isuperclass? i/k47_XY_-10_+fis!10!!p10!_+fis!10!!q10!
                      n/NumericalAbnormalKaryotypeAutosomalLoss))
  (is (r/isuperclass?
       i/k49_X_inv!X!!p21q26!_+3_inv!3!!q21q26.2!_+7_+10_-20_del!20!!q11.2!_+21
       n/NumericalAbnormalKaryotypeAutosomalLoss))
  (is (r/isuperclass? i/k89_XXYY_-1_-3_-5_+8_-21
                      n/NumericalAbnormalKaryotypeAutosomalLoss))

  ;; (is (r/isuperclass?
  ;;      i/k48_X_t!Y_12!!q11.2_p12!_del!6!!q11!_+8_t!9_22!!q34_q11.2!_+17_-21_+22
  ;;      n/NumericalAbnormalKaryotypeAutosomalLoss))
 ;; (is (r/isuperclass?
  ;;      i/k50_XX_+1_del!1!!p13!_+dup!1!!q21q32!_+inv!1!!p31q41!_+8_r!10!!p12q25!_-21
  ;;      n/NumericalAbnormalKaryotypeAutosomalLoss))
  )

;; Structural Abnormalities
(deftest StructuralAbnormalKaryotypeAddition
  (is (r/isuperclass? i/k46_XX_add!19!!p13.3!
                      n/StructuralAbnormalKaryotypeAddition))
  (is
   (r/isuperclass? i/k46_XY_add!12!!q13! n/StructuralAbnormalKaryotypeAddition))
  )

;; TOFIX
(deftest StructuralAbnormalKaryotypeDeletion
  (is (r/isuperclass?
       i/k49_X_inv!X!!p21q26!_+3_inv!3!!q21q26.2!_+7_+10_-20_del!20!!q11.2!_+21
       n/StructuralAbnormalKaryotypeDeletion))
  (is (r/isuperclass? i/k69_XXX_del!7!!p11.2!
                      n/StructuralAbnormalKaryotypeDeletion))
  (is (r/isuperclass? i/k69_XXY_del!7!!q22!_inv!7!!p13q22!_t!7_14!!p15_q11.1!
                      n/StructuralAbnormalKaryotypeDeletion))
  (is
   (r/isuperclass? i/k46_XX_del!5!!q13! n/StructuralAbnormalKaryotypeDeletion))
  (is (r/isuperclass? i/k46_XX_del!5!!q13q13!
                      n/StructuralAbnormalKaryotypeDeletion))
  (is (r/isuperclass? i/k46_XX_del!5!!q13q33!
                      n/StructuralAbnormalKaryotypeDeletion))
  (is
   (r/isuperclass? i/k46_XX_del!5!!q?! n/StructuralAbnormalKaryotypeDeletion))
  (is (r/isuperclass? i/k46_XX_del!6!!q13q23!x2
                      n/StructuralAbnormalKaryotypeDeletion))
  (is (r/isuperclass? i/k46_Y_del!X!!p21p21!
                      n/StructuralAbnormalKaryotypeDeletion))
  (is (r/isuperclass? i/k47_XX_del!6!!q13q23!x2_+del!6!!q13q23!
                      n/StructuralAbnormalKaryotypeDeletion))
  (is (r/isuperclass? i/k48_XX_+del!6!!q13q23!x2
                      n/StructuralAbnormalKaryotypeDeletion))
  (is (r/isuperclass? i/k48_XX_del!6!!q13q23!x2_+7_+7
                      n/StructuralAbnormalKaryotypeDeletion))
  (is (r/isuperclass? i/k70_XXX_+del!7!!p11.2!
                      n/StructuralAbnormalKaryotypeDeletion))
  (is (r/isuperclass? i/k92_XXYY_del!7!!p11.2!_del!7!!q22!_del!7!!q34!
                      n/StructuralAbnormalKaryotypeDeletion))
  (is (r/isuperclass? i/k92_XXYY_del!7!!p11.2!_t!7_14!!p15_q11.1!
                      n/StructuralAbnormalKaryotypeDeletion))

  ;; (is (r/isuperclass?
  ;;      i/k48_X_t!Y_12!!q11.2_p12!_del!6!!q11!_+8_t!9_22!!q34_q11.2!_+17_-21_+22
  ;;      n/StructuralAbnormalKaryotypeDeletion))
  )

(deftest StructuralAbnormalKaryotypeDuplication
  (is (r/isuperclass? i/k46_XX_dup!1!!q22q25!
                      n/StructuralAbnormalKaryotypeDuplication))
  (is (r/isuperclass? i/k46_XY_dup!1!!q25q22!
                      n/StructuralAbnormalKaryotypeDuplication))
  )

(deftest StructuralAbnormalKaryotypeFission
  (is (r/isuperclass? i/k47_XY_-10_+fis!10!!p10!_+fis!10!!q10!
                      n/StructuralAbnormalKaryotypeFission))
  )

(deftest StructuralAbnormalKaryotypeInsertion
  (is (r/isuperclass? i/k46_X_ins!5_X!!p14_q21q25!
                      n/StructuralAbnormalKaryotypeInsertion))
  (is (r/isuperclass? i/k46_XX_ins!1_?!!p22_?!
                      n/StructuralAbnormalKaryotypeInsertion))
  (is (r/isuperclass? i/k46_XX_ins!2!!p13q21q31!
                      n/StructuralAbnormalKaryotypeInsertion))
  (is (r/isuperclass? i/k46_XX_ins!2!!q13p13p23!
                      n/StructuralAbnormalKaryotypeInsertion))
  (is (r/isuperclass? i/k46_XX_ins!2!!q13p23p13!
                      n/StructuralAbnormalKaryotypeInsertion))
  (is (r/isuperclass? i/k46_XX_ins!5_2!!q31_p13p23!
                      n/StructuralAbnormalKaryotypeInsertion))
  (is (r/isuperclass? i/k46_XX_ins!5_2!!q31_p23p13!
                      n/StructuralAbnormalKaryotypeInsertion))
  (is (r/isuperclass? i/k46_XX_ins!5_?!!q13_?!
                      n/StructuralAbnormalKaryotypeInsertion))
  (is (r/isuperclass? i/k46_XY_ins!2!!p13q31q21!
                      n/StructuralAbnormalKaryotypeInsertion))
  (is (r/isuperclass? i/k46_XY_ins!5_2!!p14_q22q32!
                      n/StructuralAbnormalKaryotypeInsertion))
  (is (r/isuperclass? i/k46_XY_ins!5_2!!p14_q32q22!
                      n/StructuralAbnormalKaryotypeInsertion))
  )

(deftest StructuralAbnormalKaryotypeInversion
  (is (r/isuperclass?
       i/k49_X_inv!X!!p21q26!_+3_inv!3!!q21q26.2!_+7_+10_-20_del!20!!q11.2!_+21
       n/StructuralAbnormalKaryotypeInversion))
  (is (r/isuperclass? i/k69_XXY_del!7!!q22!_inv!7!!p13q22!_t!7_14!!p15_q11.1!
       n/StructuralAbnormalKaryotypeInversion))
  (is (r/isuperclass? i/k46_XX_inv!2!!p13p23!
                      n/StructuralAbnormalKaryotypeInversion))
  (is (r/isuperclass? i/k46_XX_inv!2!!p21q31!
                      n/StructuralAbnormalKaryotypeInversion))
  (is (r/isuperclass? i/k46_XX_inv!3!!q21q26.2!
                      n/StructuralAbnormalKaryotypeInversion))
  (is (r/isuperclass? i/k46_XY_inv!3!!p13q21!
                      n/StructuralAbnormalKaryotypeInversion))
  (is (r/isuperclass? i/k47_X_t!X_13!!q27_q12!_inv!10!!p13q22!_+21
                      n/StructuralAbnormalKaryotypeInversion))
  (is (r/isuperclass? i/k47_Y_t!X_13!!q27_q12!_inv!10!!p13q22!_+21
                      n/StructuralAbnormalKaryotypeInversion))

  ;; (is (r/isuperclass? i/k47_X_t!Y_13!!q27_q12!_inv!10!!p13q22!_+21
  ;;                     n/StructuralAbnormalKaryotypeInversion))
  ;; (is (r/isuperclass?
  ;;      i/k49_X_inv!X!!p21q26!_+3_inv!3!!p21q26.2!_+7_+10_-20_del!20!!q11.2!_+21
  ;;      n/StructuralAbnormalKaryotypeInversion))
  )

(deftest StructuralAbnormalKaryotypeQuadruplication
  (is (r/isuperclass? i/k46_XX_qdp!1!!q23q32!
                      n/StructuralAbnormalKaryotypeQuadruplication))
  )

(deftest StructuralAbnormalKaryotypeTranslocation

  (is (r/isuperclass? i/k69_XXY_del!7!!q22!_inv!7!!p13q22!_t!7_14!!p15_q11.1!
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k46_X_t!X_13!!q27_q12!
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k46_X_t!X_18!!p11.1_q11.1!
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k46_X_t!X_22_1!!q24_q11.2_p33!
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k46_XX_t!2_7_5!!p21_q22_q23!
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k46_XX_t!3_9_22_21!!p13_q34_q11.2_q21!
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k46_XX_t!5_14_9!!q13q23_q24q21_p12p23!
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k46_XX_t!9_22_17!!q34_q11.2_q22!
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k46_XY_t!12_16!!q13_p11.1!
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k46_XY_t!1_3!!p10_q10!
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k46_XY_t!1_3!!p10_p10!
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k46_XY_t!2_5!!q21_q31!
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k46_XY_t!2_5!!p12_q31!
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k46_XY_t!5_6!!q13q23_q15q23!
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k46_XY_t!X_15_18!!p11.1_p11.1_q11.1!
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k46_Y_t!X_8!!p22.3_q24.1!
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k47_X_t!X_13!!q27_q12!_inv!10!!p13q22!_+21
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k47_Y_t!X_13!!q27_q12!_inv!10!!p13q22!_+21
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k92_XXXX_t!8_14!!q24.1_q32!x2
                      n/StructuralAbnormalKaryotypeTranslocation))
  (is (r/isuperclass? i/k92_XXYY_del!7!!p11.2!_t!7_14!!p15_q11.1!
                      n/StructuralAbnormalKaryotypeTranslocation))

  ;; TOFIX
  ;; Yq11.2 doesnt exist!
  ;; (is (r/isuperclass? i/k46_t!X_Y!!q22_q11.2!
  ;;                     n/StructuralAbnormalKaryotypeTranslocation))
  ;; Yq11.2 doesnt exist!
  ;; (is (r/isuperclass? i/k46_t!X_18!!p11.2_q11.2!_t!Y_1!!q11.2_p31!
  ;;                     n/StructuralAbnormalKaryotypeTranslocation))
  ;; * not implemented
  ;; (is (r/isuperclass? i/k46_XX_t!3_7_7*!!q21_q22_p13!
  ;;                     n/StructuralAbnormalKaryotypeTranslocation))
  ;; * not implemented
  ;; (is (r/isuperclass? i/k46_XX_t!3_9_9*_22!!p13_q22_q34_q11.2!
  ;;                     n/StructuralAbnormalKaryotypeTranslocation))
  )

(deftest StructuralAbnormalKaryotypeTriplication
  (is (r/isuperclass? i/k46_XX_invtrp!1!!q32q21!
                      n/StructuralAbnormalKaryotypeTriplication))
  (is (r/isuperclass? i/k46_XX_trp!1!!q21q32!
                      n/StructuralAbnormalKaryotypeTriplication))
  )

;; Named Karyotypes - Allosomal Abnormalities
;; TOFIX
(deftest TurnerSyndrome
  ;; (is (r/isuperclass? i/k45_X n/TurnerSyndrome))
  )

(deftest KlinefelterSyndromeMostCommonVariation
  ;; TOFIX - should be TRUE
  ;; (is (r/isuperclass? i/k47_XXY n/KlinefelterSyndromeMostCommonVariation))

  (is (not
       (r/isuperclass? i/k48_XXYY n/KlinefelterSyndromeMostCommonVariation)))
  (is (not
       (r/isuperclass? i/k48_XXYc_+X n/KlinefelterSyndromeMostCommonVariation)))
  (is
   (not
    (r/isuperclass? i/k48_XY_+X_+Y n/KlinefelterSyndromeMostCommonVariation)))
  (is (not
       (r/isuperclass? i/k49_XXYYY n/KlinefelterSyndromeMostCommonVariation)))
  )

(deftest KlinefelterSyndromeAllVariations
  ;; (is (r/isuperclass? i/k48_XXXY n/KlinefelterSyndromeAllVariations))
  ;; (is (r/isuperclass? i/k49_XXXXY n/KlinefelterSyndromeAllVariations))
  ;; (is (r/isuperclass? i/k49_XXXYY n/KlinefelterSyndromeAllVariations))
  ;; (is (r/isuperclass? i/k48_XXYY n/KlinefelterSyndromeAllVariations))
  (is (r/isuperclass? i/k48_XXYc_+X n/KlinefelterSyndromeAllVariations))
  (is (r/isuperclass? i/k48_XY_+X_+Y n/KlinefelterSyndromeAllVariations))
  ;; (is (r/isuperclass? i/k49_XXYYY n/KlinefelterSyndromeAllVariations))
  )

;; NONE DEFINED
(deftest Lethal
  )

;; TOFIX
;; Named Karyotypes - Autsomal Abnormalities
(deftest Warkany2Syndrome
  (is (r/isuperclass? i/k46_XX_+8_-21 n/Warkany2Syndrome))

  ;; (is (r/isuperclass?
  ;;      i/k48_X_t!Y_12!!q11.2_p12!_del!6!!q11!_+8_t!9_22!!q34_q11.2!_+17_-21_+22
  ;;      n/Warkany2Syndrome))
  ;; (is (r/isuperclass?
  ;;      i/k50_XX_+1_del!1!!p13!_+dup!1!!q21q32!_+inv!1!!p31q41!_+8_r!10!!p12q25!_-21
  ;;      n/Warkany2Syndrome))
)

;; NONE DEFINED
(deftest Trisomy9
  )

(deftest PatauSyndrome
  (is (r/isuperclass? i/k48_XX_+13_+21 n/PatauSyndrome))
  )

;; NONE DEFINED
(deftest Trisomy16
  )

;; NONE DEFINED
(deftest EdwardsSyndrome
  )

(deftest DownSyndrome
  (is (r/isuperclass?
       i/k49_X_inv!X!!p21q26!_+3_inv!3!!q21q26.2!_+7_+10_-20_del!20!!q11.2!_+21
       n/DownSyndrome))
  (is (r/isuperclass? i/k46_Xc_+21 n/DownSyndrome))
  (is
   (r/isuperclass? i/k47_X_t!X_13!!q27_q12!_inv!10!!p13q22!_+21 n/DownSyndrome))
  (is (r/isuperclass? i/k47_XX_+21 n/DownSyndrome))
  (is
   (r/isuperclass? i/k47_Y_t!X_13!!q27_q12!_inv!10!!p13q22!_+21 n/DownSyndrome))
  (is (r/isuperclass? i/k48_XX_+13_+21 n/DownSyndrome))

  ;; TODO - NOT TRUE - THE DEFINITION FOR DOWN SYNDROMES UPDATING
  ;; (is (not (r/isuperclass? i/k48_XY_+21c_+21 n/DownSyndrome)))
  )

;; TOFIX
(deftest Trisomy22
  ;; (is (r/isuperclass? i/k48_X_t!Y_12!!q11.2_p12!_del!6!!q11!_+8_t!9_22!!q34_q11.2!_+17_-21_+22 n/Trisomy22))
  )

;; Named Karyotypes - Structural Abnormalities

;; TODO
(deftest DeletionSyndrome1p36
  )

;; TODO
(deftest WolfHirschhornSyndrome
  )

;; TODO
(deftest CriDuChat
  )

;; TODO
(deftest DeletionSyndrome5q
  )

;; TODO
(deftest WilliamsSyndrome
  )

;; TODO
(deftest JacobsenSyndrome
  )

;; TODO
(deftest AngelmanSyndrome
  )

;; TODO
(deftest PraderWilliSyndrome
  )

;; TODO
(deftest MillerDiekerSyndrome
  )

;; TODO
(deftest SmithMagenisSyndrome
  )

;; TODO
(deftest DeletionSyndrome18q
  )

;; TODO
(deftest DiGeorgeSyndrome
  )

;; TODO
(deftest CatEyeSyndrome
  )

;; TODO Probe classes
;; (is (not
;;      (with-probe-class
;;        [joined (owlclass :subclass i/k46_XY i/k45_Y)]
;;        (r/consistent? joined))))
