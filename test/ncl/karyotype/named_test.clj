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

(ns ncl.karyotype.named_test
  (:use [clojure.test])
  (:require
   [ncl.karyotype.named :as n]
   [tawny.owl :as o]
   [tawny.reasoner :as r]))

(defn ontology-reasoner-fixture [tests]
  (r/reasoner-factory :hermit)
  (o/ontology-to-namespace n/named)
  (binding [r/*reasoner-progress-monitor*
            r/reasoner-progress-monitor-silent]
    (tests)))

(use-fixtures :once ontology-reasoner-fixture)

;; to run: M-x 'lein' 'test'

(deftest Basic
  (is (r/consistent?))
  (is (r/coherent?)))

;; Ploidy Level
(deftest HaploidKaryotype
  (is (r/isuperclass? n/k23_N n/HaploidKaryotype))
  (is (r/isuperclass? n/k23_X n/HaploidKaryotype))
  (is (r/isuperclass? n/k23_Y n/HaploidKaryotype))

  (is (not (r/isuperclass? n/k46_XN n/HaploidKaryotype)))
  ;; TOFIX (is (not (r/isuperclass? n/k69_NNN n/HaploidKaryotype)))
  ;; TOFIX (is (not (r/isuperclass? n/k92_NNNN n/HaploidKaryotype)))
  )

(deftest DiploidKaryotype
  (is (r/isuperclass? n/k46_XN n/DiploidKaryotype))
  (is (r/isuperclass? n/k46_XX n/DiploidKaryotype))
  (is (r/isuperclass? n/k46_XY n/DiploidKaryotype))

  (is (not (r/isuperclass? n/k23_N n/DiploidKaryotype)))
  ;; TOFIX (is (not (r/isuperclass? n/k69_NNN n/DiploidKaryotype)))
  ;; TOFIX (is (not (r/isuperclass? n/k92_NNNN n/DiploidKaryotype)))  
  )

;; TODO Complete when base karyotypes for triploid and tetraploid are implemented
;; (deftest TriploidKaryotype
;;   (is (r/isuperclass? n/k69_NNN n/TriploidKaryotype))

;;   (is (not (r/isuperclass? n/k23_N n/TriploidKaryotype)))
;;   (is (not (r/isuperclass? n/k46_XN n/TriploidKaryotype)))
;;   (is (not (r/isuperclass? n/k92_NNNN n/TriploidKaryotype)))    
;;   )

;; (deftest TetraploidKaryotype
;;   (is (r/isuperclass? n/k69_NNN n/TetraploidKaryotype))

;;   (is (not (r/isuperclass? n/k23_N n/TetraploidKaryotype)))
;;   (is (not (r/isuperclass? n/k46_XN n/TetraploidKaryotype)))
;;   (is (not (r/isuperclass? n/k69_NNN n/TetraploidKaryotype)))    
;;   )

;; Gender
;; TODO Extend definition to include haploid, triploid and tetraploid?
(deftest FemaleKaryotype
  (is (r/isuperclass? n/k46_XX n/FemaleKaryotype))

  (is (not (r/isuperclass? n/k46_XN n/FemaleKaryotype)))
  (is (not (r/isuperclass? n/k46_XY n/FemaleKaryotype)))
  (is (not (r/isuperclass? n/KlinefelterSyndromeAllVariations n/FemaleKaryotype)))
  )

(deftest MaleKaryotype
  (is (r/isuperclass? n/k46_XY n/MaleKaryotype))
  (is (r/isuperclass? n/KlinefelterSyndromeAllVariations n/MaleKaryotype))

  (is (not (r/isuperclass? n/k46_XN n/MaleKaryotype)))  
  (is (not (r/isuperclass? n/k46_XX n/MaleKaryotype)))
  )

;; Numerical Abnormalities
(deftest NumericalAbnormalKaryotype
  (is (r/isuperclass? n/NumericalAbnormalKaryotypeAllosomalGainOrLoss n/NumericalAbnormalKaryotype))  
  (is (r/isuperclass? n/NumericalAbnormalKaryotypeAutosomalGainOrLoss n/NumericalAbnormalKaryotype))  
  (is (r/isuperclass? n/NumericalAbnormalKaryotypeChromosomalGain n/NumericalAbnormalKaryotype))
  (is (r/isuperclass? n/NumericalAbnormalKaryotypeChromosomalLoss n/NumericalAbnormalKaryotype))

  (is (not (r/isuperclass? n/k23_N n/NumericalAbnormalKaryotype)))
  (is (not (r/isuperclass? n/k46_XN n/NumericalAbnormalKaryotype)))

  ;; ;; TODO Triploidy and tetraploidy base cases when implemented
  ;; (is (not (r/isuperclass? n/k69_NNN n/NumericalAbnormalKaryotype)))
  ;; (is (not (r/isuperclass? n/k92_NNN n/NumericalAbnormalKaryotype)))
  )

(deftest NumericalAbnormalKaryotypeAllosomalGainOrLoss
  (is (r/isuperclass? n/NumericalAbnormalKaryotypeAllosomalGain n/NumericalAbnormalKaryotypeAllosomalGainOrLoss))
  (is (r/isuperclass? n/NumericalAbnormalKaryotypeAllosomalLoss n/NumericalAbnormalKaryotypeAllosomalGainOrLoss))
`
  (is (not (r/isuperclass? n/k46_XN n/NumericalAbnormalKaryotypeAllosomalGainOrLoss)))  
  )

(deftest NumericalAbnormalKaryotypeAllosomalGain
  (is (r/isuperclass? n/KlinefelterSyndromeAllVariations n/NumericalAbnormalKaryotypeAllosomalGain))

  (is (not (r/isuperclass? n/k46_XN n/NumericalAbnormalKaryotypeAllosomalGain)))  
  )

(deftest KlinefelterSyndromeAllVariations
  (is (r/isuperclass? n/KlinefelterSyndromeMostCommonVariation n/KlinefelterSyndromeAllVariations))

  (is (not (r/isuperclass? n/k46_XN n/KlinefelterSyndromeAllVariations)))  
  )

(deftest NumericalAbnormalKaryotypeAllosomalLoss
  (is (r/isuperclass? n/TurnerSyndrome n/NumericalAbnormalKaryotypeAllosomalLoss))

  (is (not (r/isuperclass? n/k46_XN n/NumericalAbnormalKaryotypeAllosomalLoss)))
  )

(deftest NumericalAbnormalKaryotypeAutosomalGainOrLoss
  (is (r/isuperclass? n/NumericalAbnormalKaryotypeAutosomalGain n/NumericalAbnormalKaryotypeAutosomalGainOrLoss))
  (is (r/isuperclass? n/NumericalAbnormalKaryotypeAutosomalGain n/NumericalAbnormalKaryotypeAutosomalGainOrLoss))

  (is (not (r/isuperclass? n/k46_XN n/NumericalAbnormalKaryotypeAutosomalGainOrLoss)))
  )

(deftest NumericalAbnormalKaryotypeAutosomalGain
  (is (r/isuperclass? n/DownSyndrome n/NumericalAbnormalKaryotypeAutosomalGain))
  (is (r/isuperclass? n/EdwardsSyndrome n/NumericalAbnormalKaryotypeAutosomalGain))
  (is (r/isuperclass? n/PatauSyndrome n/NumericalAbnormalKaryotypeAutosomalGain))
  (is (r/isuperclass? n/Trisomy16 n/NumericalAbnormalKaryotypeAutosomalGain))
  (is (r/isuperclass? n/Trisomy22 n/NumericalAbnormalKaryotypeAutosomalGain))
  (is (r/isuperclass? n/Trisomy9 n/NumericalAbnormalKaryotypeAutosomalGain))
  (is (r/isuperclass? n/Warkany2Syndrome n/NumericalAbnormalKaryotypeAutosomalGain))

  (is (not (r/isuperclass? n/k46_XN n/NumericalAbnormalKaryotypeAutosomalGain)))
  )

(deftest NumericalAbnormalKaryotypeChromosomalGain
  (is (r/isuperclass? n/NumericalAbnormalKaryotypeAllosomalGain n/NumericalAbnormalKaryotypeChromosomalGain))
  (is (r/isuperclass? n/NumericalAbnormalKaryotypeAutosomalGain n/NumericalAbnormalKaryotypeChromosomalGain))
  
  (is (not (r/isuperclass? n/k46_XN n/NumericalAbnormalKaryotypeChromosomalGain)))
  )

(deftest NumericalAbnormalKaryotypeChromosomalLoss
  (is (r/isuperclass? n/NumericalAbnormalKaryotypeAllosomalLoss n/NumericalAbnormalKaryotypeChromosomalLoss))
  (is (r/isuperclass? n/NumericalAbnormalKaryotypeAutosomalLoss n/NumericalAbnormalKaryotypeChromosomalLoss))
  
  (is (not (r/isuperclass? n/k46_XN n/NumericalAbnormalKaryotypeChromosomalLoss)))
  )

;; Structural Abnormalities
(deftest StrutcuralAbnormalKaryotype
  (is (r/isuperclass? n/StructuralAbnormalKaryotypeInsertion n/StructuralAbnormalKaryotype))  
  (is (r/isuperclass? n/StructuralAbnormalKaryotypeInversion n/StructuralAbnormalKaryotype))
  (is (r/isuperclass? n/StructuralAbnormalKaryotypeAddition n/StructuralAbnormalKaryotype))
  (is (r/isuperclass? n/StructuralAbnormalKaryotypeDeletion n/StructuralAbnormalKaryotype))
  (is (r/isuperclass? n/StructuralAbnormalKaryotypeDuplication n/StructuralAbnormalKaryotype))
  (is (r/isuperclass? n/StructuralAbnormalKaryotypeFission n/StructuralAbnormalKaryotype))
  (is (r/isuperclass? n/StructuralAbnormalKaryotypeQuadruplication n/StructuralAbnormalKaryotype))
  (is (r/isuperclass? n/StructuralAbnormalKaryotypeTranslocation n/StructuralAbnormalKaryotype))
  (is (r/isuperclass? n/StructuralAbnormalKaryotypeTriplication n/StructuralAbnormalKaryotype))

  (is (not (r/isuperclass? n/k23_N n/StructuralAbnormalKaryotype)))
  (is (not (r/isuperclass? n/k46_XN n/StructuralAbnormalKaryotype)))

  ;; ;; TODO Triploidy and tetraploidy base cases when implemented
  ;; (is (not (r/isuperclass? n/k69_NNN n/NumericalAbnormalKaryotype)))
  ;; (is (not (r/isuperclass? n/k92_NNN n/NumericalAbnormalKaryotype)))
  )

(deftest StructuralAbnormalKaryotypeAddition
  (is (not (r/isuperclass? n/k46_XN n/StructuralAbnormalKaryotypeAddition)))
  )

(deftest StructuralAbnormalKaryotypeDeletion
  (is (not (r/isuperclass? n/k46_XN n/StructuralAbnormalKaryotypeDeletion)))
  )

(deftest StructuralAbnormalKaryotypeDuplication
  (is (not (r/isuperclass? n/k46_XN n/StructuralAbnormalKaryotypeDuplication)))
  )

(deftest StructuralAbnormalKaryotypeFission
  (is (not (r/isuperclass? n/k46_XN n/StructuralAbnormalKaryotypeFission)))
  )

(deftest StructuralAbnormalKaryotypeInsertion
  (is (not (r/isuperclass? n/k46_XN n/StructuralAbnormalKaryotypeInsertion)))
  )

(deftest StructuralAbnormalKaryotypeInversion
  (is (not (r/isuperclass? n/k46_XN n/StructuralAbnormalKaryotypeInversion)))
  )

(deftest StructuralAbnormalKaryotypeQuadruplication
  (is (not (r/isuperclass? n/k46_XN n/StructuralAbnormalKaryotypeQuadruplication)))
  )

(deftest StructuralAbnormalKaryotypeTranslocation
  (is (not (r/isuperclass? n/k46_XN n/StructuralAbnormalKaryotypeTranslocation)))
  )

(deftest StructuralAbnormalKaryotypeTriplication
  (is (not (r/isuperclass? n/k46_XN n/StructuralAbnormalKaryotypeTriplication)))
  )

;; Other
(deftest KlinefelterSyndromeAllVariations
  (is (r/isuperclass? n/KlinefelterSyndromeMostCommonVariation n/KlinefelterSyndromeAllVariations))

  (is (not (r/isuperclass? n/k46_XN n/NumericalAbnormalKaryotype)))
  )