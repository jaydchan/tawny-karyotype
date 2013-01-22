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

(deftest FemaleKaryotype
  (is (r/isuperclass? n/k46_XX n/FemaleKaryotype))

  
  (is (not (r/isuperclass? n/k46_XY n/FemaleKaryotype)))
  (is (not (r/isuperclass? n/KlinefelterSyndromeAllVariations n/FemaleKaryotype)))
  (is (not (r/isuperclass? n/KlinefelterSyndromeMostCommonVariation n/FemaleKaryotype)))
  )

(deftest MaleKaryotype
  (is (r/isuperclass? n/k46_XY n/MaleKaryotype))
  (is (r/isuperclass? n/KlinefelterSyndromeAllVariations n/MaleKaryotype))
  (is (r/isuperclass? n/KlinefelterSyndromeMostCommonVariation n/MaleKaryotype))

  
  (is (not (r/isuperclass? n/k46_XX n/MaleKaryotype)))
  )

(deftest KlinefelterSyndromeAllVariations
  (is (r/isuperclass? n/KlinefelterSyndromeMostCommonVariation n/KlinefelterSyndromeAllVariations))

  
  )

(deftest NumericalAbnormalKaryotype
  (is (r/isuperclass? n/NumericalAbnormalKaryotypeChromosomalGain n/NumericalAbnormalKaryotype))
  (is (r/isuperclass? n/NumericalAbnormalKaryotypeChromosomalLoss n/NumericalAbnormalKaryotype))
  (is (r/isuperclass? n/NumericalAbnormalKaryotypeAllosomalGainOrLoss n/NumericalAbnormalKaryotype))  
  (is (r/isuperclass? n/NumericalAbnormalKaryotypeAutosomalGainOrLoss n/NumericalAbnormalKaryotype))

  
  (is (not (r/isuperclass? n/k46_XX n/NumericalAbnormalKaryotype)))
  )

(deftest NumericalAbnormalKaryotypeChromosomalGain

  
  (is (not (r/isuperclass? n/k46_XX n/NumericalAbnormalKaryotypeChromosomalGain)))
  )

(deftest NumericalAbnormalKaryotypeChromosomalLoss

  
  (is (not (r/isuperclass? n/k46_XX n/NumericalAbnormalKaryotypeChromosomalLoss)))
  )

(deftest NumericalAbnormalKaryotypeAllosomalGain
  (is (r/isuperclass? n/KlinefelterSyndromeAllVariations n/NumericalAbnormalKaryotypeAllosomalGain))

  
  )

(deftest NumericalAbnormalKaryotypeAutosomalGain
  (is (r/isuperclass? n/DownSyndrome n/NumericalAbnormalKaryotypeAutosomalGain))
  (is (r/isuperclass? n/EdwardsSyndrome n/NumericalAbnormalKaryotypeAutosomalGain))
  (is (r/isuperclass? n/PatauSyndrome n/NumericalAbnormalKaryotypeAutosomalGain))
  (is (r/isuperclass? n/Trisomy16 n/NumericalAbnormalKaryotypeAutosomalGain))
  (is (r/isuperclass? n/Trisomy22 n/NumericalAbnormalKaryotypeAutosomalGain))
  (is (r/isuperclass? n/Trisomy9 n/NumericalAbnormalKaryotypeAutosomalGain))
  (is (r/isuperclass? n/WarkanySyndrome n/NumericalAbnormalKaryotypeAutosomalGain))

  
  )

(deftest NumericalAbnormalKaryotypeAllosomalLoss
  (is (r/isuperclass? n/TurnerSyndrome n/NumericalAbnormalKaryotypeAllosomalLoss))

  
  )

(deftest StrutcuralAbnormalKaryotype
  (is (r/isuperclass? n/StructuralAbnormalKaryotypeInsertion n/StructuralAbnormalKaryotype))  
  (is (r/isuperclass? n/StructuralAbnormalKaryotypeInversion n/StructuralAbnormalKaryotype))


  (is (not (r/isuperclass? n/k46_XX n/StructuralAbnormalKaryotype)))
  )

(deftest StructuralAbnormalKaryotypeInsertion

  
  (is (not (r/isuperclass? n/k46_XX n/StructuralAbnormalKaryotypeInsertion)))
  )

(deftest StructuralAbnormalKaryotypeInversion

  
  (is (not (r/isuperclass? n/k46_XX n/StructuralAbnormalKaryotypeInversion)))
  )



