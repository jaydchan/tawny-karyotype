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

(ns ^{:doc "Generating tests for example karyotypes from the
ISCN2013."
      :author "Jennifer Warrender"}
  ncl.karyotype.generate_iscnexamples_test
  (:use (incanter core io excel)
        [tawny.owl :exclude [save-ontology]]
        [ncl.karyotype.generic])
  (:require [tawny.render :as r]
            [ncl.karyotype [iscnexamples :as i]]
            [clojure.java.io :as io]))

(defn test-string
  "TODO"
  [name parent bool]
  (str "(is "
       (if (false? bool)
         "(not ")
       "(r/isuperclass? i/" name " n/" parent"))"
       (if (false? bool)
         ")")))

;; If tests does not exist or bypass set to false
(defn generate-iscn-tests
  "TODO"
  [output-file bypass]
  (if (or (false? bypass) (not (.exists (io/as-file output-file))))

    ;; Read data from .xlsx file
    (with-data (read-xls
                (.getFile (io/resource "iscnexamples_test.xlsx")))

      ;; Clean file
      (output output-file "" false "Error with new file.")

      ;; view data in popup table
      ;; (view $data)

      ;; Check all defined ISCNExamplesKaryotype are in spreadsheet and
      ;; clojure file
      (let [clojure_file
            (into #{}
                  (map
                   #(get-entity-short-string %)
                   (direct-subclasses i/iscnexamples i/ISCNExampleKaryotype)))
            spreadsheet_data (into #{} ($ :Name))
            missing_clojure (clojure.set/difference spreadsheet_data clojure_file)
            missing_spreadsheet (clojure.set/difference
                                 clojure_file spreadsheet_data)]

        (if (> (count missing_clojure) 0)
          (println (str "Missing the following examples from clojure file:\n"
                        (clojure.string/join "\n" missing_clojure))))
        (if (> (count missing_spreadsheet) 0)
          (println (str "Missing the following examples from spreadsheet:\n"
                        (clojure.string/join "\n" missing_spreadsheet)))))

      ;; TODO Still looks U--GLY
      ;; Generate tests for iscnexamples
      (let [names (into [] ($ :Name))
            tests {:Male "MaleKaryotype" :Female "FemaleKaryotype"
                   :Haploid "HaploidKaryotype" :Diploid "DiploidKaryotype"
                   :Triploid "TriploidKaryotype" :Tetraploid "TetraploidKaryotype"
                   ;; :AllosomalGain "NumericalAbnormalKaryotypeAllosomalGain"
                   ;; :AllosomalLoss "NumericalAbnormalKaryotypeAllosomalLoss"
                   ;; :AutosomalGain "NumericalAbnormalKaryotypeAutosomalGain"
                   ;; :AutosomalLoss "NumericalAbnormalKaryotypeAutosomalGain"
                   ;; :Turner "TurnerSyndrome"
                   :Addition "StructuralAbnormalKaryotypeAddition"
                   :Deletion "StructuralAbnormalKaryotypeDeletion"
                   :Duplication "StructuralAbnormalKaryotypeDuplication"
                   :Fission "StructuralAbnormalKaryotypeFission"
                   :Insertion "StructuralAbnormalKaryotypeInsertion"
                   :Inversion "StructuralAbnormalKaryotypeInversion"
                   :Quadruplication "StructuralAbnormalKaryotypeQuadruplication"
                   :Translocation "StructuralAbnormalKaryotypeTranslocation"
                   :Triplication "StructuralAbnormalKaryotypeTriplication"
                   }]

        (doseq [test (keys tests)]
          (let [parent (get tests test)
                vector (into [] ($ test))]
            (output
             output-file
             (str "(deftest " parent "\n"
                  (clojure.string/join
                   "\n"
                   (for [i (range (count names))]
                     (let [instance (get vector i)
                           name (get names i)]
                       (cond
                        (= instance 1.0)
                        (test-string name parent true)
                        (= instance -1.0)
                        (test-string name parent false)))))
                  "\n)")
             true
             (str "Error with " parent " testing."))))))))
