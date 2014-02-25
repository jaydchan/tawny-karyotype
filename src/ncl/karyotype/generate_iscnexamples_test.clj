;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2014, Newcastle University

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

(ns ^{:doc "Testing example karyotypes from the ISCN2013."
      :author "Jennifer Warrender"}
  ncl.karyotype.generate_iscnexamples_test
  (:use (incanter core io excel))
  (:require [ncl.karyotype [named :as n]]
            [clojure.java.io :as io]))

(defn output [output-file string append error]
  "APPENDs STRING to OUTPUT-FILE unless there is an ERROR"
  (try
    (spit output-file string :append append)
    (catch
        Exception exp (println error exp))))

;; MAIN
;; Clean file
(def output-file "./test/ncl/karyotype/iscnexamplesB_test.clj")
(output output-file "" false "Error with new file.")

;; Read data from .xlsx file
(with-data (read-xls
            (.getFile (io/resource "iscnexamples_test.xlsx")))

  ;; view data in popup table
  ;; (view $data)

  (let [names (into [] ($ :Name))
        tests {:male ["MaleKaryotype"
                      (into [] ($ :Male))]
               :female ["FemaleKaryotype"
                        (into [] ($ :Female))]}]

    (doseq [test (vals tests)]
      (output
       output-file
       (str "(deftest " (first test) "\n"
            (clojure.string/join
             "\n"
             (for [i (range (count names))]
               (let [instance (get (second test) i)
                     name (get names i)]
                 (cond
                  (= instance 1.0)
                  (str "(is (r/isuperclass? i/" name " n/" (first test)"))")
                  (= instance -1.0)
                  (str
                   "(is (not (r/isuperclass? i/" name " n/" (first test)")))")))))
            "\n)")
       true
       (str "Error with " (first test) " testing.")))))