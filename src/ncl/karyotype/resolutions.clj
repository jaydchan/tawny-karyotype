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

(ns ncl.karyotype.resolutions
  (:use [tawny.owl])
  (:require [ncl.karyotype [human :as h]]
            [clojure.java.io :as io]))

(defontology resolutions
  :iri "http://ncl.ac.uk/karyotype/resolutions"
  :prefix "res:")

;; import all ncl.karyotype axioms
(owl-import h/human)

(defclass Resolution)

(as-disjoint-subclasses
 Resolution
 (defclass r300-band)
 (defclass r400-band)
 (defclass r550-band)
 (defclass r700-band)
 (defclass r850-band))

;; TODO - define object properties
(defoproperty seenAtResolution)

;; function to read a file
(defn get-lines [file-name]
  (with-open [r (io/reader file-name)]
    (doall (line-seq r))))

;; reads in data
(def string-results (get-lines "resolutions.txt"))
(def results
  (for [r string-results]
    (read-string r)))

(defn get-band [string]
  (let [string-band
        (str "HumanChromosome"
             (re-find #"\d+|X|Y" string)
             "Band"
             (clojure.string/replace-first
              string #"\d+|X|Y" ""))]
    (owl-class h/human string-band)))

(defn get-resolution [value]
  (let [string-resolution
        (str "r" value "-band")]
    (owl-class string-resolution)))

(defn resolution [band & resolutions]
  (refine (owl-class h/human band)
          :subclass
          (some-only seenAtResolution resolutions)))

(doseq [r results]
  (resolution (get-band (first r))
              (for [res (rest r)]
                (get-resolution res))))

;; missing 300-bands
;; 4p12 & 4p13 -> 4p12p13p14
;; Xq12 -> Xq12q13

;; ACTUAL sixe of 300-bands is 307

;; CHECK 400,550,700,850