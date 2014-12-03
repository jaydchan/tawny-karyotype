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
;; along with this program. If not, see http://www.gnu.org/licenses/.

(ns ^{:doc "Translating resolution information for human chromosomal
bands."
      :author "Jennifer Warrender"}
  ncl.karyotype.resolutions
  (:use [tawny.owl]
        [ncl.karyotype.generic :only [get-lines]])
  (:require [ncl.karyotype
             [karyotype :as k]
             [human :as h]]
            [clojure.java.io :as io]))

(defontology resolutions
  :iri "http://www.purl.org/captau/karyotype/resolutions"
  :prefix "res:"
  :comment "Resolution ontology for Human Karyotype Ontology,
  written using the tawny-owl library")

(defclass Resolution)

(as-disjoint-subclasses
 Resolution
 (defclass r300-band)
 (defclass r400-band)
 (defclass r550-band)
 (defclass r700-band)
 (defclass r850-band))

;; TODO - define object properties
(defoproperty seenAtResolution
  :domain h/HumanChromosomeBand
  :range Resolution)

;; Auxiliary functions
(defn get-band
  "Returns (finds) human chromosome band."
  [string] {:post [(h/band? %)]}
  (let [string-band
        (str "HumanChromosome"
             (re-find #"\d+|X|Y" string)
             "Band"
             (clojure.string/replace-first
              string #"\d+|X|Y" ""))]
    (owl-class h/human string-band)))

(defn get-resolution
  "Returns (finds) resolution class."
  [value] {:post [(subclass? resolutions Resolution %)]}
  (let [string-resolution
        (str "r" value "-band")]
    (owl-class resolutions string-resolution)))

(defn resolution
  "Resolution pattern - redefines human band class with additional
resolution information."
  [band & resolutions]
  (refine (owl-class h/human band)
          :subclass
          (some-only seenAtResolution resolutions)))

;; MAIN
(def ^{:doc "The resolution information read in from resources text
  file as java.lang.Cons."}
  string-results (get-lines (.getFile (io/resource "resolutions.txt"))))

(def ^{:doc "The resolution information read in from resources text
  file as LazySeq."}
  results (for [r string-results]
            (read-string r)))

;; creates resolution restrictions using resolution pattern
(doseq [r results]
  (resolution (get-band (first r))
              (for [res (rest r)]
                (get-resolution res))))

;; equivalency classes - useful for tests
(defclass is-300-band
  :equivalent
  (owl-and h/HumanChromosomeBand
             (owl-some seenAtResolution r300-band)))

(defclass is-400-band
  :equivalent
  (owl-and h/HumanChromosomeBand
             (owl-some seenAtResolution r400-band)))

(defclass is-550-band
  :equivalent
  (owl-and h/HumanChromosomeBand
             (owl-some seenAtResolution r550-band)))

(defclass is-700-band
  :equivalent
  (owl-and h/HumanChromosomeBand
             (owl-some seenAtResolution r700-band)))

(defclass is-850-band
  :equivalent
  (owl-and h/HumanChromosomeBand
             (owl-some seenAtResolution r850-band)))

;; TODO Should be somewhere else? - owl-import not required if placed correctly
(owl-import h/human)
(defclass centromere-and-telomere
  :equivalent
  (owl-and h/HumanChromosomeBand
           (owl-or
            (owl-some k/isBandOf h/HumanCentromere)
            (owl-some k/isBandOf h/HumanTelomere))))