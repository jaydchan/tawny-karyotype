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

(ns ncl.karyotype.randomkaryotype
  (:use [tawny.owl])
  (:require [tawny [reasoner :as r]]
            [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]
            [ncl.karyotype [events :as e]]
            [ncl.karyotype [features :as f]]
            [ncl.karyotype [named :as n]]))

(defontology randomkaryotype
  :iri "http://ncl.ac.uk/karyotype/randomkaryotype"
  :prefix "rkar:")

;; import all ncl.karyotype axioms
(owl-import k/karyotype)
(owl-import h/human)
(owl-import e/events)
(owl-import f/features)
(owl-import n/named)

(defclass RandomKaryotype
  :subclass k/Karyotype)

(defn karyotype-class [the-name & frames]
  (apply owl-class
         (list* (str "r" the-name)
                :label (str "The" the-name "Karyotype")
                :subclass RandomKaryotype
                frames)))

(defn get-bands [vector]
)

(def bands-300 [1
                ["p36.3" "p36.3" "p36.2" "p36.1" "p35" "p34" "p33" "p32" "p31" "p22" "p21" "p13" "p12" "p11" "q11" "q12" "q21" "q22q23q24" "q25" "q31" "q32" "q41" "q42" "q43q44"]])


(def chromosomes
  (let [types (direct-subclasses h/human h/HumanChromosome)]
    (into [] (apply clojure.set/union
           (for [type types]
             (into #{} (direct-subclasses h/human type)))))))

(defn random-chromosome []
  (let [r (rand-int (count chromosomes))]
    (get chromosomes r)))

(defn random-deletion []
  (e/deletion 1 (random-chromosome)))

(defn random-addition []
  (e/addition 1 (random-chromosome)))

(def abnormalities [random-deletion random-addition])
(defn random-abnormality []
  (let [r (rand-int (count abnormalities))]
    ((get abnormalities r))))

;; TODO check that they are distinct/unique
(defn random-abnormality-driver [max]
  (let [n (rand-int max)]
    (for [i (range n)]
      (random-abnormality))))

(defn random-karyotype [name max]
  (karyotype-class name
                   :subclass (random-abnormality-driver max)))

(defn random-karyotype-driver [number max]
  (doseq [i (range number)]
    (random-karyotype i max)))

"Number of random karyotypes"
(def n 1000000)
"Max number of abnormalities"
(def m 3)

(random-karyotype-driver n m)

;; TODO
;; paper - left/right http://www.cs.man.ac.uk/~mikroyae/#Publications
;; graphing - incanter https://github.com/liebke/incanter
;; need to create driver for fission
;; seperate drivers from patterns in event and features
