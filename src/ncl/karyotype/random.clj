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

(ns ncl.karyotype.random
  (:use [tawny.owl])
  (:require [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]
            [ncl.karyotype [events :as e]]
            [ncl.karyotype [features :as f]]
            [ncl.karyotype [base :as b]]
            [ncl.karyotype [parse :as p]]))

(defontology random
  :iri "http://ncl.ac.uk/karyotype/random"
  :prefix "rkar:")

;; import all ncl.karyotype axioms
(owl-import k/karyotype)
(owl-import h/human)
(owl-import e/events)
(owl-import f/features)
(owl-import b/base)

(defclass RandomKaryotype
  :subclass k/Karyotype)

(def sex (into [] (direct-subclasses b/base b/k46_XN)))
(defn random-sex []
  (let [r (rand-int (count sex))]
    (get sex r)))

;; TODO does not include constitutional karyotypes
(defn karyotype-class [name & frames]
  (apply owl-class
         (list* (str "r" name)
                :subclass RandomKaryotype
                (owl-some b/derivedFrom (random-sex))
                frames)))

;; missing chromo 2-22,X,Y bands
(def bands-300 [1
                ["p36.3" "p36.3" "p36.2" "p36.1" "p35" "p34" "p33" "p32" "p31" "p22" "p21" "p13" "p12" "p11" "q11" "q12" "q21" "q22q23q24" "q25" "q31" "q32" "q41" "q42" "q43q44"]])

(defn get-band [chromosome band]
  (owl-class h/human (str "HumanChromosome" chromosome "Band" band)))

(defn random-band []
  (let [bands (second bands-300)
        r (rand-int (count bands))]
    (get-band (first bands-300) (get bands r))))

(def chromosomes
  (let [types (direct-subclasses h/human h/HumanChromosome)]
    (into [] (apply clojure.set/union
           (for [type types]
             (into #{} (direct-subclasses h/human type)))))))

(defn random-chromosome []
  (let [r (rand-int (count chromosomes))]
    (get chromosomes r)))

(defn random-terminal-deletion []
  (e/deletion 1 (random-band)))

(defn random-interstitial-deletion []
  (e/deletion 1 (random-band) (random-band)))

(def deletions [random-terminal-deletion random-interstitial-deletion])
(defn random-band-deletion-driver []
  (let [r (rand-int (count deletions))]
    ((get deletions r))))

(defn random-chromosome-deletion []
  (e/deletion 1 (random-chromosome)))

(defn random-band-addition []
  (e/addition 1 (random-band)))

(defn random-chromosome-addition []
  (e/addition 1 (random-chromosome)))

(def abnormalities [random-chromosome-deletion
  random-band-deletion-driver random-chromosome-addition
  random-band-addition])
(defn random-abnormality []
  (let [r (rand-int (count abnormalities))]
    ((get abnormalities r))))

;; TODO check that they are only two at most of the same abnormality
(defn random-abnormality-driver [max]
  (let [n (rand-int max)]
    (for [i (range n)]
      (random-abnormality))))

(defn refine-label [clazz]
  (let [k (p/parse-karyotype-class random clazz)]
    (refine clazz :label
            (str "The " k " Karyotype"))))

(defn random-karyotype [name max]
  (refine-label (karyotype-class name
                                 :subclass (random-abnormality-driver max))))

(defn random-karyotype-driver [number max]
  (doseq [i (range number)]
    (random-karyotype i max)))