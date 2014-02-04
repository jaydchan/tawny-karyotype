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

(ns ^{:doc "Creation of random karyotypes which may include addition
and/or deletion events for scaling purposes. Limitation - only
utilises 300-band chromosome bands."
      :author "Jennifer Warrender"}
  ncl.karyotype.random
  (:use [tawny.owl])
  (:require [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]
            [ncl.karyotype [events :as e]]
            [ncl.karyotype [features :as f]]
            [ncl.karyotype [base :as b]]
            [ncl.karyotype [parse :as p]]))

(defontology random
  :iri "http://ncl.ac.uk/karyotype/random"
  :prefix "rkar:"
  :comment "'Random' ontology for Human Karyotype Ontology, written
  using the tawny-owl library")

(defclass RandomKaryotype
  :subclass k/Karyotype)

(def ^{:doc "An array of available diploid base karyotype classes."}
  sex (into [] (direct-subclasses b/base b/k46_XN)))
(defn random-sex []
  "Returns the 46,XX or 46,XY class."
  (let [r (rand-int (count sex))]
    (get sex r)))

;; TODO does not include constitutional karyotypes
(defn karyotype-class [name & frames]
  "Creates a class in the random ontology, with clojure symbol NAME,
and subclass restrictions a) RandomKaryotype b) derivedFrom axiom c)
FRAMES. Returns an OWL class as described."
  (apply owl-class
         (list* (str "r" name)
                :subclass RandomKaryotype
                (owl-some b/derivedFrom (random-sex))
                frames)))

;; TODO Use ontology to derive this...i.e. subclasses is300-band
;; missing chromo 2-22,X,Y bands
(def ^{:doc "An array of available 300-band resolution band information for
  chromosome 1 only."}
  bands-300 [1
  ["p36.3" "p36.2" "p36.1" "p35" "p34" "p33" "p32" "p31" "p22" "p21" "p13" "p12"
   "p11" "q11" "q12" "q21" "q22q23q24" "q25" "q31" "q32" "q41" "q42" "q43q44"]])
(defn get-band [chromosome band]
  "Returns a 300-band chromosomal band class."
  (owl-class h/human (str "HumanChromosome" chromosome "Band" band)))
(defn random-band []
  {:post (true? (h/band? %))}
  "Returns a random 300-band chromosomal band class."
  (let [bands (second bands-300)
        r (rand-int (count bands))]
    (get-band (first bands-300) (get bands r))))

(def ^{:doc "An array of available chromosome classes."}
  chromosomes
  (let [types (direct-subclasses h/human h/HumanChromosome)]
    (into [] (apply clojure.set/union
           (for [type types]
             (into #{} (direct-subclasses h/human type)))))))
(defn random-chromosome []
  {:post (true? (h/chromosome? %))}
  "Returns a human chromosome class."
  (let [r (rand-int (count chromosomes))]
    (get chromosomes r)))

(defn random-terminal-deletion []
  "Returns a terminal deletion event restriction."
  (e/deletion 1 (random-band)))

(defn random-interstitial-deletion []
  "Returns a interstitial deletion event restriction."
  (e/deletion 1 (random-band) (random-band)))

(def ^{:doc "An array of available deletion auxiliary functions."}
  deletions [random-terminal-deletion random-interstitial-deletion])
(defn random-band-deletion []
  "Returns a deletion event restriction."
  (let [r (rand-int (count deletions))]
    ((get deletions r))))

(defn random-chromosome-deletion []
  "Returns a chromosomal deletion event restriction."
  (e/deletion 1 (random-chromosome)))

(defn random-band-addition []
  "Returns a chromosomal band addition event restriction."
  (e/addition 1 (random-band)))

(defn random-chromosome-addition []
  "Returns a chromosomal addition event restriction."
  (e/addition 1 (random-chromosome)))

(def ^{:doc "An array of available addition and deletion functions."}
  abnormalities [random-chromosome-deletion
  random-band-deletion random-chromosome-addition
  random-band-addition])
(defn random-abnormality []
  "Returns an event restriction."
  (let [r (rand-int (count abnormalities))]
    ((get abnormalities r))))

(defn random-karyotype0 [old n]
  "Recursive function - Returns a distinct list of event restrictions."
  (if (= n 0)
    old
    (let [new (conj old (random-abnormality))]
      (if (> (count new) (count old))
        (random-karyotype0 new (- n 1))
        (random-karyotype0 new n)))))

(defn random-abnormality-driver [n]
  "Returns a list of N number of event restrictions."
  (into '() (random-karyotype0 #{} n)))

(defn refine-label [clazz]
  "Returns the updated class definition of CLAZZ."
  (let [k (p/parse-karyotype-class random clazz)]
    (refine clazz :label
            (str "The " k " Karyotype"))))

(defn random-karyotype [name max]
  "Returns a random karyotype class with clojure symbol NAME and has
MAX number of restirctions."
  (refine-label (karyotype-class name
                                 :subclass (random-abnormality-driver max))))

(defn random-karyotype-driver [number max]
  "Creates NUMBER number of random karyotypes with MAX number
of event restrictions. Returns nil."
  (doseq [i (range number)]
    (random-karyotype i max)))

;; TESTING
;; import all ncl.karyotype axioms
(owl-import k/karyotype)
(owl-import h/human)
(owl-import e/events)
(owl-import f/features)
(owl-import b/base)