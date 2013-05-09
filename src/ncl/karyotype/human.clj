;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2012, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.


(ns ncl.karyotype.human
  (:use [tawny.owl])
  (:require [tawny
             [reasoner :as r]
             [read]]
            [clojure.set]
            [ncl.karyotype [karyotype :as k]]))

(defontology human
  :iri "http://ncl.ac.uk/karyotype/human"
  :prefix "hum:")

(owlimport k/karyotype)

;; Auxiliary Functions
(defn pband? [band]
  "Determine if the gvien band is a p band"
  (re-find #"p" band))

(defn qband? [band]
  "Determine if the gvien band is a q band"
  (re-find #"q" band))

(defn ter? [band]
  "Determine if the gvien band is a telomere"
  (re-find #"Ter" band))

(defn cen? [band]
  "Determine if the gvien band is a centromere"
  (re-find #"Cen" band))

(defn create-class-with-superclasses
  "Creates a class with given name and superclasses"
  [name & parents]
  (tawny.read/intern-entity
   (owlclass name :subclass parents)))

;; define classes
(defclass HumanChromosome
  :subclass k/Chromosome)

(defclass HumanChromosomeBand
  :subclass k/ChromosomeBand)

(defclass HumanCentromere
  :subclass k/Centromere)

(defclass HumanTelomere
  :subclass k/Telomere)

;; include disjoint axiom for autosome and allosome
(as-disjoint
 (defclass HumanAutosome
   :subclass HumanChromosome)

 (defclass HumanAllosome
   :subclass HumanChromosome))

;; define all the human autosomes, as disjoint
(as-disjoint
 (doseq [number (range 1 23)]
   (create-class-with-superclasses
     (str "HumanChromosome" number)
     HumanAutosome)))

;; define all the human allosomes, as disjoint
(as-disjoint
 (defclass HumanChromosomeX
   :subclass HumanAllosome)

 (defclass HumanChromosomeY
   :subclass HumanAllosome))

(defn group-for-band
  "Given a band return the appropriate bandgroup"
  [bandgroupp bandgroupq band]
  (cond
   (pband? band)
   bandgroupp
   (qband? band)
   bandgroupq
   (ter? band)
   HumanTelomere
   (cen? band)
   HumanCentromere
   :default
   (throw (IllegalArgumentException.
           (str "Band syntax not recognised: " band)))))

;; private functions
(defn- human-sub-band
  "Adds NAME as a sub-band of BAND and a kind of
PARENT, which is either p or q band."
  [parent name band]
  (create-class-with-superclasses
   name parent
   (owlsome k/isSubBandOf band)))

(defn- human-centromere
  "The band is a centromere, so describe it as such
 and add two classes p10 and q10 which are part of it."
  [bandgroupp bandgroupq bandgroup group]
  (as-disjoint
   (create-class-with-superclasses (str bandgroup "p10")
     (str group "Centromere"))
   (create-class-with-superclasses (str bandgroup "q10")
     (str group "Centromere"))))

(defn- humanbands0 [chromosome parent container bands firstlevel]
  "Recursive auxiliary function for humanbands - used to create
 subbands of the human chromosome bands"
  (let [bandgroup (str
                   (.getFragment
                    (.getIRI
                     chromosome)) "Band")]

    ;; Generates a band
    (if (nil? firstlevel)
      ;; generates a first-level band
      (create-class-with-superclasses (str bandgroup container) parent)
      ;; generates a sub-band
      (human-sub-band parent (str bandgroup container)
                      (str bandgroup firstlevel)))

    ;; Generates sub-bands of current band, or calls recursive function again to
    ;; generate sub-sub-bands
    (as-disjoint
     (doseq [band bands]
       (if (vector? band)
         ;; if band is a vector, then generate sub-sub-bands
         (humanbands0 chromosome parent (first band)
                      (rest band) container)
         ;; else generate sub-band
         (human-sub-band parent (str bandgroup band)
                         (str bandgroup container)))))))

(defn- humanbands [chromosome & bands]
  "Function to generate human chromosome bands for a chromosome"
  (let [group (str
               (.getFragment
                (.getIRI
                 chromosome)))
        bandgroup (str group "Band")
        bandgroupp (str bandgroup "p")
        bandgroupq (str bandgroup "q")
        fgroup (partial group-for-band
                        bandgroupp bandgroupq)]

    ;; generate the band group that all the entities we create
    ;; will be part of
    (create-class-with-superclasses
      bandgroup
      (owlsome k/isBandOf chromosome)
      HumanChromosomeBand)

    ;; generate the band p and band q subclasses
    (as-disjoint
     (create-class-with-superclasses bandgroupp bandgroup)
     (create-class-with-superclasses bandgroupq bandgroup))

    ;; generate the associated centromere entity
    (create-class-with-superclasses
      (str group "Centromere")
      HumanCentromere
      (owlsome k/isComponentOf chromosome))

    ;; generate the associated telomere entity
    (create-class-with-superclasses
      (str group "Telomere")
      HumanTelomere
      (owlsome k/isComponentOf chromosome))

    ;; generates bands
     (doseq [band bands]
       (cond
        (vector? band)
        ;; if we have a set of bands, so we work over all of these
        (humanbands0 chromosome
                     (fgroup (first band))
                     (first band) (rest band) nil)
        ;; else we have a single band
        ;; if the band is the centromere, generate associated
        ;; centromere bands
        (cen? band)
        (human-centromere bandgroupp bandgroupq bandgroup group)
        ;; if the band is a terminal, generate associated telomere 
        ;; bands
        (ter? band)
        (as-disjoint (create-class-with-superclasses
          (str bandgroup band)
          (str group "Telomere")))
        ;; if the band is a p or q band, generate the band
        (or (pband? band)
            (qband? band))
        (create-class-with-superclasses
          (str bandgroup band)
          (fgroup band))
        ;; else the band syntax is not recognized
        :default
        (throw (IllegalArgumentException.
                (str "Band must be string or sequence:" band)))))))

;; Band information for Human Chromosome 1
(humanbands
 HumanChromosome1
 "pTer"
 ["p36.3" "p36.33" "p36.32" "p36.31"]
 ["p36.2" "p36.23" "p36.22" "p36.21"]
 ["p36.1" "p36.13" "p36.12" "p36.11"]
 ["p35" "p35.3" "p35.2" "p35.1"]
 ["p34" "p34.3" "p34.2" "p34.1"]
 "p33"
 ["p32" "p32.3" "p32.2" "p32.1"]
 ["p31" "p31.3" "p31.2" "p31.1"]
 ["p22" "p22.3" "p22.2" "p22.1"]
 ["p21" "p21.3" "p21.2" "p21.1"]
 ["p13" "p13.3" "p13.2" "p13.1"]
 "p12"
 ["p11" "p11.2" "p11.1"]
 "Cen"
 "q11"
 "q12"
 ["q21" "q21.1" "q21.2" "q21.3"]
 ["q22q23q24" "q22"
  ["q23" "q23.1" "q23.2" "q23.3"]
  ["q24" "q24.1" "q24.2" "q24.3"]]
 ["q25" "q25.1" "q25.2" "q25.3"]
 ["q31" "q31.1" "q31.2" "q31.3"]
 ["q32"
  ["q32.1" "q32.11" "q32.12" "q32.13"]
  "q32.2" "q32.3"]
 "q41"
 ["q42"
  ["q42.1" "q42.11" "q42.12" "q42.13"]
  "q42.2" "q42.3"]
 ["q43q44" "q43" "q44"]
 "qTer"
)

;; Band information for Human Chromosome 2
(humanbands
 HumanChromosome2
 "pTer"
 ["p25" "p25.3" "p25.2" "p25.1"]
 ["p24" "p24.3" "p24.2" "p24.1"]
 ["p23" "p23.3" "p23.2" "p23.1"]
 ["p22" "p22.3" "p22.2" "p22.1"]
 "p21"
 ["p14p15p16"
  ["p16" "p16.3" "p16.2" "p16.1"]
  "p15" "p14"]
 ["p13" "p13.3" "p13.2" "p13.1"]
 "p12"
 "p11.2"
 "p11.1"
 "Cen"
 "q11.1"
 "q11.2"
 ["q12q13q14"
  ["q12" "q12.1" "q12.2" "q12.3"]
  "q13"
  ["q14.1" "q14.2" "q14.3"]]
 ["q21" "q21.1" "q21.2" "q21.3"]
 ["q22q23q24"
  ["q22" "q22.1" "q22.2" "q22.3"]
  ["q23" "q23.1" "q23.2" "q23.3"]
  ["q24" "q24.1" "q24.2" "q24.3"]]
 "q31"
 ["q32" "q32.1" "q32.2" "q32.3"]
 "q33"
 ["q34q35q36" "q34" "q35"
  ["q36" "q36.1" "q36.2" "q36.3"]]
 ["q37" "q37.1" "q37.2" "q37.3"]
 "qTer"
)

;; Band information for Human Chromosome 3
(humanbands
 HumanChromosome3
 "pTer"
 ["p26" "p26.3" "p26.2" "p26.1"]
 ["p25" "p25.3" "p25.2" "p25.1"]
 ["p22p23p24"
  ["p24" "p24.3" "p24.2" "p24.1"]
  "p23"
  ["p22" "p22.3" "p22.2" "p22.1"]]
 ["p21"
  ["p21.3" "p21.33" "p21.32" "p21.31"]
  "p21.2" "p21.1"]
 ["p14" "p14.3" "p14.2" "p14.1"]
 "p13"
 ["p12" "p12.3" "p12.2" "p12.1"]
 ["p11" "p11.2" "p11.1"]
 "Cen"
 "q11.1"
 "q11.2"
 ["q12" "q12.1" "q12.2" "q12.3"]
 ["q13"
  ["q13.1" "q13.11" "q13.12" "q13.13"]
  "q13.2"
  ["q13.3" "q13.31" "q13.32" "q13.33"]]
 ["q21" "q21.1" "q21.2" "q21.3"]
 ["q22q23q24"
  ["q22" "q22.1" "q22.2" "q22.3"]
  "q23" "q24"]
 ["q25" "q25.1" "q25.2"
  ["q25.3" "q25.31" "q25.32" "q25.33"]]
 ["q26" "q26.1" "q26.2"
  ["q26.3" "q26.31" "q26.32" "q26.33"]]
 ["q27" "q27.1" "q27.2" "q27.3"]
 "q28"
 "q29"
 "qTer"
)

;; Band information for Human Chromosome 4
(humanbands
 HumanChromosome4
 "pTer"
 ["p16" "p16.3" "p16.2" "p16.1"]
 ["p15.3" "p15.33" "p15.32" "p15.31"]
 "p15.2"
 "p15.1"
 "p14"
 "p13"
 "p12"
 "p11"
 "Cen"
 "q11"
 "q12"
 ["q13" "q13.1" "q13.2" "q13.3"]
 ["q21" "q21.1" "q21.2" "q21.3"]
 ["q22" "q22.1" "q22.2" "q22.3"]
 "q23"
 "q24"
 "q25"
 "q26"
 "q27"
 ["q28" "q28.1" "q28.2" "q28.3"]
 "q31.1"
 ["q31.2" "q31.21" "q31.22" "q31.23"]
 "q31.3"
 ["q32" "q32.1" "q32.2" "q32.3"]
 "q33"
 ["q34" "q34.1" "q34.2" "q34.3"]
 ["q35" "q35.1" "q35.2"]
 "qTer"
)

;; Band information for Human Chromosome 5
(humanbands
 HumanChromosome5
 "pTer"
 ["p15"
  ["p15.3" "p15.33" "p15.32" "p15.31"]
  "p15.2" "p15.1"]
 ["p14" "p14.3" "p14.2" "p14.1"]
 ["p13" "p13.3" "p13.2" "p13.1"]
 "p12"
 "p11"
 "Cen"
 "q11.1"
 "q11.2"
 ["q12" "q12.1" "q12.2" "q12.3"]
 ["q13" "q13.1"
  ["q13.2" "q13.21" "q13.22" "q13.23"]
  "q13.3"]
 ["q14q15q21"
  ["q14" "q14.1" "q14.2" "q14.3"]
  "q15"
  ["q21" "q21.1" "q21.2" "q21.3"]]
 ["q22" "q22.1" "q22.2" "q22.3"]
 ["q23" "q23.1" "q23.2" "q23.3"]
 ["q31" "q31.1" "q31.2" "q31.3"]
 ["q32q33q34" "q32"
  ["q33" "q33.1" "q33.2" "q33.3"]
  "q34"]
 ["q35" "q35.1" "q35.2" "q35.3"]
 "qTer"
)

;; Band information for Human Chromosome 6
(humanbands
 HumanChromosome6
 "pTer"
 ["p25" "p25.3" "p25.2" "p25.1"]
 "p24"
 "p23"
 ["p22"
  ["p22.3" "p22.33" "p22.32" "p22.31"]
  "p22.2" "p22.1"]
 ["p21"
  ["p21.3" "p21.33" "p21.32" "p21.31"]
  "p21.2" "p21.1"]
 ["p12" "p12.3" "p12.2" "p12.1"]
 ["p11" "p11.2" "p11.1"]
 "Cen"
 ["q11" "q11.1" "q11.2"]
 "q12"
 "q13"
 ["q14q15q16"
  ["q14" "q14.1" "q14.2" "q14.3"]
  "q15"
  ["q16" "q16.1" "q16.2" "q16.3"]]
 ["q21" "q21.1" "q21.2" "q21.3"]
 ["q22" "q22.1" "q22.2"
  ["q22.3" "q22.31" "q22.32" "q22.33"]]
 ["q23" "q23.1" "q23.2" "q23.3"]
 ["q24" "q24.1" "q24.2" "q24.3"]
 ["q25q26q27"
  ["q25" "q25.1" "q25.2" "q25.3"]
  "q26" "q27"]
 "qTer"
)

;; Band information for Human Chromosome 7
(humanbands
 HumanChromosome7
 "pTer"
 ["p22" "p22.3" "p22.2" "p22.1"]
 ["p21" "p21.3" "p21.2" "p21.1"]
 ["p15" "p15.3" "p15.2" "p15.1"]
 ["p14" "p14.3" "p14.2" "p14.1"]
 "p13"
 ["p12" "p12.3" "p12.2" "p12.1"]
 ["p11" "p11.2" "p11.1"]
 "Cen"
 "q11.1"
 ["q11.2" "q11.21" "q11.22" "q11.23"]
 ["q21"
  ["q21.1" "q21.11" "q21.12" "q21.13"]
  "q21.2" "q21.3"]
 ["q22" "q22.1" "q22.2" "q22.3"]
 ["q31" "q31.1" "q31.2"
  ["q31.3" "q31.31" "q31.32" "q31.33"]]
 ["q32" "q32.1" "q32.2" "q32.3"]
 ["q33q34q35" "q33" "q34" "q35"]
 ["q36" "q36.1" "q36.2" "q36.3"]
 "qTer"
)

;; Band information for Human Chromosome 8
(humanbands
 HumanChromosome8
 "pTer"
 ["p23" "p23.3" "p23.2" "p23.1"]
 "p22"
 ["p21" "p21.3" "p21.2" "p21.1"]
 "p12"
 ["p11.2" "p11.23" "p11.22" "p11.21"]
 "p11.1"
 "Cen"
 "q11.1"
 ["q11.2" "q11.21" "q11.22" "q11.23"]
 ["q12" "q12.1" "q12.2" "q12.3"]
 ["q13" "q13.1" "q13.2" "q13.3"]
 ["q21"
  ["q21.1" "q21.11" "q21.12" "q21.13"]
  "q21.2" "q21.3"]
 ["q22" "q22.1" "q22.2" "q22.3"]
 ["q23" "q23.1" "q23.2" "q23.3"]
 ["q24"
  ["q24.1" "q24.11" "q24.12" "q24.13"]
  ["q24.2" "q24.21" "q24.22" "q24.23"]
  "q24.3"]
 "qTer"
)

;; Band information for Human Chromosome 9
(humanbands
 HumanChromosome9
 "pTer"
 ["p24" "p24.3" "p24.2" "p24.1"]
 "p23"
 ["p22" "p22.3" "p22.2" "p22.1"]
 ["p21" "p21.3" "p21.2" "p21.1"]
 ["p13" "p13.3" "p13.2" "p13.1"]
 "p12"
 ["p11" "p11.2" "p11.1"]
 "Cen"
 "q11"
 "q12"
 "q13"
 ["q21"
  ["q21.1" "q21.11" "q21.12" "q21.13"]
  "q21.2"
  ["q21.3" "q21.31" "q21.32" "q21.33"]]
 ["q22" "q22.1" "q22.2"
  ["q22.3" "q22.31" "q22.32" "q22.33"]]
 ["q31q32q33"
  ["q31" "q31.1" "q31.2" "q31.3"]
  "q32"
  ["q33" "q33.1" "q33.2" "q33.3"]]
 ["q34" "q34.1" "q34.2" "q34.3"]
 "qTer"
)

;; Band information for Human Chromosome 10
(humanbands
 HumanChromosome10
 "pTer"
 "p15"
 "p14"
 "p13"
 ["p12" "p12.3" "p12.2" "p12.1"]
 "p11.2"
 "p11.1"
 "Cen"
 "q11.1"
 "q11.2"
 ["q21" "q21.1" "q21.2" "q21.3"]
 ["q22" "q22.1" "q22.2" "q22.3"]
 ["q23" "q23.1" "q23.2"
  ["q23.3" "q23.31" "q23.32" "q23.33"]]
 ["q24" "q24.1" "q24.2"
  ["q24.3" "q24.31" "q24.32" "q24.33"]]
 ["q25" "q25.1" "q25.2" "q25.3"]
 ["q26"
  ["q26.1" "q26.11" "q26.12" "q26.13"]
  "q26.2" "q26.3"]
 "qTer"
)

;; Band information for Human Chromosome 11
(humanbands
 HumanChromosome11
 "pTer"
 ["p15" "p15.5" "p15.4" "p15.3" "p15.2" "p15.1"]
 ["p12p13p14"
  ["p14" "p14.3" "p14.2" "p14.1"]
  "p13" "p12"]
 "p11.2"
 ["p11.1" "p11.12" "p11.11"]
 "Cen"
 "q11"
 ["q12" "q12.1" "q12.2" "q12.3"]
 ["q13" "q13.1" "q13.2" "q13.3" "q13.4" "q13.5"]
 ["q14q21q22"
  ["q14" "q14.1" "q14.2" "q14.3"]
  "q21"
  ["q22" "q22.1" "q22.2" "q22.3"]]
 ["q23" "q23.1" "q23.2" "q23.3"]
 ["q24" "q24.1" "q24.2" "q24.3"]
 "q25"
 "qTer"
)

;; Band information for Human Chromosome 12
(humanbands
 HumanChromosome12
 "pTer"
 ["p13"
  ["p13.3" "p13.33" "p13.32" "p13.31"]
  "p13.2" "p13.1"]
 ["p12" "p12.3" "p12.2" "p12.1"]
 ["p11.2" "p11.23" "p11.22" "p11.21"]
 "p11.1"
 "Cen"
 "q11"
 "q12"
 ["q13"
  ["q13.1" "q13.11" "q13.12" "q13.13"]
  "q13.2" "q13.3"]
 ["q14" "q14.1" "q14.2" "q14.3"]
 "q15"
 ["q21q22q23"
  ["q21" "q21.1" "q21.2"
   ["q21.3" "q21.31" "q21.32" "q21.33"]]
  "q22"
  ["q23" "q23.1" "q23.2" "q23.3"]]
 ["q24.1" "q24.11" "q24.12" "q24.13"]
 ["q24.2" "q24.21" "q24.22" "q24.23"]
 ["q24.3" "q24.31" "q24.32" "q24.33"]
 "qTer"
)

;; Band information for Human Chromosome 13
(humanbands
 HumanChromosome13
 "pTer"
 "p13"
 "p12"
 "p11.2"
 "p11.1"
 "Cen"
 "q11"
 ["q12"
  ["q12.1" "q12.11" "q12.12" "q12.13"]
  "q12.2" "q12.3"]
 ["q13" "q13.1" "q13.2" "q13.3"]
 ["q14"
  ["q14.1" "q14.11" "q14.12" "q14.13"]
  "q14.2" "q14.3"]
 ["q21" "q21.1" "q21.2"
  ["q12.3" "q12.31" "q12.32" "q12.33"]]
 ["q22" "q22.1" "q22.2" "q22.3"]
 ["q31q32q33"
  ["q31" "q31.1" "q31.2" "q31.3"]
  ["q32" "q32.1" "q32.2" "q32.3"]
  ["q33" "q33.1" "q33.2" "q33.3"]]
 "q34"
 "qTer"
)

;; Band information for Human Chromosome 14
(humanbands
 HumanChromosome14
 "pTer"
 "p13"
 "p12"
 "p11.2"
 "p11.1"
 "Cen"
 "q11.1"
 "q11.2"
 ["q12q13q21" "q12"
  ["q13" "q13.1" "q13.2" "q13.3"]
  ["q21" "q21.1" "q21.2" "q21.3"]]
 ["q22" "q22.1" "q22.2" "q22.3"]
 ["q23" "q23.1" "q23.2" "q23.3"]
 ["q24" "q24.1" "q24.2" "q24.3"]
 ["q31" "q31.1" "q31.2" "q31.3"]
 ["q32"
  ["q32.1" "q32.11" "q32.12" "q32.13"]
  "q32.2"
  ["q32.3" "q32.31" "q32.32" "q32.33"]]
 "qTer"
)

;; Band information for Human Chromosome 15
(humanbands
 HumanChromosome15
 "pTer"
 "p13"
 "p12"
 "p11.2"
 "p11.1"
 "Cen"
 "q11.1"
 "q11.2"
 ["q12q13q14" "q12"
  ["q13" "q13.1" "q13.2" "q13.3"]
  "q14"]
 ["q15" "q15.1" "q15.2" "q15.3"]
 ["q21" "q21.1" "q21.2" "q21.3"]
 ["q22" "q22.1" "q22.2"
  ["q22.3" "q22.31" "q22.32" "q22.33"]]
 "q23"
 ["q24" "q24.1" "q24.2" "q24.3"]
 ["q25" "q25.1" "q25.2" "q25.3"]
 ["q26" "q26.1" "q26.2" "q26.3"]
 "qTer"
)

;; Band information for Human Chromosome 16
(humanbands
 HumanChromosome16
 "pTer"
 "p13.3"
 "p13.2"
 ["p13.1" "p13.13" "p13.12" "p13.11"]
 ["p12" "p12.3" "p12.2" "p12.1"]
 "p11.2"
 "p11.1"
 "Cen"
 "q11.1"
 "q11.2"
 ["q12q13" "q12.1" "q12.2" "q13"]
 ["q21q22q23" "q21"
  ["q22" "q22.1" "q22.2" "q22.3"]
  ["q23" "q23.1" "q23.2" "q23.3"]]
 ["q24" "q24.1" "q24.2" "q24.3"]
 "qTer"
)

;; Band information for Human Chromosome 17
(humanbands
 HumanChromosome17
 "pTer"
 ["p13" "p13.3" "p13.2" "p13.1"]
 "p12"
 "p11.2"
 "p11.1"
 "Cen"
 "q11.1"
 "q11.2"
 "q12"
 ["q21" "q21.1" "q21.2"
  ["q21.3" "q21.31" "q21.32" "q21.33"]]
 ["q22q23q24" "q22"
  ["q23" "q23.1" "q23.2" "q23.3"]
  ["q24" "q24.1" "q24.2" "q24.3"]]
 ["q25" "q25.1" "q25.2" "q25.3"]
 "qTer"
)

;; Band information for Human Chromosome 18
(humanbands
 HumanChromosome18
 "pTer"
 ["p11.3" "p11.32" "p11.31"]
 ["p11.2" "p11.23" "p11.22" "p11.21"]
 "p11.1"
 "Cen"
 "q11.1"
 "q11.2"
 ["q12" "q12.1" "q12.2" "q12.3"]
 ["q21" "q21.1" "q21.2"
  ["q21.3" "q21.31" "q21.32" "q21.33"]]
 ["q22" "q22.1" "q22.2" "q22.3"]
 "q23"
 "qTer"
)

;; Band information for Human Chromosome 19
(humanbands
 HumanChromosome19
 "pTer"
 ["p13" "p13.3" "p13.2"
  ["p13.1" "p13.13" "p13.12" "p13.11"]]
 "p12"
 "p11"
 "Cen"
 "q11"
 "q12"
 ["q13.1q13.2q13.3"
  ["q13.1" "q13.11" "q13.12" "q13.13"]
  "q13.2"
  ["q13.3" "q13.31" "q13.32" "q13.33"]]
 ["q13.4" "q13.41" "q13.42" "q13.43"]
 "qTer"
)

;; Band information for Human Chromosome 20
(humanbands
 HumanChromosome20
 "pTer"
 "p13"
 ["p12" "p12.3" "p12.2" "p12.1"]
 ["p11.2" "p11.23" "p11.22" "p11.21"]
 "p11.1"
 "Cen"
 "q11.1"
 ["q11.2q12q13.1"
  ["q11.2" "q11.21" "q11.22" "q11.23"]
  "q12"
  ["q13.1" "q13.11" "q13.12" "q13.13"]]
 "q13.2"
 "q13.3"
 "qTer"
)

;; Band information for Human Chromosome 21
(humanbands
 HumanChromosome21
 "pTer"
 "p13"
 "p12"
 "p11.2"
 "p11.1"
 "Cen"
 "q11.1"
 "q11.2"
 ["q21" "q21.1" "q21.2" "q21.3"]
 ["q22"
  ["q22.1" "q22.11" "q22.12" "q22.13"]
  "q22.2" "q22.3"]
 "qTer"
)

;; Band information for Human Chromosome 22
(humanbands
 HumanChromosome22
 "pTer"
 "p13"
 "p12"
 "p11.2"
 "p11.1"
 "Cen"
 "q11.1"
 ["q11.2" "q11.21" "q11.22" "q11.23"]
 ["q12" "q12.1" "q12.2" "q12.3"]
 ["q13" "q13.1" "q13.2"
  ["q13.3" "q13.31" "q13.32" "q13.33"]]
 "qTer"
)

;; Band information for Human Chromosome X
(humanbands
 HumanChromosomeX
 "pTer"
 ["p22.3" "p22.33" "p22.32" "p22.31"]
 "p22.2"
 ["p22.1" "p22.13" "p22.12" "p22.11"]
 ["p21" "p21.3" "p21.2" "p21.1"]
 ["p11.2p11.3p11.4" "p11.4" "p11.3"
  ["p11.2" "p11.23" "p11.22" "p11.21"]]
 "p11.1"
 "Cen"
 ["q11" "q11.1" "q11.2"]
 "q12"
 ["q13" "q13.1" "q13.2" "q13.3"]
 ["q21" "q21.1" "q21.2"
  ["q21.3" "q21.31" "q21.32" "q21.33"]]
 ["q22q23q24"
  ["q22" "q22.1" "q22.2" "q22.3"]
  "q23" "q24"]
 ["q25q26q27" "q25"
  ["q26" "q26.1" "q26.2" "q26.3"]
  ["q27" "q27.1" "q27.2" "q27.3"]]
 "q28"
 "qTer"
)

;; Band information for Human Chromosome Y
(humanbands
 HumanChromosomeY
 "pTer"
 ["p11.3" "p11.32" "p11.31"]
 "p11.2"
 "p11.1"
 "Cen"
 "q11.1"
 "q11.21"
 ["q11.22" "q11.221" "q11.222" "q11.223"]
 "q11.23"
 "q12"
 "qTer"
 )

;; add disjoint axiom for the children of HumanCentromere
(disjointclasseslist
 (into () (isubclasses HumanCentromere)))

;; add disjoint axiom for the children of HumanChromosomeBand
(disjointclasseslist
 (into () (isubclasses HumanChromosomeBand)))

;; add disjoint axiom for the children of HumanTelomere
(disjointclasseslist
 (into () (isubclasses HumanTelomere)))

(r/reasoner-factory :hermit)

(import 'org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor)

(defn reasoner-progress-monitor-quieter []
  (proxy [org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor] []
    (reasonerTaskBusy[]
      (println "Reasoner task busy");; stuff
      )
    (reasonerTaskProgressChanged [val max]
      ;;(println "Reasoner task changed" val ":" max)
      )
    (reasonerTaskStarted [name]
      (println "reasoner task started" name))
    (reasonerTaskStopped []
      (println "reasoner task stopped"))))


(defn- create-first-level-bands-disjoint
  "Creates the disjoint axiom for given a parent p or q band"
  [chromosomeband]

  ;; generate temporary equivalent OWL class
  (let [temp-entity
        (owlclass "tempentity"
          :equivalent
          (owlsome k/isSubBandOf chromosomeband))]

    ;; obtain the first-level bands of a given parent p or q band
    (binding [r/*reasoner-progress-monitor*
              reasoner-progress-monitor-quieter]
      (disjointclasseslist
       ;; the difference of
       (clojure.set/difference
        ;; a set of all subclasses of CHROMOSOMEBAND
        (into #{} (isubclasses chromosomeband))
        ;; a set of all inferred subclasses of TempEntity (i.e. all sub-bands of
        ;; the associated CHROMOSOMEBAND)
        (.getFlattened (r/isubclasses TempEntity)))))

    ;; remove temporary equivalent OWL class
    (remove-entity temp-entity)
)

;; add disjoint axiom for the first children of each HumanChromosomeBand
(let [chromosomebands (into [] (isubclasses HumanChromosomeBand))]
  (doseq [chromosomeband chromosomebands]
    (doseq [porqband (into [] (isubclasses chromosomeband))]
      (create-first-level-bands-disjoint porqband))))

;; add disjoint axioms for terminal p and terminal q for
;; each chromosome telomere
(doseq [cband (into [] (isubclasses HumanTelomere))]
  (disjointclasseslist (into () (isubclasses cband))))