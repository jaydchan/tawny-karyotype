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
  (:require [tawny [reasoner :as r]]
            [ncl.karyotype [karyotype :as k]]))

(defontology human
  :iri "http://ncl.ac.uk/karyotype/human"
  :prefix "hum:")

(defclass HumanChromosome
  :subclass k/Chromosome)

(defclass HumanChromosomeBand
  :subclass k/ChromosomeBand)

(defclass HumanAutosome
  :subclass HumanChromosome)

;; define all the human chromosomes - autosomes
(as-disjoint
 (doall
  (map
   #(do
      (let [classname (str "HumanChromosome" %)]
        (intern
         *ns* (symbol classname)
         ;; human chromosome defns here.
         (owlclass classname
                   :subclass HumanAutosome)
         
         )))
   (flatten (list (range 1 23))))))

(defclass HumanAllosome
  :subclass HumanChromosome)

;; define all the human chromosomes - allosomes
(as-disjoint
 (doall
  (map
   #(do
      (let [classname (str "HumanChromosome" %)]
        (intern
         *ns* (symbol classname)
         ;; human chromosome defns here.
         (owlclass classname
                   :subclass HumanAllosome)
         
         )))
   (flatten (list "X" "Y")))))

;; function to define all the human bands
(defn humanbands [chromosome & bands]
  
  (let [bandgroup (str
                   (.getFragment
                    (.getIRI
                     chromosome)) "Band")]
    
    (intern *ns*
            (symbol bandgroup)
            (owlclass bandgroup
                      :subclass HumanChromosomeBand))

    (let [bandgroupp (str
                     (.getFragment
                      (.getIRI
                       chromosome)) "Bandp")]
      (intern *ns*
              (symbol bandgroupp)
              (owlclass bandgroupp
                        :subclass bandgroup))

      (let [bandgroupq (str
                       (.getFragment
                        (.getIRI
                         chromosome)) "Bandq")]
        (intern *ns*
                (symbol bandgroupq)
                (owlclass bandgroupq
                          :subclass bandgroup))
        
        (as-disjoint
         (doall
          (map
           (fn [band]
             ;; if band contains a p or q
             (if (re-find #"[pq]" band)
               ;; if band contains a p
               (if (re-find #"[p]" band)
                 (intern *ns*
                         (symbol (str bandgroup band))
                         (owlclass (str bandgroup band)
                                   :subclass bandgroupp
                                   (owlsome k/isBandOf chromosome)))
                 ;; else band contains a q
                 (intern *ns*
                         (symbol (str bandgroup band))
                         (owlclass (str bandgroup band)
                                   :subclass bandgroupq
                                   (owlsome k/isBandOf chromosome))))
               ;; else (cen)
               (intern *ns*
                       (symbol (str bandgroup band))
                       (owlclass (str bandgroup band)
                                 :subclass bandgroup
                                 (owlsome k/isBandOf chromosome)))))
           bands)))))))

;; TOFIX - find the most applicable not the first
(defn- find_parent2 [bandgroup child]
  (first (filter #(re-find (re-pattern child) (str %)) (filter #(re-find (re-pattern (str bandgroup (subs child 0 1))) (str %)) (keys (ns-interns 'ncl.karyotype.human))))))

(defn- find_parent [bandgroup child]
  (if (> (.length child) 2)
    (if (find_parent2 bandgroup child)
      (find_parent2 bandgroup child)
      (let [parent (subs child 0 (- (.length child) 1))]
        (if (re-find #"[.]" (subs parent (- (.length parent) 1) (.length parent)))
          (find_parent bandgroup (subs parent 0 (- (.length parent) 1)))
          (find_parent bandgroup parent))))
    "***ERROR***"))

;; function to define all the human bands
(defn humanbands2 [chromosome & bands]
  
  (let [bandgroup (str
                   (.getFragment
                    (.getIRI
                     chromosome)) "Band")]      
        (as-disjoint
         (doall
          (map
           (fn [band]
             (if-not (subclass? HumanChromosomeBand (str bandgroup band))
               (let [child (str (find_parent bandgroup band))]
                 (try 
                   (intern *ns*
                           (symbol (str bandgroup band))
                           (owlclass (str bandgroup band)
                                     :subclass (str bandgroup (subs child (.length bandgroup)))))
                 (catch Exception e (print "***ERROR***" bandgroup band "has no parent:" child "\n"))))))
           bands)))))

;; function to define all the human bands
(defn humanbands_original [chromosome & bands]
  
  (let [bandgroup (str
                   (.getFragment
                    (.getIRI
                     chromosome)) "Band")]
    
    (intern *ns*
            (symbol bandgroup)
            (owlclass bandgroup
                      :subclass HumanChromosomeBand))

    
    (as-disjoint
     (doall
      (map
       (fn [band]
         (intern *ns*
                 (symbol (str bandgroup band))
                 (owlclass (str bandgroup band)
                           :subclass bandgroup
                           (owlsome k/isBandOf chromosome))))
       bands)))))


;; Currently describes the human chormosome band for resolution 300
;; TODO Need to be able to describe all resolutions

(as-disjoint

(humanbands
 HumanChromosome1
 "pTer"
 "p36.3"
 "p36.2"
 "p36.1"
 "p35"
 "p34"
 "p33"
 "p32"
 "p31"
 "p22"
 "p21"
 "p13"
 "p12"
 "p11"
 "Cen"
 "q11"
 "q12"
 "q21"
 "q22q23q24"
 "q25"
 "q31"
 "q32"
 "q41"
 "q42"
 "q43q44"
 "qTer")
 
(humanbands
 HumanChromosome2
 "pTer"
 "p25"
 "p24"
 "p23"
 "p22"
 "p21"
 "p14p15p16"
 "p13"
 "p12"
 "p11.2"
 "p11.1"
 "Cen"
 "q11.1"
 "q11.2"
 "q12q13q14"
 "q21"
 "q22q23q24"
 "q31"
 "q32"
 "q33"
 "q34q35q36"
 "q37"
 "qTer")

(humanbands
 HumanChromosome3
 "pTer"
 "p26"
 "p25"
 "p22p23p24"
 "p21"
 "p14"
 "p13"
 "p12"
 "p11"
 "Cen"
 "q11.1"
 "q11.2"
 "q12"
 "q13"
 "q21"
 "q22q23q24"
 "q25"
 "q26"
 "q27"
 "q28"
 "q29"
 "qTer")

(humanbands
 HumanChromosome4
 "pTer"
 "p16"
 "p15.3"
 "p15.2"
 "p15.1"
 "p14"
 "p13"
 "p12"
 "p11"
 "Cen"
 "q11"
 "q12"
 "q13"
 "q21"
 "q22"
 "q23"
 "q24"
 "q25"
 "q26"
 "q27"
 "q28"
 "q31.1"
 "q31.2"
 "q31.3"
 "q32"
 "q33"
 "q34"
 "q35"
 "qTer")

(humanbands
 HumanChromosome5
 "pTer"
 "p15"
 "p14"
 "p13"
 "p12"
 "p11"
 "Cen"
 "q11.1"
 "q11.2"
 "q12"
 "q13"
 "q14q15q21"
 "q22"
 "q23"
 "q31"
 "q32q33q34"
 "q35"
 "qTer")

(humanbands
 HumanChromosome6
 "pTer"
 "p25"
 "p24"
 "p23"
 "p22"
 "p21"
 "p12"
 "p11"
 "Cen"
 "q11"
 "q12"
 "q13"
 "q14q15q16"
 "q21"
 "q22"
 "q23"
 "q24"
 "q25q26q27"
 "qTer")

(humanbands
 HumanChromosome7
 "pTer"
 "p22"
 "p21"
 "p15"
 "p14"
 "p13"
 "p12"
 "p11"
 "Cen"
 "q11.1"
 "q11.2"
 "q21"
 "q22"
 "q31"
 "q32"
 "q33q34q35"
 "q36"
 "qTer")

(humanbands
 HumanChromosome8
 "pTer"
 "p23"
 "p22"
 "p21"
 "p12"
 "p11.2"
 "p11.1"
 "Cen"
 "q11.1"
 "q11.2"
 "q12"
 "q13"
 "q21"
 "q22"
 "q23"
 "q24"
 "qTer")

(humanbands
 HumanChromosome9
 "pTer"
 "p24"
 "p23"
 "p22"
 "p21"
 "p13"
 "p12"
 "p11"
 "Cen"
 "q11"
 "q12"
 "q13"
 "q21"
 "q22"
 "q31q32q33"
 "q34"
 "qTer")

(humanbands
 HumanChromosome10
 "pTer"
 "p15"
 "p14"
 "p13"
 "p12"
 "p11.2"
 "p11.1"
 "Cen"
 "q11.1"
 "q11.2"
 "q21"
 "q22"
 "q23"
 "q24"
 "q25"
 "q26"
 "qTer")

(humanbands
 HumanChromosome11
 "pTer"
 "p15"
 "p12p13p14"
 "p11.2"
 "p11.1"
 "Cen"
 "q11"
 "q12"
 "q13"
 "q14q21q22"
 "q23"
 "q24"
 "q25"
 "qTer")

(humanbands
 HumanChromosome12
 "pTer"
 "p13"
 "p12"
 "p11.2"
 "p11.1"
 "Cen"
 "q11"
 "q12"
 "q13"
 "q14"
 "q15"
 "q21q22q23"
 "q24.1"
 "q24.2"
 "q24.3"
 "qTer")

(humanbands
 HumanChromosome13
 "pTer"
 "p13"
 "p12"
 "p11.2"
 "p11.1"
 "Cen"
 "q11"
 "q12"
 "q13"
 "q14"
 "q21"
 "q22"
 "q31q32q33"
 "q34"
 "qTer")

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
 "q12q13q21"
 "q22"
 "q23"
 "q24"
 "q31"
 "q32"
 "qTer")

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
 "q12q13q14"
 "q15"
 "q21"
 "q22"
 "q23"
 "q24"
 "q25"
 "q26"
 "qTer")

(humanbands
 HumanChromosome16
 "pTer"
 "p13.3"
 "p13.2"
 "p13.1"
 "p12"
 "p11.2"
 "p11.1"
 "Cen"
 "q11.1"
 "q11.2"
 "q12q13"
 "q21q22q23"
 "q24"
 "qTer")

(humanbands
 HumanChromosome17
 "pTer"
 "p13"
 "p12"
 "p11.2"
 "p11.1"
 "Cen"
 "q11.1"
 "q11.2"
 "q12"
 "q21"
 "q22q23q24"
 "q25"
 "qTer")

(humanbands
 HumanChromosome18
 "pTer"
 "p11.3"
 "p11.2"
 "p11.1"
 "Cen"
 "q11.1"
 "q11.2"
 "q12"
 "q21"
 "q22"
 "q23"
 "qTer")

(humanbands
 HumanChromosome19
 "pTer"
 "p13"
 "p12"
 "p11"
 "Cen"
 "q11"
 "q12"
 "q13.1q13.2q13.3"
 "q13.4"
 "qTer")

(humanbands
 HumanChromosome20
 "pTer"
 "p13"
 "p12"
 "p11.2"
 "p11.1"
 "Cen"
 "q11.1"
 "q11.2q12q13.1"
 "q13.2"
 "q13.3"
 "qTer")

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
 "q21"
 "q22"
 "qTer")

(humanbands
 HumanChromosome22
 "pTer"
 "p13"
 "p12"
 "p11.2"
 "p11.1"
 "Cen"
 "q11.1"
 "q11.2"
 "q12"
 "q13"
 "qTer")

(humanbands
 HumanChromosomeX
 "pTer"
 "p22.3"
 "p22.2"
 "p22.1"
 "p21"
 "p11.2p11.3p11.4"
 "p11.1"
 "Cen"
 "q11"
 "q12?"
 "q13"
 "q21"
 "q22q23q24"
 "q25q26q27"
 "q28"
 "qTer")

(humanbands
 HumanChromosomeY
 "pTer"
 "p11.3"
 "p11.2"
 "p11.1"
 "Cen"
 "q11.1"
 "q11.21"
 "q11.22"
 "q11.23"
 "q12"
 "qTer"
 )
)

(humanbands2
 HumanChromosome1
 "pTer"
 "p36.3"
 "p36.2"
 "p36.1"
 "p35"
 "p34.3"
 "p34.2"
 "p34.1"
 "p33"
 "p32.3"
 "p32.2"
 "p32.1"
 "p31.3"
 "p31.2"
 "p31.1"
 "p22.3"
 "p22.2"
 "p22.1"
 "p21"
 "p13.3"
 "p13.2"
 "p13.1"
 "p12"
 "p11"
 "Cen"
 "q11"
 "q12"
 "q21"
 "q22"
 "q23.1"
 "q23.2"
 "q23.3"
 "q24"
 "q25.1"
 "q25.2"
 "q25.3"
 "q31"
 "q32.1"
 "q32.2"
 "q32.3"
 "q41"
 "q42.1"
 "q42.2"
 "q42.3"
 "q43"
 "q44"
 "qTer")

(humanbands2
 HumanChromosome2
 "pTer"
"p25.3"
"p25.2"
"p25.1"
"p24"
"p23"
"p22"
"p21"
"p16"
"p15"
"p14"
"p13"
"p12"
"p11.2"
"p11.1"
"Cen"
"q11.1"
"q11.2"
"q12"
"q13"
"q14.1"
"q14.2"
"q14.3"
"q21.1"
"q21.2"
"q21.3"
"q22"
"q23"
"q24.1"
"q24.2"
"q24.3"
"q31"
"q32.1"
"q32.2"
"q32.3"
"q33"
"q34"
"q35"
"q36"
"q37.1"
"q37.2"
"q37.3"
"qTer")

(humanbands2
 HumanChromosome3
"pTer"
"p26"
"p25"
"p24.3"
"p24.2"
"p24.1"
"p23"
"p22"
"p21.3"
"p21.2"
"p21.1"
"p14.3"
"p14.2"
"p14.1"
"p13"
"p12"
"p11.2"
"p11.1"
"Cen"
"q11.1"
"q11.2"
"q12"
"q13.1"
"q13.2"
"q13.3"
"q21"
"q22"
"q23"
"q24"
"q25.1"
"q25.2"
"q25.3"
"q26.1"
"q26.2"
"q26.3"
"q27"
"q28"
"q29"
"qTer")

(humanbands2
 HumanChromosome4
"pTer"
"p16.3"
"p16.2"
"p16.1"
"p15.3"
"p15.2"
"p15.1"
"p14"
"p13"
"p12"
"p11"
"Cen"
"q11"
"q12"
"q13.1"
"q13.2"
"q13.3"
"q21.1"
"q21.2"
"q21.3"
"q22"
"q23"
"q24"
"q25"
"q26"
"q27"
"q28"
"q31.1"
"q31.2"
"q31.3"
"q32"
"q33"
"q34"
"q35"
"qTer")

(humanbands2
 HumanChromosome5
 "pTer"
"p15.3"
"p15.2"
"p15.1"
"p14"
"p13.3"
"p13.2"
"p13.1"
"p12"
"p11"
"Cen"
"q11.1"
"q11.2"
"q12"
"q13.1"
"q13.2"
"q13.3"
"q14"
"q15"
"q21"
"q22"
"q23.1"
"q23.2"
"q23.3"
"q31.1"
"q31.2"
"q31.3"
"q32"
"q33.1"
"q33.2"
"q33.3"
"q34"
"q35.1"
"q35.2"
"q35.3"
"qTer")

(humanbands2
 HumanChromosome6
 "pTer"
"p25"
"p24"
"p23"
"p22.3"
"p22.2"
"p22.1"
"p21.3"
"p21.2"
"p21.1"
"p12"
"p11.2"
"p11.1"
"Cen"
"q11"
"q12"
"q13"
"q14"
"q15"
"q16.1"
"q16.2"
"q16.3"
"q21"
"q22.1"
"q22.2"
"q22.3"
"q23.1"
"q23.2"
"q23.3"
"q24"
"q25.1"
"q25.2"
"q25.3"
"q26"
"q27"
"qTer")

(humanbands2
 HumanChromosome7
 "pTer"
"p22"
"p21"
"p15.3"
"p15.2"
"p15.1"
"p14"
"p13"
"p12"
"p11.2"
"p11.1"
"Cen"
"q11.1"
"q11.21"
"q11.22"
"q11.23"
"q21.1"
"q21.2"
"q21.3"
"q22"
"q31.1"
"q31.2"
"q31.3"
"q32"
"q33"
"q34"
"q35"
"q36"
"qTer")

(humanbands2
 HumanChromosome8
 "pTer"
"p23.3"
"p23.2"
"p23.1"
"p22"
"p21.3"
"p21.2"
"p21.1"
"p12"
"p11.2"
"p11.1"
"Cen"
"q11.1"
"q11.21"
"q11.22"
"q11.23"
"q12"
"q13"
"q21.1"
"q21.2"
"q21.3"
"q22.1"
"q22.2"
"q22.3"
"q23"
"q24.1"
"q24.21"
"q24.22"
"q24.23"
"q24.3"
"qTer")

(humanbands2
 HumanChromosome9
"pTer"
"p24"
"p23"
"p22"
"p21"
"p13"
"p12"
"p11"
"Cen"
"q11"
"q12"
"q13"
"q21.1"
"q21.2"
"q21.3"
"q22.1"
"q22.2"
"q22.3"
"q31"
"q32"
"q33"
"q34.1"
"q34.2"
"q34.3"
"qTer")

(humanbands2
 HumanChromosome10
 "pTer"
"p15"
"p14"
"p13"
"p12.3"
"p12.2"
"p12.1"
"p11.2"
"p11.1"
"Cen"
"q11.1"
"q11.2"
"q21.1"
"q21.2"
"q21.3"
"q22.1"
"q22.2"
"q22.3"
"q23.1"
"q23.2"
"q23.3"
"q24.1"
"q24.2"
"q24.3"
"q25.1"
"q25.2"
"q25.3"
"q26.1"
"q26.2"
"q26.3"
"qTer")

(humanbands2
 HumanChromosome11
"pTer"
"p15.5"
"p15.4"
"p15.3"
"p15.2"
"p15.1"
"p14"
"p13"
"p12"
"p11.2"
"p11.12"
"p11.11"
"Cen"
"q11"
"q12"
"q13.1"
"q13.2"
"q13.3"
"q13.4"
"q13.5"
"q14.1"
"q14.2"
"q14.3"
"q21"
"q22.1"
"q22.2"
"q22.3"
"q23.1"
"q23.2"
"q23.3"
"q24.1"
"q24.2"
"q24.3"
"q25"
"qTer")

(humanbands2
 HumanChromosome12
"pTer"
"p13.3"
"p13.2"
"p13.1"
"p12.3"
"p12.2"
"p12.1"
"p11.2"
"p11.1"
"Cen"
"q11"
"q12"
"q13.1"
"q13.2"
"q13.3"
"q14"
"q15"
"q21.1"
"q21.2"
"q21.3"
"q22"
"q23"
"q24.1"
"q24.2"
"q24.31"
"q24.32"
"q24.33"
"qTer")

(humanbands2
 HumanChromosome13
"pTer"
"p13"
"p12"
"p11.2"
"p11.1"
"Cen"
"q11"
"q12.1"
"q12.2"
"q12.3"
"q13"
"q14.1"
"q14.2"
"q14.3"
"q21.1"
"q21.2"
"q21.3"
"q22"
"q31"
"q32"
"q33"
"q34"
"qTer")

(humanbands2
 HumanChromosome14
"pTer"
"p13"
"p12"
"p11.2"
"p11.1"
"Cen"
"q11.1"
"q11.2"
"q12"
"q13"
"q21"
"q22"
"q23"
"q24.1"
"q24.2"
"q24.3"
"q31"
"q32.1"
"q32.2"
"q32.3"
"qTer")

(humanbands2
 HumanChromosome15
"pTer"
"p13"
"p12"
"p11.2"
"p11.1"
"Cen"
"q11.1"
"q11.2"
"q12"
"q13"
"q14"
"q15"
"q21.1"
"q21.2"
"q21.3"
"q22.1"
"q22.2"
"q22.3"
"q23"
"q24"
"q25"
"q26.1"
"q26.2"
"q26.3"
"qTer")

(humanbands2
 HumanChromosome16
"pTer"
"p13.3"
"p13.2"
"p13.1"
"p12"
"p11.2"
"p11.1"
"Cen"
"q11.1"
"q11.2"
"q12.1"
"q12.2"
"q13"
"q21"
"q22"
"q23"
"q24"
"qTer")

(humanbands2
 HumanChromosome17
"pTer"
"p13"
"p12"
"p11.2"
"p11.1"
"Cen"
"q11.1"
"q11.2"
"q12"
"q21.1"
"q21.2"
"q21.3"
"q22"
"q23"
"q24"
"q25"
"qTer")

(humanbands2
 HumanChromosome18
"pTer"
"p11.32"
"p11.31"
"p11.2"
"p11.1"
"Cen"
"q11.1"
"q11.2"
"q12.1"
"q12.2"
"q12.3"
"q21.1"
"q21.2"
"q21.3"
"q22"
"q23"
"qTer")

(humanbands2
 HumanChromosome19
"pTer"
"p13.3"
"p13.2"
"p13.1"
"p12"
"p11"
"Cen"
"q11"
"q12"
"q13.1"
"q13.2"
"q13.3"
"q13.4"
"qTer")

(humanbands2
 HumanChromosome20
"pTer"
"p13"
"p12"
"p11.2"
"p11.1"
"Cen"
"q11.1"
"q11.2"
"q12"
"q13.1"
"q13.2"
"q13.3"
"qTer")

(humanbands2
 HumanChromosome21
"pTer"
"p13"
"p12"
"p11.2"
"p11.1"
"Cen"
"q11.1"
"q11.2"
"q21"
"q22.1"
"q22.2"
"q22.3"
"qTer")

(humanbands2
 HumanChromosome22
"pTer"
"p13"
"p12"
"p11.2"
"p11.1"
"Cen"
"q11.1"
"q11.2"
"q12.1"
"q12.2"
"q12.3"
"q13.1"
"q13.2"
"q13.3"
"qTer")

(humanbands2
 HumanChromosomeX
"pTer"
"p22.33"
"p22.32"
"p22.31"
"p22.2"
"p22.1"
"p21.3"
"p21.2"
"p21.1"
"p11.4"
"p11.3"
"p11.23"
"p11.22"
"p11.21"
"p11.1"
"Cen"
"q11.1"
"q11.2"
"q12"
"q13"
"q21.1"
"q21.2"
"q21.3"
"q22.1"
"q22.2"
"q22.3"
"q23"
"q24"
"q25"
"q26"
"q27"
"q28"
"qTer")

(humanbands2
 HumanChromosomeY
"pTer"
"p11.32"
"p11.31"
"p11.2"
"p11.1"
"Cen"
"q11.1"
"q11.21"
"q11.221"
"q11.222"
"q11.223"
"q11.23"
"q12"
"qTer")