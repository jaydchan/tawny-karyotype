;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2012, Newcastle University

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

(ns ^{:doc "Defining event information for human karyotypes."
      :author "Jennifer Warrender"}
  ncl.karyotype.events
  (:use [tawny.owl])
  (:require [ncl.karyotype
             [generic :as g]
             [karyotype :as k]
             [human :as h]]
            [tawny
             [reasoner :as rea]
             [render :as ren]]))

(defontology events
  :iri "http://ncl.ac.uk/karyotype/events"
  :prefix "evn:"
  :comment "Event ontology for Human Karyotype Ontology, written using
  the tawny-owl library.")

;; OWL CLASSES - EVENTS
(defclass Event)

(as-disjoint-subclasses
 Event
 (defclass Addition)
 (defclass Deletion)
 (defclass Duplication)
 (defclass Fission)
 (defclass Insertion)
 (defclass Inversion)
 (defclass Quadruplication)
 (defclass Translocation)
 (defclass Triplication))

(as-disjoint-subclasses
 Duplication
 (defclass DirectDuplication)
 (defclass InverseDuplication))

(as-disjoint-subclasses
 Insertion
 (defclass InsertionOneChromosome)
 (defclass InsertionTwoChromosome))

(as-disjoint-subclasses
 InsertionOneChromosome
 (defclass DirectInsertionOneChromosome)
 (defclass InverseInsertionOneChromosome))

(as-disjoint-subclasses
 InsertionTwoChromosome
 (defclass DirectInsertionTwoChromosome)
 (defclass InverseInsertionTwoChromosome))

;; define object properties
;; event object properties
(as-inverse
 (defoproperty hasEvent
   :range Event
   :domain k/Karyotype)
 (defoproperty isEventOf))

(as-inverse
 (defoproperty hasDirectEvent
   :subproperty hasEvent)
 (defoproperty isDirectEventOf
   :subproperty isEventOf))

;; due to build dependancy, the subproperty chain axiom will be added
;; in named-clj
(as-inverse
 (defoproperty hasDerivedEvent
   :subproperty hasEvent)
 (defoproperty isDerivedEventOf
   :subproperty isEventOf))

;; breakpoint object properties
(as-inverse
 (defoproperty hasBreakPoint
   :range k/ChromosomeComponent
   :domain k/Karyotype)
 (defoproperty isBreakPointOf))

(as-inverse
 (defoproperty hasReceivingBreakPoint
   :subproperty hasBreakPoint)
 (defoproperty isReceivingBreakPointOf
   :subproperty isBreakPointOf))

(as-inverse
 (defoproperty hasProvidingBreakPoint
   :subproperty hasBreakPoint)
 (defoproperty isProvidingBreakPointOf
   :subproperty isBreakPointOf))

;; AUXILLARY FUNCTIONS
(defn- parentband?
  "Determines if the given BAND is the parent band."
  [band]
  (= h/HumanChromosomeBand band))

(defn- telomere-band
  "Returns sub-axiom to find associated telomere band using given CHROMOSOME."
  [chromosome] {:pre [(h/chromosome? chromosome)]}
  (owl-and h/HumanChromosomeBand
           (owl-some k/isBandOf chromosome)
           (owl-some k/isBandOf h/HumanTelomere)))

(defn- telomere-component
  "Returns sub-axiom to find associated telomere component using given
CHROMOSOME."
  [chromosome] {:pre [(h/chromosome? chromosome)]}
  (owl-and h/HumanTelomere
           (owl-some k/isComponentOf chromosome)))

;; TODO Get rid of hermitt window popup
;; (rea/reasoner-factory :hermit)
;; (binding [rea/*reasoner-progress-monitor*
;;           (atom
;;            rea/reasoner-progress-monitor-silent)]
;;   (defn query-class [o & frames]
;;     "Returns the subclasses of the temp query class."
;;     (with-probe-entities o
;;       [clazz (apply owl-class
;;                      (list* "temp"
;;                             frames))]
;;       (-> (rea/isubclasses o clazz)))))

(defn- filter-parent-axioms
  "Returns PROPERTY axioms of given CLAZZ."
  [clazz property]
  (let [parents (superclasses h/human clazz)
        axioms (filter #(instance?
                         org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom %)
                       parents)]
    (filter #(= property (.getProperty %)) axioms)))

(defn- get-chromosome0
  "Returns a chromosome found in the filler of given AXIOMS."
  [axioms]
  (.getFiller (first (filter #(h/chromosome? (.getFiller %)) axioms))))

(defn get-chromosome
  "Returns associated chromosome of given CLAZZ."
  [clazz]
  (cond
   (h/chromosome? clazz)
   clazz
   (or (= h/HumanChromosomeBand clazz)
       (= h/HumanCentromere clazz)
       (= h/HumanTelomere clazz))
   h/HumanChromosome
   (h/band? clazz)
   (get-chromosome0 (filter-parent-axioms clazz k/isBandOf))
   (or (h/cen? clazz) (h/ter? clazz))
   (get-chromosome0 (filter-parent-axioms clazz k/isComponentOf))
   :default
   (throw (IllegalArgumentException.
           (str "Class not recognized:" clazz)))))

(defn- get-centromere-string
  "Returns associated centromere of given CLAZZ. The centromere can be
specified by providing an ARM function."
  [clazz arm] {:pre [(or (= arm h/pband?) (= arm h/qband?) (nil? arm))]}
  (cond
   (h/cen? clazz)
   clazz
   (or (= h/HumanChromosomeBand clazz)
       (= h/HumanTelomere clazz)
       (= h/HumanChromosome clazz)
       (= h/HumanAutosome clazz)
       (= h/HumanSexChromosome clazz))
   h/HumanCentromere
   (h/pband? clazz)
   (owl-class h/human
              (str (re-find #"HumanChromosome[\dXY]+Bandp" (str clazz)) "10"))
   (h/qband? clazz)
   (owl-class h/human
              (str (re-find #"HumanChromosome[\dXY]+Bandq" (str clazz)) "10"))
   (or (h/chromosome? clazz) (h/band? clazz) (h/ter? clazz))
   (if (nil? arm)
     (owl-class h/human
                (str
                 (re-find #"HumanChromosome[\dXY]+" (str clazz)) "Centromere"))
     (filter arm (direct-subclasses
                  h/human
                  (owl-class
                   h/human
                   (str
                    (re-find #"HumanChromosome[\dXY]+" (str clazz)) "Band")))))
   :default
   (throw (IllegalArgumentException.
           (str "Class not recognized:" clazz)))))

(defn- get-band-no
  [band] {:pre [(h/band? band)]}
  (re-find #"[\d\.]+$"
           (g/get-entity-short-string
            (owl-class h/human band))))

(defn- get-direction
  "Determines the direction of the band range as either direct or inverse"
  [band1 band2] {:pre [(h/band? band1) (h/band? band2)]}
  (cond
   (or (not (or (h/pband? band1) (h/qband? band1)))
       (not (or (h/pband? band2) (h/qband? band2))))
   "Unknown"
   (and (h/pband? band1) (h/qband? band2))
   "Direct"
   (and (h/qband? band1) (h/pband? band2))
   "Inverse"
   (or
    (and (h/pband? band1) (h/pband? band2))
    (and (h/qband? band1) (h/qband? band2)))
   (let [digit1 (get-band-no band1)
         digit2 (get-band-no band1)]
     (if (or (= digit1 nil) (= digit2 nil))
       "Unknown"
       (if (<= (read-string digit1) (read-string digit2))
         "Direct"
         "Inverse")))))

(defn- get-telomere-string
  "Returns associated telomere of given CLAZZ."
  [clazz]
  (cond
   (h/ter? clazz)
   clazz
   (or (= h/HumanChromosomeBand clazz)
       (= h/HumanCentromere clazz)
       (= h/HumanChromosome clazz)
       (= h/HumanAutosome clazz)
       (= h/HumanSexChromosome clazz))
   h/HumanTelomere
   (h/pband? clazz)
   (owl-class h/human
              (str (re-find #"HumanChromosome[\dXY]+Bandp" (str clazz)) "Ter"))
   (h/qband? clazz)
   (owl-class h/human
              (str (re-find #"HumanChromosome[\dXY]+Bandq" (str clazz)) "Ter"))
   (or (h/chromosome? clazz) (h/band? clazz) (h/cen? clazz))
   (owl-class h/human
              (str (re-find #"HumanChromosome[\dXY]+" (str clazz)) "Telomere"))
   :default
   (throw (IllegalArgumentException.
           (str "Class not recognized:" clazz)))))

;; (defn- get-telomere-ontology [clazz]
;;   "Returns associated telomere of given CLAZZ."
;;   (cond
;;    (h/ter? clazz)
;;    clazz
;;    (or (= h/HumanChromosomeBand clazz)
;;        (= h/HumanCentromere clazz)
;;        (= h/HumanChromosome clazz)
;;        (= h/HumanAutosome clazz)
;;        (= h/HumanSexChromosome clazz))
;;    h/HumanTelomere
;;    (h/chromosome? clazz)
;;    (first (query-class h/human :equivalent (telomere-component clazz)))
;;    (h/pband? clazz)
;;    (first (filter h/pband?
;;                   (query-class h/human
;;                                :equivalent (telomere-band
;;                                             (get-chromosome clazz)))))
;;    (h/qband? clazz)
;;    (first (filter h/qband?
;;                   (query-class h/human
;;                                :equivalent (telomere-band
;;                                             (get-chromosome clazz)))))
;;    (or (h/band? clazz) (h/cen? clazz))
;;    (first (query-class h/human
;;                        :equivalent (telomere-component (get-chromosome clazz))))
;;    :default
;;    (throw (IllegalArgumentException.
;;            (str "Class not recognized:" clazz)))))

;; get-telomere-ontology takes awhile
(defn get-telomere
  "TODO"
  [clazz]
  (get-telomere-string clazz))

;; hasEvent auxiliary functions
(defn- some-event
  "Returns a LazySeq of SomeValuesFrom hasEvent restrictions."
  [axiom]
  (owl-some hasEvent axiom))

(defn- exactly-event
  "Returns a (single) ExactCardinality hasEvent restriction."
  [n axiom] {:pre [(number? n)]}
  (exactly n hasEvent axiom))

(defn event
  "(Either) Returns a LazySeq SomeValuesFrom or one ExactCardinality
hasEvent restrictions."
  [n axiom]
  (if (nil? n)
    (some-event axiom)
    (exactly-event n axiom)))

;; hasDirectEvent auxiliary functions
(defn- some-direct-event
  "Returns a LazySeq of SomeValuesFrom hasDirectEvent restrictions"
  [axiom]
  (owl-some hasDirectEvent axiom))

(defn- exactly-direct-event
  "Returns a (single) ExactCardinality hasDirectEvent restriction."
  [n axiom] {:pre [(number? n)]}
  (exactly n hasDirectEvent axiom))

(defn direct-event
  "(Either) Returns a LazySeq SomeValuesFrom or one ExactCardinality
hasDirectEvent restrictions."
  [n axiom]
  (if (nil? n)
    (some-direct-event axiom)
    (exactly-direct-event n axiom)))

;; NOT NEEDED ???
;; (as-disjoint-subclasses
;;  Insertion
;;  (defclass DirectInsertion)
;;  (defclass InverseInsertion))
;; (as-subclasses
;;  DirectInsertion
;;  DirectInsertionOneChromosome DirectInsertionTwoChromosome)
;; (as-subclasses
;;  InverseInsertion
;;  InverseInsertionOneChromosome InverseInsertionTwoChromosome)

(as-disjoint-subclasses
 Triplication
 (defclass DirectTriplication)
 (defclass InverseTriplication))


;; FUNCTIONS

;; Addition patterns
(defn addition-chromosome
  "Pattern - returns part of chromosomal addition axiom."
  [chromosome] {:pre [(h/chromosome? chromosome)]}
  (owl-and Addition chromosome))

(defn addition-band
  "Pattern - returns part of chromosomal band addition axiom."
  [band] {:pre [(h/band? band)]}
  (owl-and Addition
           (owl-some hasBreakPoint band)))

;; Chromosomal Addition OR Chromosomal Band Addition
;; Invovles only 1 chromosome
(defn addition
  "Returns an addition retriction. N is the number of addition
restrictions. CHROM_BAND is either of type HumanChromosome or
HumanChromosomeBand."
  [n chrom_band]
  (cond
   ;; If chrom_band is of type HumanChromosome then restriction
   ;; represents a chromosomal gain.
   (h/chromosome? chrom_band)
   (direct-event n (addition-chromosome chrom_band))
   ;; If chrom_band is of type HumanChromosomeBand then
   ;; restriction represents a chromosomal band addition.
   (h/band? chrom_band)
   (direct-event n (addition-band chrom_band))
   :default
   (throw
    (IllegalArgumentException.
     (str "Addition expects a Chromosome or ChromosomeBand. Got:"
            chrom_band)))))

;; Deletion patterns
(defn deletion-chromosome
  "Pattern - returns chromosomal deletion axiom."
  [chromosome] {:pre [(h/chromosome? chromosome)]}
  (owl-and Deletion chromosome))

(defn deletion-band
  "Pattern - return chromosomal band deletion axiom."
  [band1 band2] {:pre [(h/band? band1)
                       (or (h/ter? band2) (h/band? band2))]}
  (owl-and Deletion
           (owl-some hasBreakPoint band1 band2)))

;; Chromosomal Deletion OR Chromosomal Band Deletion : includes
;; Terminal deletion with a break AND Interstitial deletion with
;; breakage and reuinion (::) of bands.
;; Invovles only 1 chromosome
(defn deletion
  "Returns a deletion retriction. N is the number of deletion
restrictions. CHROM_BAND is either of type HumanChromosome or
HumanChromosomeBand. BAND, BAND1, BAND are of type
HumanChromosomeBand."
  ([n chrom_band]
     (cond
      ;; If chrom_band is of type HumanChromosome then restriction
      ;; represents a chromosomal loss.
      (h/chromosome? chrom_band)
      (direct-event n (deletion-chromosome chrom_band))
      ;; If chrom_band is of type HumanChromosomeBand then
      ;; restriction represents a terminal band deletion with a break
      ;; (:).
      (h/band? chrom_band)
      (direct-event n (deletion-band chrom_band (get-telomere chrom_band)))
      :default
      (throw
       (IllegalArgumentException.
        (str "Deletion expects a HumanChromosome or
               HumanChromosomeBand. Got:" chrom_band)))))
  ([n band1 band2]
     ;; This represents Interstitial band deletion with breakage and
     ;; reunion (::).  band1, band2 are of type HumanChromosomeBand.
     (direct-event n (deletion-band band1 band2))))

(defn duplication-pattern
  "Pattern - returns an EVENT duplication restriction using BAND1 and
BAND2 bands."
  [event band1 band2] {:pre [(or (superclass? events event Duplication)
                                 (= event Duplication))]}
  (owl-and event
           (owl-some hasBreakPoint band1 band2)))

;; Chromosomal Band Duplication
;; Can be preceeded by the triplets dir or inv to indicate direct or
;; inverted direction
;; Invovles only 1 chromosome
(defn duplication
  "Returns a duplication retriction. N is the number of duplication
restrictions. BAND1, BAND2 are of type HumanChromosomeBand."
  [n band1 band2] {:pre [(h/band? band1) (h/band? band2)]}
  (let [direction (get-direction band1 band2)]
    (cond
     (= direction "Unknown")
     (direct-event n
                   (duplication-pattern Duplication band1 band2))
     (= direction "Direct")
     (direct-event n
                   (duplication-pattern DirectDuplication band1 band2))
     (= direction "Inverse")
     (direct-event n
                   (duplication-pattern InverseDuplication band1 band2)))))

;; Chromosomal Band Fission AKA Centric fission - break in the centromere
;; Involves only 1 chromosome
(defn fission
  "Returns a fission retriction. N is the number of fission
restrictions. BAND is of type HumanChromosomeBand."
  [n chrom_band]
  (cond
   (h/band? chrom_band)
   (direct-event n (owl-and Fission
                            (owl-some hasBreakPoint chrom_band
                                      (get-telomere chrom_band))))
   (h/chromosome? chrom_band)
   [(fission n (get-centromere-string chrom_band h/pband?))
    (fission n (get-centromere-string chrom_band h/qband?))]
   :default
   (throw
    (IllegalArgumentException.
     (str "Fission expects a HumanChromosome or
               HumanChromosomeBand. Got:" chrom_band)))))

;; Chromosomal Band Insertion
;; Can be preceeded by the triplets dir or inv to indicate direct or
;; inverted direction
;; Involves at most 2 chromosomes
(defn insertion-pattern
  "Pattern - returns an EVENT insertion restriction using BAND1, BAND2
and BAND3 bands."
  [event band1 band2 band3] {:pre [(or (superclass? events event Insertion)
                                       (= event Insertion))]}
  (owl-and event
           (owl-some hasReceivingBreakPoint band1)
           (owl-some hasProvidingBreakPoint band2 band3)))

;; Choromosomal Band Insertion
;; Involves at most 2 chromosomes
(defn insertion
  "Returns an insertion retriction. N is the number of insertion
restrictions. BAND1, BAND2, BAND3 is of type HumanChromosomeBand."
  ([n chrom1] {:pre [(= 3 (count chrom1))]}
     (let [band2 (second chrom1)
           band3 (second (rest chrom1))
           direction (get-direction band2 band3)]
       (cond
        (= direction "Unknown")
        (direct-event n
                      (insertion-pattern InsertionOneChromosome
                                         (first chrom1) band2 band3))
        (= direction "Direct")
        (direct-event n
                      (insertion-pattern DirectInsertionOneChromosome
                                         (first chrom1) band2 band3))
        (= direction "Inverse")
        (direct-event n
                      (insertion-pattern InverseInsertionOneChromosome
                                         (first chrom1) band2 band3)))))
  ([n chrom1 chrom2] {:pre [(= 1 (count chrom1)) (= 2 (count chrom2))]}
     (let [band2 (first chrom2)
           band3 (second chrom2)
           direction (get-direction band2 band3)]
       (cond
        (= direction "Unknown")
        (direct-event n
                      (insertion-pattern InsertionTwoChromosome
                                         (first chrom1) band2 band3))
        (= direction "Direct")
        (direct-event n
                      (insertion-pattern DirectInsertionTwoChromosome
                                         (first chrom1) band2 band3))
        (= direction "Inverse")
        (direct-event n
                      (insertion-pattern InverseInsertionTwoChromosome
                                         (first chrom1) band2 band3))))))

;; Chromosomal Band Inversion : includes both paracentric (involves
;; only 1 arm) and pericentric (involves both arms) inversion.
;; Involves only 1 chromosome
(defn inversion-pattern
  "Returns an inversion retriction. BAND1, BAND2 is of type
HumanChromosomeBand."
  [band1 band2] {:pre [(or (= (get-chromosome band1) (get-chromosome band2))
                           (parentband? band1) (parentband? band2))
                       (h/band? band1) (h/band? band2)]}
  (owl-and Inversion
           (owl-some hasBreakPoint band1 band2)))

(defn inversion
  "Returns an inversion retriction. N is the number of inversion
restrictions. BAND1, BAND2 is of type HumanChromosomeBand."
  [n band1 band2]
  (direct-event n (inversion-pattern band1 band2)))

;; Chromosomal Band Quadruplication
;; Note: It is not possible to indicate the orientations of the
;; segments with the short system!
(defn quadruplication
  "Returns a quadruplication retriction. N is the number of
quadruplication restrictions. BAND1, BAND2 is of type
HumanChromosomeBand."
  [n band1 band2] {:pre [(or (= (get-chromosome band1) (get-chromosome band2))
                             (parentband? band1) (parentband? band2))
                         (h/band? band1) (h/band? band2)]}
  (direct-event n (owl-and Quadruplication
                           (owl-some hasBreakPoint band1 band2))))

;; Auxilary function for translocation function.
(defn- adjust-bands
  "Returns a vector of vectors - each vector contains 2 bands. BANDS
is a list of vectors - each vector contains 1 or 2 HumanChromosome."
  [bands]
  (into []
        (for [band bands]
          (cond
           ;; If band only contains 1 HumanChromosomeBand then
           ;; identify associated telomere.
           (= (count band) 1)
           (conj band (get-telomere (first band)))
           ;; If band only contains 2 HumanChromosomeBand then
           ;; return band i.e. do nothing.
           (= (count band) 2)
           band
           :default
           (throw (IllegalArgumentException.
                   (str "Band should contain 1 or 2
                   HumanChromosomeBand: " band)))))))

;; Chromosomal Band Translocation
;; Must involve more than one chromosome/band
(defn translocation
  "Returns a translocation restriction. N is the number of
translocation restrictions.BANDS is a list of vectors - each vector
contains 1 or 2 HumanChromosomeBand"
  [n & bands] {:pre [(> (count bands) 1)]}
  (let [sorted-bands (adjust-bands bands)]
    (direct-event n
             (owl-and Translocation
                     (for [x (range (count sorted-bands))]
                       (let [curr-band (get sorted-bands x)]
                         (apply owl-and
                                (owl-some hasReceivingBreakPoint
                                          (first curr-band))
                                (owl-some hasReceivingBreakPoint
                                          (second curr-band))
                                (if (= x (- (count sorted-bands) 1))
                                  [(owl-some hasProvidingBreakPoint
                                            (first (first sorted-bands)))
                                   (owl-some hasProvidingBreakPoint
                                            (second (first sorted-bands)))]
                                  (let [next-band (get sorted-bands (+ x 1))]
                                    [(owl-some hasProvidingBreakPoint
                                              (first next-band))
                                     (owl-some hasProvidingBreakPoint
                                              (second next-band))])))))))))

;; Chromosomal Band Triplication
;; Invovles only 1 chromosome
;; QUERY: Book says "It is not possible to indicate the orientations
;; of the segments with the short system" however the example shown
;; seem to show the orientations fine. What other detailed systems
;; occur for the first example?  Similar to Duplication
(defn triplication-pattern
  "Returns a triplication retriction. N is the number of triplication
restrictions. BAND1, BAND2 is of type HumanChromosomeBand."
  [event band1 band2] {:pre [(h/band? band1) (h/band? band2)]}
  (owl-and event
           (owl-some hasBreakPoint band1 band2)))

(defn triplication
  "Returns a triplication retriction. N is the number of triplication
restrictions. BAND1, BAND2 is of type HumanChromosomeBand."
  [n band1 band2]
  (let [direction (get-direction band1 band2)]
    (cond
     (= direction "Unknown")
     (direct-event n
                   (triplication-pattern Triplication band1 band2))
     (= direction "Direct")
     (direct-event n
                   (triplication-pattern DirectTriplication band1 band2))
     (= direction "Inverse")
     (direct-event n
                   (triplication-pattern InverseTriplication band1 band2)))))