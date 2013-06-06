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

(ns ncl.karyotype.events
  (:use [tawny.owl])
  (:require [tawny [reasoner :as r]]
            [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]))

(defontology events
  :iri "http://ncl.ac.uk/karyotype/events"
  :prefix "evn:")

(defclass Event)

;; define object properties
(as-inverse
 (defoproperty hasEvent
   :range Event
   :domain k/Karyotype)

 (defoproperty isEventOf
   :range k/Karyotype
   :domain Event)
 )

(as-inverse
 (defoproperty hasBreakPoint
   :range k/ChromosomeComponent
   :domain k/Karyotype)

 (defoproperty isBreakPointOf
   :range k/Karyotype
   :domain k/ChromosomeComponent)
 )

(as-inverse
 (defoproperty hasReceivingBreakPoint
   :subpropertyof hasBreakPoint)

 (defoproperty isReceivingBreakPointOf
   :subpropertyof isBreakPointOf)
 )

(as-inverse
 (defoproperty hasProvidingBreakPoint
   :subpropertyof hasBreakPoint)

 (defoproperty isProvidingBreakPointOf
   :subpropertyof isBreakPointOf)
 )


;; AUXILLARY FUNCTIONS
(defn parentband? [band]
  "Determine if the given band is the parent band"
  (re-find #"HumanChromosomeBand" band))

;; NEEDED?
;; (defn parentchromsomeband? [band]
;;   "Determine if the given band is the parent band"
;;   (re-find #"HumanChromosome[\d]+Band" band))

(defn- get-telomere
  ([band]
     (cond
      (h/pband? (second (clojure.string/split (str band) #"human#")))
      (str (second (clojure.string/split (str band) #"human#|Band")) "BandpTer")
      (h/qband? (second (clojure.string/split (str band) #"human#")))
      (str (second (clojure.string/split (str band) #"human#|Band")) "BandqTer")
      (parentband? (second (clojure.string/split (str band) #"human#")))
      (str (second (clojure.string/split (str band)
                                         #"human#|Chromosome")) "Telomere")
      :default
      (throw (IllegalArgumentException.
              (str "Band syntax not recognized:" band)))))
  ([chromosome type]
     (if (h/pband? (str type))
       (str (second (clojure.string/split (str chromosome)
                                          #"human#|>")) "BandpTer")
       (str (second (clojure.string/split
                     (str chromosome) #"human#|>")) "BandqTer"))))

(defn- get-centromere [chromosome type]
  (if (h/pband? (str type))
    (str (second (clojure.string/split (str chromosome)
                                       #"human#|>")) "Bandp10")
    (str (second (clojure.string/split
                  (str chromosome) #"human#|>")) "Bandq10")))

(defn- getParent [band]
  (second (clojure.string/split (str band) #"Band|[pq]Ter|Cen|human#")))


;; EVENTS
(as-disjoint-subclasses
 Event
 ;; Chromosomal Addition OR Chromosomal Band Addition
 (defclass Addition)
 (defclass Deletion)
 (defclass Duplication)
 (defclass Fission)
 (defclass Insertion)
 (defclass Inversion)
 (defclass Quadruplication)
 (defclass Translocation)
 (defclass Triplication)
 ) ;; end disjoint

(defn addition
  ([n chrom_band]
     (with-ontology
       ncl.karyotype.human/human
       (cond
        (superclass? chrom_band h/HumanChromosome )
        (exactly n hasEvent
                 (owland Addition chrom_band))
        (superclass? chrom_band h/HumanChromosomeBand)
        (exactly n hasEvent
                 (owland Addition
                         (owlsome hasBreakPoint chrom_band)))
        :default
        (throw
         (IllegalArgumentException.
          (str "Addition expects a Chromosome or ChromosomeBand. Got:"
               chrom_band)))))))

;; Chromosomal Deletion OR Chromosomal Band Deletion : includes
;; Terminal deletion with a break AND Interstitial deletion with
;; breakage and reuinion (::) of bands.
;; Invovles only 1 chromosome

(defn deletion
  ;; AKA Chromosomal deletion or terminal band deletion with a break
  ([n chrom_band]
     (with-ontology
       ncl.karyotype.human/human
       (cond
        (superclass? chrom_band h/HumanChromosome)
        (exactly n hasEvent
                 (owland Deletion chrom_band))
        (superclass? chrom_band h/HumanChromosomeBand)
        (exactly n hasEvent
                 (owland Deletion
                         (owlsome hasBreakPoint chrom_band
                                  (get-telomere chrom_band))))
        :default
        (throw
         (IllegalArgumentException.
          (str "Deletion expects a Chromosome or ChromosomeBand. Got:"
               chrom_band))))))
  ;; AKA Interstitial band deletion with breakage and reunion
  ([n band1 band2]
     (if (= (str band1) (str band2))
       (exactly n hasEvent
                (owland Deletion
                        (owlsome hasBreakPoint band1)))
       (exactly n hasEvent
                (owland Deletion
                        (owlsome hasBreakPoint band1 band2))))))

;; Chromosomal Band Duplication
;; Can be preceeded by the triplets dir or inv to indicate direct or
;; inverted direction. There shouldn't be any of this type.

(defn duplication [n band1 band2]
  (exactly n hasEvent
           (owland Duplication
                   (owlsome hasBreakPoint band1 band2))))

;; Chromosomal Band Fission AKA Centric fission - break in the centromere
;; Involves only 1 chromosome

(defn fission [n chromosome]
  (with-ontology
    ncl.karyotype.human/human
    (exactly n hasEvent
             (owland Fission
                     (owlsome hasBreakPoint chromosome
                              (get-telomere chromosome))))))

;; Chromosomal Band Insertion
;; Can be preceeded by the triplets dir or inv to indicate direct or
;; inverted direction
;; Rules: p only/ q only (big to small) = Direct insertion
;; QUERY: How do we classify ins(1)(p13p11q21)? Direct or Inverse?
;; There shouldn't be any of this type

(defn insertion [n band1 band2 band3]
  (exactly n hasEvent
           (owland Insertion
                   (owlsome hasReceivingBreakPoint band1)
                   (owlsome hasProvidingBreakPoint band2 band3))))

;; Chromosomal Band Inversion : includes both paracentric (involves
;; only 1 arm) and pericentric (involves both arms) inversion.
;; Involves only 1 chromosome

(defn inversion [n band1 band2]
  (exactly n hasEvent
           (owland Inversion
                   (owlsome hasBreakPoint band1 band2))))

;; Chromosomal Band Quadruplication
;; Note: It is not possible to indicate the orientations of the
;; segments with the short system!

(defn quadruplication [n band1 band2]
  (exactly n hasEvent
           (owland Quadruplication
                   (owlsome hasBreakPoint band1 band2))))

;; Chromosomal Band Translocation
;; Must involve more than one chromosome/band


(defn- translocation3 [receive1 receive2 provide1 provide2]
  (owland
   (owlsome hasReceivingBreakPoint receive1)
   (owlsome hasReceivingBreakPoint receive2)
   (owlsome hasProvidingBreakPoint provide1)
   (owlsome hasProvidingBreakPoint provide2)))

(defn- translocation2 [provide1 provide2 bands]
  (with-ontology
    ncl.karyotype.human/human
    
    [
     (cond
      (= (count bands) 1)
      (translocation3 (first bands) (get-telomere (first bands)) provide1 provide2)
      (= (count bands) 2)
      (if (= (getParent (first bands)) (getParent (second bands)))
        (translocation3 (first bands) (second bands) provide1 provide2)
        (flatten [(translocation3 (first bands) (get-telomere (first bands)) (second bands) (get-telomere (second bands)))
                  (translocation2 provide1 provide2 (rest bands))]))
      (= (count bands) 3)
      (if (or (= (getParent (first bands)) (getParent (second bands))) (= (getParent (second bands)) (getParent (second (rest bands)))))
        (if (= (getParent (first bands)) (getParent (second bands)))
          (flatten [(translocation3 (first bands) (second bands) (second (rest bands)) (get-telomere (second (rest bands)))) 
                    (translocation2 provide1 provide2 (rest (rest bands)))])
          (flatten [(translocation3 (first bands) (get-telomere (first bands)) (second bands) (second (rest bands)))
                    (translocation3 provide1 provide2 (rest bands))]))
        (flatten [(translocation2 provide1 provide2 (rest bands))
                  (translocation3 (first bands) (get-telomere (first bands)) (second bands) (get-telomere (second bands)))]))
      (> (count bands) 3)
      (if (= (getParent (first bands)) (getParent (second bands)))
        (if (= (getParent (second (rest bands))) (getParent (second (rest (rest bands)))))
          (flatten [(translocation3 (first bands) (second bands) (second (rest bands)) (second (rest (rest bands))))
                    (translocation2 provide1 provide2 (rest (rest bands)))])
          (flatten [(translocation3 (first bands) (second bands) (second (rest bands)) (get-telomere (second (rest bands))))
                    (translocation2 provide1 provide2 (rest (rest bands)))]))
        (if (= (getParent (first (rest bands))) (getParent (second (rest bands))))
          (flatten [(translocation3 (first bands) (get-telomere (first bands)) (second bands) (second (rest bands)))
                    (translocation2 provide1 provide2 (rest bands))])
          (flatten [(translocation3 (first bands) (get-telomere (first bands)) (second bands) (get-telomere (second bands)))
                    (translocation2 provide1 provide2 (rest bands))])))
      :default
      (throw
       (IllegalArgumentException.
        (str "Problem with translocation macro. Got:"
             bands))))
     ]

    ))

;; TODO - make it more efficient!
(defn translocation
  "Returns a translocation restriction.

n is the number of translocations involved.
chrom_no is the number of chromosomes involved.
Bands is the bands involved in the translocation.


"
  [n chrom_no & bands]
  (if (> (count bands) 1)
    (if (= (getParent (first bands)) (getParent (second bands)))
      (exactly 1 hasEvent
               (owland Translocation
                       (take chrom_no
                             (flatten
                              (translocation2 (first bands) (second bands)
                                              bands)))))
      (exactly 1 hasEvent
               (owland Translocation
                       (take chrom_no
                             (flatten
                              (translocation2 (first bands)
                                              (get-telomere (first bands))
                                              bands))))))
    (throw
     (IllegalArgumentException.
      (str "There should be at least 2 band parameters. Got:"
           bands)))))


(defn translocation-new
  "Returns a translocation restriction.
n is the number of translocations involved."
  [n & bands]
  {:pre (> 1 (count bands))}
  
  )


;; Chromosomal Band Triplication
;; QUERY: Book says "It is not possible to indicate the orientations
;; of the segments with the short system" however the example shown
;; seem to show the orientations fine. What other detailed systems
;; occur for the first example?  Similar to Duplication

(defn triplication [n band1 band2]
  (exactly 1 hasEvent
           (owland Triplication
                   (owlsome hasBreakPoint band1 band2))))



(as-disjoint-subclasses
 Duplication
 ;; Chromosomal Band DirectDuplication
 ;; Invovles only 1 chromosome
 (defclass DirectDuplication)
 (defn direct-duplication [n band1 band2]
   (exactly n hasEvent
            (owland DirectDuplication
                    (owlsome hasBreakPoint band1 band2))))

 ;; Chromosomal Band InverseDuplication
 ;; Invovles only 1 chromosome
 (defclass InverseDuplication)
 (defn inverse-duplication [n band1 band2]
   (exactly n hasEvent
            (owland InverseDuplication
                    (owlsome hasBreakPoint band1 band2)))))

(as-disjoint-subclasses
 Insertion
 ;; Choromosomal Band DirectInsertion
 ;; Involves at most 2 chromosomes
 (defclass DirectInsertion)
 (defn direct-insertion [n band1 band2 band3]
   (exactly n hasEvent
            (owland DirectInsertion
                    (owlsome hasReceivingBreakPoint band1)
                    (owlsome hasProvidingBreakPoint band2 band3))))

 ;; Choromosomal Band InverseInsertion
 ;; Involves at most 2 chromosomes
 (defclass InverseInsertion)
 (defn inverse-insertion [n band1 band2 band3]
   (exactly n hasEvent
            (owland InverseInsertion
                    (owlsome hasReceivingBreakPoint band1)
                    (owlsome hasProvidingBreakPoint band2 band3)))))

(as-disjoint-subclasses
 Triplication
 (defclass DirectTriplication)
 (defn direct-triplication [n band1 band2]
   (exactly 1 hasEvent
            (owland DirectTriplication
                    (owlsome hasBreakPoint band1 band2))))

 (defclass InverseTriplication)
 (defn inverse-triplication [n band1 band2]
   (exactly 1 hasEvent
            (owland InverseTriplication
                    (owlsome hasBreakPoint band1 band2)))))

;; Potential new disorder.clj file
;; (defclass Disorder)

;; define object properties
;; (defoproperty hasDisorder
;;   :range Disorder
;;   :domain k/Karyotype
;;   )

;; Information from Registered Chromosome Disorders
;; http://www.rarechromo.co.uk/html/bychromo.asp
;; define all the structural features
;; (as-disjoint-subclasses
;;  Disorder
;;   (defclass additional_unidentified_material)
;;   (defclass balanced_translocation)
;;   (defclass cornelia_de_lange)
;;   (defclass dicentric)
;;   (defclass diploid_triploid_mosaicism)
;;   (defclass diploid_triploid_tetraploid_mosaicism)
;;   (defclass distal_deletion)
;;   (defclass distal_duplication)
;;   (defclass double_ring)
;;   (defclass enlarged_satellite)
;;   (defclass fragile_site)
;;   (defclass hexsomy_mosaic)
;;   (defclass homozygosity)
;;   (defclass interstitial)
;;   (defclass interstitial_duplication)
;;   (defclass inv_dup_del)
;;   (defclass additional_unidentified_material)
;;   (defclass isochromosome)
;;   (defclass isodicentric_!idic!)
;;   (defclass jacobsen_+dup9p)
;;   (defclass jumpimg_translocation)
;;   (defclass maternal_uniparental_disomy)
;;   (defclass mecp2_duplication)
;;   (defclass microduplication)
;;   (defclass microtriplication)
;;   (defclass miller-diecker)
;;   (defclass monosomy)
;;   (defclass monosomy_mosaic)
;;   (defclass mosaic_triple_x_syndrome)
;;   (defclass partial_methylation)
;;   (defclass partial_triplication)
;;   (defclass pentasomy)
;;   (defclass pentasomy mosaic)
;;   (defclass premature_chromatid_separation)
;;   (defclass rearrangement)
;;   (defclass recombinant)
;;   (defclass ring)
;;   (defclass ring_15_mosaicism)
;;   (defclass robertsonian_down_syndrome)
;;   (defclass robertsonian_translocation)
;;   (defclass single_gene_defect)
;;   (defclass small_chromosome)
;;   (defclass subtelomeric_deletion)
;;   (defclass subtelomeric_duplication)
;;   (defclass subtelomeric_microdeletion)
;;   (defclass supernumerary)
;;   (defclass telomeric_duplication)
;;   (defclass tetraploidy)
;;   (defclass tetraploidy_mosaic)
;;   (defclass tetrasomy)
;;   (defclass tetrasomy_mosaic)
;;   (defclass total_premature_chromatid_separation)
;;   (defclass triple_inverted_duplication)
;;   (defclass triplication)
;;   (defclass triploidy)
;;   (defclass trisomy)
;;   (defclass trisomy_mosaic)
;;   (defclass unbalanced_translocation)
;;   (defclass undiagnosed)
;;   (defclass uniparental_disomy)
;;   (defclass unknown)
;;   (defclass unspecified)
;;   (defclass unusual_banding_pattern)
;;   (defclass variant)
;;   (defclass von_willebrand_syndrome)
;;   (defclass worster_drought)
;;   (defclass x-autosomal_translocation)
;;   (defclass x-linked_disorder)
;;   (defclass x-linked_polymicrogyria)
;;   (defclass xx_male)
;;   (defclass xy_female)
;; )
