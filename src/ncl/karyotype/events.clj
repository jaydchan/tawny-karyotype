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
(defn- getTerminal
  ([band]
     (if (h/pband? (second (clojure.string/split (str band) #"human#")))
       (str (second (clojure.string/split (str band) #"human#|Band")) "pTer")
       (str (second (clojure.string/split (str band) #"human#|Band")) "qTer")))
  ([chromosome type]
     (if (h/pband? (str type))
       (str (second (clojure.string/split (str chromosome) #"human#|>")) "pTer")
       (str (second (clojure.string/split
                     (str chromosome) #"human#|>")) "qTer"))))

(defn- getCentromere
  ([chromosome]
     (str (second (clojure.string/split (str chromosome) #"human#|>")) "Cen"))
  ([chromosome type]
     (if (h/pband? (str type))
       (str (second (clojure.string/split (str chromosome) #"human#|>")) "p10")
       (str (second (clojure.string/split
                     (str chromosome) #"human#|>")) "q10"))))

(defn- getParent [band]
  (second (clojure.string/split (str band) #"Band|[pq]Ter|Cen|human#")))


;; EVENTS
(as-disjoint-subclasses
 Event

 ;; Chromosomal Addition OR Chromosomal Band Addition
 (defclass Addition)
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
 (defclass Deletion)
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
                                   (getTerminal chrom_band))))
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
 (defclass Duplication)

 ;; Chromosomal Band Fission AKA Centric fission - break in the centromere
 ;; Involves only 1 chromosome
 (defclass Fission)
 (defn fission
   ([n band1 band2]
      (with-ontology
        ncl.karyotype.human/human
        (exactly n hasEvent
                 (owland Fission
                         (owlsome hasBreakPoint band1 band1)))))
   ([n chromosome]
      [(deletion n chromosome)
       (fission n
                (getTerminal chromosome "p")
                (getCentromere chromosome "p"))
       (fission n
                (getCentromere chromosome "q")
                (getTerminal chromosome "q"))]))

 ;; Chromosomal Band Insertion
 ;; Can be preceeded by the triplets dir or inv to indicate direct or inverted direction
 ;; Rules: p only/ q only (big to small) = Direct insertion
 ;; QUERY: How do we classify ins(1)(p13p11q21)? Direct or Inverse?
 ;; There shouldn't be any of this type
 (defclass Insertion)
 (defn insertion
   ([n band1 band2 band3]
      (exactly n hasEvent
               (owland Insertion
                       (owlsome hasReceivingBreakPoint band1)
                       (owlsome hasProvidingBreakPoint band2 band3)))))

 ;; Chromosomal Band Inversion : includes both paracentric (involves
 ;; only 1 arm) and pericentric (involves both arms) inversion.
 ;; Involves only 1 chromosome
 (defclass Inversion)
 (defn inversion
   ([n band1 band2]
      (exactly n hasEvent
               (owland Inversion
                       (owlsome hasBreakPoint band1 band2)))))

 ;; Chromosomal Band Quadruplication
 ;; Note: It is not possible to indicate the orientations of the
 ;; segments with the short system!
 (defclass Quadruplication)
 (defn quadruplication
   ([n band1 band2]
      (exactly n hasEvent
               (owland Quadruplication
                       (owlsome hasBreakPoint band1 band2)))))

 ;; Chromosomal Band Translocation
 ;; Must involve more than one chromosome/band
 (defclass Translocation)

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
       (translocation3 (first bands) (getTerminal (first bands)) provide1 provide2)
       (= (count bands) 2)
       (if (= (getParent (first bands)) (getParent (second bands)))
         (translocation3 (first bands) (second bands) provide1 provide2)
         (flatten [(translocation3 (first bands) (getTerminal (first bands)) (second bands) (getTerminal (second bands)))
                   (translocation2 provide1 provide2 (rest bands))]))
       (= (count bands) 3)
       (if (or (= (getParent (first bands)) (getParent (second bands))) (= (getParent (second bands)) (getParent (second (rest bands)))))
         (if (= (getParent (first bands)) (getParent (second bands)))
           (flatten [(translocation3 (first bands) (second bands) (second (rest bands)) (getTerminal (second (rest bands)))) 
                     (translocation2 provide1 provide2 (rest (rest bands)))])
           (flatten [(translocation3 (first bands) (getTerminal (first bands)) (second bands) (second (rest bands)))
                     (translocation3 provide1 provide2 (rest bands))]))
         (flatten [(translocation2 provide1 provide2 (rest bands))
                   (translocation3 (first bands) (getTerminal (first bands)) (second bands) (getTerminal (second bands)))]))
       (> (count bands) 3)
       (if (= (getParent (first bands)) (getParent (second bands)))
         (if (= (getParent (second (rest bands))) (getParent (second (rest (rest bands)))))
           (flatten [(translocation3 (first bands) (second bands) (second (rest bands)) (second (rest (rest bands))))
                     (translocation2 provide1 provide2 (rest (rest bands)))])
           (flatten [(translocation3 (first bands) (second bands) (second (rest bands)) (getTerminal (second (rest bands))))
                     (translocation2 provide1 provide2 (rest (rest bands)))]))
         (if (= (getParent (first (rest bands))) (getParent (second (rest bands))))
           (flatten [(translocation3 (first bands) (getTerminal (first bands)) (second bands) (second (rest bands)))
                     (translocation2 provide1 provide2 (rest bands))])
           (flatten [(translocation3 (first bands) (getTerminal (first bands)) (second bands) (getTerminal (second bands)))
                     (translocation2 provide1 provide2 (rest bands))])))
       :default
       (throw
        (IllegalArgumentException.
         (str "Problem with translocation macro. Got:"
              bands))))
      ]

     ))

 ;; TODO - make it more efficient!
 (defn translocation [n chrom_no & bands]
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
                                (getTerminal (first bands))
                                bands))))))
     (throw
      (IllegalArgumentException.
       (str "There should be at least 2 band parameters. Got:"
            bands)))))

 ;; Chromosomal Band Triplication
 ;; QUERY: Book says "It is not possible to indicate the orientations
 ;; of the segments with the short system" however the example shown
 ;; seem to show the orientations fine. What other detailed systems
 ;; occur for the first example?  Similar to Duplication
 (defclass Triplication))

(as-disjoint-subclasses
 Duplication
 ;; Chromosomal Band DirectDuplication
 ;; Invovles only 1 chromosome
 (defclass DirectDuplication)
 (defn direct_duplication
   ([n band1 band2]
      (exactly n hasEvent
               (owland DirectDuplication
                       (owlsome hasBreakPoint band1 band2)))))

 ;; Chromosomal Band InverseDuplication
 ;; Invovles only 1 chromosome
 (defclass InverseDuplication)
 (defn inverse_duplication
   ([n band1 band2]
      (exactly n hasEvent
               (owland InverseDuplication
                       (owlsome hasBreakPoint band1 band2))))))

(as-disjoint-subclasses
 Insertion
 ;; Choromosomal Band DirectInsertion
 ;; Involves at most 2 chromosomes
 (defclass DirectInsertion)
 (defn direct_insertion
   ([n band1 band2 band3]
      (exactly n hasEvent
               (owland DirectInsertion
                       (owlsome hasReceivingBreakPoint band1)
                       (owlsome hasProvidingBreakPoint band2 band3)))))

 ;; Choromosomal Band InverseInsertion
 ;; Involves at most 2 chromosomes
 (defclass InverseInsertion)
 (defn inverse_insertion
   ([n band1 band2 band3]
      (exactly n hasEvent
               (owland InverseInsertion
                       (owlsome hasReceivingBreakPoint band1)
                       (owlsome hasProvidingBreakPoint band2 band3))))))

(as-disjoint-subclasses
 Triplication
 (defclass DirectTriplication)
 (defclass InverseTriplication))

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
;;   (defclass deletion)
;;   (defclass dicentric)
;;   (defclass diploid_triploid_mosaicism)
;;   (defclass diploid_triploid_tetraploid_mosaicism)
;;   (defclass distal_deletion)
;;   (defclass distal_duplication)
;;   (defclass double_ring)
;;   (defclass duplication)
;;   (defclass enlarged_satellite)
;;   (defclass fragile_site)
;;   (defclass hexsomy_mosaic)
;;   (defclass homozygosity)
;;   (defclass interstitial)
;;   (defclass interstitial_duplication)
;;   (defclass inv_dup)
;;   (defclass inv_dup_del)
;;   (defclass additional_unidentified_material)
;;   (defclass inversion)
;;   (defclass inverted_triplication)
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
