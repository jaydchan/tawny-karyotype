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
(defoproperty hasEvent
  :range Event
  :domain k/Karyotype)

(defoproperty hasBreakPoint
  :range k/ChromosomeBand
  :domain k/Karyotype)

(defoproperty hasReceivingBreakPoint
  :subpropertyof hasBreakPoint)

(defoproperty hasProvidingBreakPoint
  :subpropertyof hasBreakPoint)



;; define all the events
;; TODO define restrictions
;; TODO Subclasses of each event i.e. inversion insertion

;; Chromosomal Addition : exactly <#> hasEvent (owland Addition <HumanChromsome>)
;; Chromosomal Band Addition : exactly <#> hasEvent (owland Addition (owlsome hasBreakPoint <HumanChromosomeBand>)
(defclass Addition
  :subclass Event)

(defn addition
  ([n chrom_band]
     (with-ontology
       ncl.karyotype.human/human
       (cond
        (subclass? h/HumanChromosome chrom_band)
        (exactly n hasEvent (owland Addition chrom_band))
        (subclass? h/HumanChromosomeBand chrom_band)
        (exactly n hasEvent (owland Addition (owlsome hasBreakPoint chrom_band)))
        :default
        (throw
         (IllegalArgumentException.
          (str "Deletion expects a Chromosome or ChromosomeBand. Got:"
               chrom_band))))))
  )

;; Chromosomal Deletion : exactly <#> hasEvent (owland Deletion <HumanChromsome>)
;; Chromosomal Band Deletion : exactly <#> hasEvent (owland Deletion (owlsome hasBreakPoint <HumanChromosomeBand> <HumanChromosomeBand>))
;; Terminal deletion with a break : in <HumanChromosomeBand> (aka HumanChromosomeBand && qTer) OR Interstitial deletion with breakage and reuinion (::) of bands <HumanChromosome>x2 (If equivalent then just state 1)
;; Invovles only 1 chromosome
(defclass Deletion
  :subclass Event)

;; TOFIX
;; NOTE Cannot use superclass? for Bands as it does not know to handle isBandOf ObjectProperty
(defn deletion
  ([n chrom_band]
     (with-ontology
       ncl.karyotype.human/human
       (cond
        (subclass? h/HumanChromosome chrom_band)
        (exactly n hasEvent (owland Deletion chrom_band))
        (subclass? h/HumanChromosomeBand chrom_band)
        (exactly n hasEvent (owland Deletion (owlsome hasBreakPoint chrom_band)))
        :default
        (throw
         (IllegalArgumentException.
          (str "Deletion expects a Chromosome or ChromosomeBand. Got:"
               chrom_band))))))
  ([n band1 band2]
     ;; if band1 == band2 
     (exactly n hasEvent (owland Deletion (owlsome hasBreakPoint band1 band2))))
  )

;; Can be preceeded by the triplets dir or inv to indicate direct or inverted direction
;; There shouldn't be any of this type
(defclass Duplication
  :subclass Event)

;; Chromosomal Band ForwardDuplication : exactly <#> hasEvent (owland ForwardDuplication (owlsome hasBreakPoint <HumanChromosomeBand>))
;; Invovles only 1 chromosome
(defclass DirectDuplication
  :subclass Duplication)

(defn direct_duplication
  ([n band1 band2]
     (exactly 1 hasEvent (owland DirectDuplication (owlsome hasBreakPoint band1 band2)))))

;; Chromosomal Band InverseDuplication : exactly <#> hasEvent (owland InverseDuplication (owlsome hasBreakPoint <HumanChromosomeBand>))
;; Invovles only 1 chromosome
(defclass InverseDuplication
  :subclass Duplication)

(defn inverse_duplication
  ([n band1 band2]
     (exactly 1 hasEvent (owland InverseDuplication (owlsome hasBreakPoint band1 band2)))))

;; Chromosomal Band Fission : exactly <#> hasEvent (owland Fission (owlsome hasBreakPoint <HumanChromosome>))
;; AKA Centric fission - break in the centromere
;; Involves only 1 chromosome
;; QUERY: always come in 2's?
(defclass Fission
  :subclass Event)

;;TOFIX
(defn fission
  ([n band1 band2]
     (exactly 1 hasEvent (owland Fission (owlsome hasBreakPoint band1 band2))))
  
  ;; Input chromsome
  ;; ([n chromosome] (list (deletion n chromosome) (fission n pter cen) (fission n qter cen)))
  )

;; Can be preceeded by the triplets dir or inv to indicate direct or inverted direction
;; Rules: p only/ q only (big to small) = Direct insertion
;; QUERY: How do we classify ins(1)(p13p11q21)? Direct or Inverse?
;; There shouldn't be any of this type
(defclass Insertion
  :subclass Event)

;; TOFIX IS it possible to automatically assign whether the insertion is direct or inverse?
;; (defn insertion
;;   ([n band1 band2]))

;; Choromosomal Band ForwardInsertion: exactly <#> hasEvent (owland ForwardInsertion (owlsome hasRecievingBreakPoint <HumanChromosomeBand>) (owlsome hasProvidingBreakPoint <HumanChromosomeBand> <HumanChromosomeBand>))
;; Involves at most 2 chromosomes
(defclass DirectInsertion
  :subclass Insertion)

(defn direct_insertion
  ([n band1 band2 band3]
     (exactly 1 hasEvent (owland DirectInsertion (owlsome hasReceivingBreakPoint band1) (owlsome hasProvidingBreakPoint band2 band3)))))

;; Choromosomal Band InverseInsertion: exactly <#> hasEvent (owland InverseInsertion (owlsome hasRecievingBreakPoint <HumanChromosomeBand>) (owlsome hasProvidingBreakPoint <HumanChromosomeBand> <HumanChromosomeBand>))
;; Involves at most 2 chromosomes
(defclass InverseInsertion
  :subclass Insertion)

(defn inverse_insertion
  ([n band1 band2 band3]
     (exactly 1 hasEvent (owland InverseInsertion (owlsome hasReceivingBreakPoint band1) (owlsome hasProvidingBreakPoint band2 band3)))))

;; Chromosomal Band Inversion: exactly <#> hasEvent (owland Inversion (owlsome hasBreakPoint <HumanChromosomeBand> <HumanChromosomeBand>))
;; Involves both paracentric (involves only 1 arm) and pericentric (involves both arms) inversion
;; Involves only 1 chromosome
(defclass Inversion
  :subclass Event)

(defn inversion
  ([n band1 band2]
     (exactly n hasEvent (owland Inversion (owlsome hasBreakPoint band1 band2))))) 

;; TOFIX: It is not possible to indicate the orientations of the segments with the short system!
(defclass Quadruplication
  :subclass Event)

(defn quadruplication
  ([n band1 band2]
     (exactly 1 hasEvent (owland Quadruplication (owlsome hasBreakPoint band1 band2))))) 

;; TODO
(defclass Translocation
  :subclass Event)

;; (defn translocation
;;   ([n band1 band2 band3 band4 band5 band6]
;;      (exactly 1 e/hasEvent
;;               (owland e/Translocation
;;                       (owland
;;                        (owlsome e/hasProvidingBreakPoint band1)
;;                        (owlsome e/hasProvidingBreakPoint band2)
;;                        (owlsome e/hasReceivingBreakPoint band3))
;;                       (owland
;;                        (owlsome e/hasProvidingBreakPoint band3)
;;                        (owlsome e/hasProvidingBreakPoint band4)
;;                        (owlsome e/hasReceivingBreakPoint band5))
;;                       (owland
;;                        (owlsome e/hasProvidingBreakPoint band5)
;;                        (owlsome e/hasProvidingBreakPoint band6)
;;                        (owlsome e/hasReceivingBreakPoint band1))))))

;; QUERY: Book says "It is not possible to indicate the orientations of the segments with the short system" however the example shown seem to show the orientations fine. What other detailed systems occur for the first example?
;; Similar to Duplication
(defclass Triplication
  :subclass Event)
(defclass DirectTriplication
  :subclass Triplication)
(defclass InverseTriplication
  :subclass Triplication)





;; Potential new disorder.clj file
;; (defclass Disorder)

;; define object properties
;; (defoproperty hasDisorder
;;   :range Disorder
;;   :domain k/Karyotype
;;   )

;; Information from Registered Chromosome Disorders http://www.rarechromo.co.uk/html/bychromo.asp
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
