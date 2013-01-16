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
  ;; :range Event
  :domain k/Karyotype
  )

(defoproperty effects
  :range k/ChromosomeBand
  :domain k/Karyotype
  )

(defoproperty hasBreakPoint
  :range k/ChromosomeBand
  :domain k/Karyotype
  )

;; define all the events
;; TODO define restrictions
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
  (defclass Triplication)
)

;; Potential new feature.clj file
;; (defclass Feature)

;; define object properties
;; (defoproperty hasFeature
;;   :range Feature
;;   :domain k/Karyotype
;;   )

;; define all the structural features
;; (as-disjoint-subclasses
;;  Feature
;;   (defclass derivative_chromosome)
;;   (defclass dicentric_chromosome)
;;   (defclass fragile_sites)
;;   (defclass homogeneously_staining_region)
;;   (defclass isoderiviative_chromosome)
;;   (defclass isochromosomes)
;;   (defclass isodicentric_chromosome)
;;   (defclass marker_chromosome)
;;   (defclass neocentromere)
;;   (defclass pseudodicentric_chromosome)
;;   (defclass pseudoisodicentric_chromosome)
;;   (defclass recombiant_chromosome)
;;   (defclass ring_chromosome)
;;   (defclass telomeric_associations)
;;   (defclass tricentric_ring_chromosome)
;; )


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
;;   (defclass xxx)
;;   (defclass xxxx)
;;   (defclass xxxxx)
;;   (defclass xxxxxy)
;;   (defclass xxxxy)
;;   (defclass xxxy)
;;   (defclass xxxyy)
;;   (defclass xxy)
;;   (defclass xy_female)
;;   (defclass xyy)
;;   (defclass xyyy)
;;   (defclass xyyyy)
;; )
