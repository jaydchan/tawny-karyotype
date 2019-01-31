;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2012-2019, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see http://www.gnu.org/licenses/.

(ns ^{:doc "Base karyotype information for human karyotypes."
      :author "Jennifer Warrender"}
  ncl.karyotype.core
  (:use [ncl.karyotype.generic :only [save-ontology]])
  (:require [ncl.karyotype karyotype human resolutions events features
             base named iscnexamples random ;; parse
             generate_iscnexamples_test])
  (:gen-class))

(defn -main
  "Save ontologies in .omn and .owl format"
  [& args]

  (save-ontology ncl.karyotype.karyotype/karyotype "karyotype.omn" :omn)
  (save-ontology ncl.karyotype.karyotype/karyotype "karyotype.owl" :owl)

  (save-ontology ncl.karyotype.human/human "human.omn" :omn)
  (save-ontology ncl.karyotype.human/human "human.owl" :owl)

  (save-ontology ncl.karyotype.resolutions/resolutions "resolutions.omn" :omn)
  (save-ontology ncl.karyotype.resolutions/resolutions "resolutions.owl" :owl)

  (save-ontology ncl.karyotype.events/events "events.omn" :omn)
  (save-ontology ncl.karyotype.events/events "events.owl" :owl)

  (save-ontology ncl.karyotype.features/features "features.omn" :omn)
  (save-ontology ncl.karyotype.features/features "features.owl" :owl)

  (save-ontology ncl.karyotype.base/base "base.omn" :omn)
  (save-ontology ncl.karyotype.base/base "base.owl" :owl)

  (save-ontology ncl.karyotype.named/named "named.omn" :omn)
  (save-ontology ncl.karyotype.named/named "named.owl" :owl)

  (save-ontology
   ncl.karyotype.iscnexamples/iscnexamples "iscnexamples.omn" :omn)
  (save-ontology
   ncl.karyotype.iscnexamples/iscnexamples "iscnexamples.owl" :owl)

  ;; (save-ontology ncl.karyotype.parse/parse "parse.omn" :omn)
  ;; (save-ontology ncl.karyotype.parse/parse "parse.owl" :owl)

  (save-ontology ncl.karyotype.random/random "random.omn" :omn)
  (save-ontology ncl.karyotype.random/random "random.owl" :owl)
)
