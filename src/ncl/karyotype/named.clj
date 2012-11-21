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


(ns ncl.karyotype.named
  (:use [owl.owl])
  (:require [owl [reasoner :as r]]
            [ncl.karyotype [karyotype :as k]]
            ))

(defontology named
  :file "named.omn"
  :iri "http://ncl.ac.uk/karyotype/named"
  :prefix "nmd:")

(defclass NamedKaryotype
  :subclass k/Karyotype
  )

;; define object properties
(defoproperty derivedFrom
  :range NamedKaryotype
  :domain NamedKaryotype
  )

;; ;;function to define all the named karyotypes
;; (defn namedkaryotypes [karyotypes]

;;   :name "";;karyotype with no k""
;;   :label "The ";;karyotype with no k" karyotype"
;;   :subclass NamedKaryotype
;;   )

;; (namedkaryotypes
;;  NamedKaroytype
;;  ;;we have to pass these in as strings because they start with integers which brings up an NumberFormatException
;;  "46_XX"
;;  "46_XY"
;;  "46_XO")
  
;; define all the namedKaryotypes
(as-disjoint-subclasses
 NamedKaryotype
 (defclass k46_XX
   :name "46_XX")
 (defclass k46_XY
   :label "The 46,XY karyotype")
  ;; (defclass 46_XO)
  ;; (defclass 45_X)
  ;; (defclass 45_Y)
  ;; (defclass 46_YY)
  ;; (defclass 47_XXX)
  ;; (defclass 47_XXY)
  ;; (defclass 47_XYY)
  ;; (defclass 47_YYY)
  ;; (defclass 48_XXXX)
  ;; (defclass 48_XXXY)
  ;; (defclass 48_XXYY)
  ;; (defclass 48_XYYY)
  ;; (defclass 48_YYYY)
  ;; (defclass 49_XXXXX)
  ;; (defclass 49_XXXXY)
  ;; (defclass 49_XXXYY)
  ;; (defclass 49_XXYYY)
  ;; (defclass 49_XYYYY)
  ;; (defclass 49_YYYYY)
  ;; (defclass 26_X_+4_+6_+21)
  ;; (defclass 71_XXX_+8_+10)
  ;; (defclass 89_XXYY_-1_-3_-5_+8_-21)
  ;; (defclass 47_XX_+X)
  ;; (defclass 45_X_-X)
  ;; (defclass 45_X_-Y)
  ;; (defclass 45_Y_-X)
  ;; (defclass 48_XY_+X_+Y)
  ;; (defclass 48_XXYc_+X)
  ;; (defclass 46_Xc_+X)
  ;; (defclass 46_XXYc_-X)
  ;; (defclass 44_Xc_-X)
  ;; (defclass 46_Xc_+21)
  ;; (defclass 47_XX_+21)
  ;; (defclass 48_XX_+13_+21)
  ;; (defclass 45_XX_-22)
  ;; (defclass 46_XX_+8_-21)
  ;; (defclass 48_XY_+21c_+21)
  ;; (defclass 46_XY_+21c_-21)
 )

