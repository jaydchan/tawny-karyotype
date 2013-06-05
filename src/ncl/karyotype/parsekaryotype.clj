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

(ns ncl.karyotype.parsekaryotype
  (:use [tawny.owl])
  (:require [tawny [reasoner :as r]]
            [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]
            [ncl.karyotype [events :as e]]
;;            [ncl.karyotype [features :as f]]
            [ncl.karyotype [named :as n]]
            [ncl.karyotype [iscnexamples :as i]]
            )
  )

(defontology parsekaryotype
  :iri "http://ncl.ac.uk/karyotype/parsekaryotype"
  :prefix "pkr:"
  )

;; import all ncl.karyotype axioms
(owlimport k/karyotype)
(owlimport h/human)
(owlimport e/events)
;; (owlimport f/features)
(owlimport n/named)

;; MACROS
;; TOFIX remove + replace when + is available
;; edit bracket replace with ! when is avaiable
(defn- make-safe [karyotype]
  "Generates the 'safe' name for the OWL class"
  (str "p"
       (clojure.string/replace
        (clojure.string/replace
         (clojure.string/replace
          karyotype
          #"[;,]" "_")
         (re-pattern "[\\(\\)]") "j")
        #"\+" "a")))

(defn- parse-int-first [s]
  "Converts the first instance of the match into an Integer from String"
  (Integer. (re-find  #"\d+" s )))

;; assume that you are unable to have +Y when youre a female
(defn- get-derived-from [karyotype]
  "Obtains the derivedFrom subclass"
  (let [value (parse-int-first (first (clojure.string/split karyotype #",")))]
    (cond
     (<= value 34)
     (if (re-find #"Y" karyotype)
       n/k23_Y
       n/k23_X)
     (and (>= value 35) (<= value 57))
     (if (re-find #"Y" karyotype)
       n/k46_XY
       n/k46_XX)
     ;; TOFIX
     (and (>= value 58) (<= value 80))
     (if (re-find #"Y" karyotype)
       n/k69_XNN
       n/k69_XXX)
     ;; TOFIX
     (and (>= value 81) (<= value 103))
     (if (re-find #"Y" karyotype)
       n/k92_XNNN
       n/k92_XXXX)
     :default
     (throw (IllegalArgumentException.
             (str "Karyotype syntax not recognised: " karyotype))))))

(defn- split-bands [bandinfo]
  "Splits bandinfo from one string to a vector of bands"
  (into [] (re-seq #"[pq][\d.]+" bandinfo)))

(defn- get-bands [chrominfo bandinfo]
  "Get the band entities inferred in bandinfo"
  (with-ontology
    ncl.karyotype.human/human
    (for [band (split-bands bandinfo)]
      (ensure-class
       (str "HumanChromosome" chrominfo "Band"
            band)))))

(defn- get-direction [bandinfo]
  "Determines the dircetion of the band range as either direct or inverse"
  (let [bands (split-bands bandinfo)
        band1 (get bands 0)
        band2 (get bands 1)]
    (cond
     ;; (or (parentband? band1) (parentchromosomeband? band1)
     ;;     (parentband? band2) (parentchromosomeband? band2))
     ;; "Unknown"
     (and (h/pband? band1) (h/qband? band2))
     "Direct"
     (and (h/qband? band1) (h/pband? band2))
     "Inverse"
     (or
      (and (h/pband? band1) (h/pband? band2))
      (and (h/qband? band1) (h/qband? band2)))
     (if (<=
          (read-string (re-find #"[\d.]+" band1))
          (read-string (re-find #"[\d.]+" band2)))
       "Direct"
       "Inverse"))))

(defn- get-inverse-function [bandinfo]
  (cond
   ;; Needed?
   ;; (= (get-direction bandinfo) "Unknown")
   ;; e/insertion
   (= (get-direction bandinfo) "Direct")
   e/direct-insertion
   (= (get-direction bandinfo) "Inverse")
   e/inverse-insertion))

(defn- define-event [event]
  (with-ontology
    ncl.karyotype.human/human
     (let [info (clojure.string/split event (re-pattern "[\\(\\)]"))]
       (cond
        ;; If event is a band addition event
        (re-find #"add" event)
        (apply e/addition 1 (get-bands (get info 1) (get info 3)))
        ;; If event is a band deletion event
        (re-find #"del" event)
        (apply e/deletion 1 (get-bands (get info 1) (get info 3)))
        ;; If event is a duplication event
        (re-find #"dup" event)
        (cond
         ;; Needed?
         ;; (= (get-direction (get info 3)) "Unknown")
         ;; (apply e/duplication 1 (get-bands (get info 1) (get info 3)))
         (= (get-direction (get info 3)) "Direct")
         (apply e/direct-duplication 1 (get-bands (get info 1) (get info 3)))
         (= (get-direction (get info 3)) "Inverse")
         (apply e/inverse-duplication 1 (get-bands (get info 1) (get info 3))))
        ;; If event is a fission event
        (re-find #"fis" event)
        (apply e/fission 1 (get-bands (get info 1) (get info 3)))
        ;; If event is an insertion event
        (re-find #"ins" event)
        (let [bands (split-bands (get info 3))
              band1 (get bands 0)
              band2n3 (str (get bands 1) (get bands 2))]
          (cond
           (re-find #"^[pq][\d.]+[pq][\d.]+[pq][\d.]+$" (get info 3))
           (apply (get-inverse-function band2n3)
                  1 (get-bands (get info 1) (get info 3)))
           (re-find #"^[pq][\d.]+;[pq][\d.]+[pq][\d.]+$" (get info 3))
           (apply (get-inverse-function band2n3)
                  1
                  (flatten
                   (conj (get-bands (re-find #"^\d+" (get info 1)) band1)
                         (get-bands (re-find #"\d+$" (get info 1)) band2n3))))))
        ;; If event is an inversion event
        (re-find #"inv" event)
        (apply e/inversion 1 (get-bands (get info 1) (get info 3)))
        ;; If event is a quadruplication event
        (re-find #"qdp" event)
        (apply e/quadruplication 1 (get-bands (get info 1) (get info 3)))
        ;; If event is a translocation event TODO
        (re-find #"t" event)
        (e/translocation 1 2 h/HumanChromosomeBand h/HumanChromosomeBand)
        ;; If event is a triplication event
        (re-find #"tri" event)
        (cond
         ;; Needed?
         ;; (= (get-direction (get info 3)) "Unknown")
         ;; (apply e/triplication 1 (get-bands (get info 1) (get info 3)))
         (= (get-direction (get info 3)) "Direct")
         (apply e/direct-triplication 1 (get-bands (get info 1) (get info 3)))
         (= (get-direction (get info 3)) "Inverse")
         (apply e/inverse-triplication 1 (get-bands (get info 1) (get info 3))))
        ;; If event is a chromosomal addition event
        (re-find #"\+" event)
        (e/addition 1 (ensure-class
                       (str "HumanChromosome"
                            (subs event 1))))
        ;; If event is a chromosomal deletion event
        (re-find #"\-" event)
        (e/deletion 1 (ensure-class
                       (str "HumanChromosome"
                            (subs event 1))))
        :default
        (throw (IllegalArgumentException.
                (str "Event syntax not recognised: " event)))))))

(defn- get-subclasses [class karyotype]
  "Obtains the other associated subclass"
  (doseq [subclass (rest (rest (clojure.string/split karyotype #",")))]
    (add-subclass class (define-event subclass))))

(defn- parse-karyotype-string [karyotype]
  "Creates OWL entity equivalent of ISCN String"
  (let [name (make-safe karyotype)]
   (tawny.read/intern-entity
    (owlclass name
               :label (str "The " karyotype " karyotype")
               :subclass i/ISCNExampleKaryotype
               (owlsome n/derivedFrom (get-derived-from karyotype))))
    (get-subclasses (ensure-class name) karyotype)))

(parse-karyotype-string "26,X,+4,+6,+21")
(parse-karyotype-string "71,XXX,+8,+10")
(parse-karyotype-string "89,XXYY,-1,-3,-5,+8,-21")
(parse-karyotype-string "47,XX,+X")
(parse-karyotype-string "45,X,-X")
(parse-karyotype-string "45,X,-Y")
(parse-karyotype-string "45,Y,-X")
(parse-karyotype-string "46,XX,add(19)(p13.3)")
(parse-karyotype-string "46,XY,add(12)(q13)")
(parse-karyotype-string "46,XX,del(5)(q13)")
(parse-karyotype-string "46,XX,del(5)(q13q33)")
(parse-karyotype-string "46,XX,del(5)(q13q13)")
(parse-karyotype-string "46,Y,del(X)(p21p21)")
(parse-karyotype-string "46,XX,dup(1)(q22q25)")
(parse-karyotype-string "46,XY,dup(1)(q25q22)")
(parse-karyotype-string "47,XY,-10,+fis(10)(p10),+fis(10)(q10)")
(parse-karyotype-string "46,XX,ins(2)(p13q21q31)")
(parse-karyotype-string "46,XY,ins(2)(p13q31q21)")
(parse-karyotype-string "46,XY,ins(5;2)(p14;q22q32)")
(parse-karyotype-string "46,XY,ins(5;2)(p14;q32q22)")
(parse-karyotype-string "46,XX,ins(5;2)(q31;p13p23)")
(parse-karyotype-string "46,XX,ins(5;2)(q31;p23p13)")
(parse-karyotype-string "46,XX,inv(3)(q21q26.2)")
(parse-karyotype-string "46,XY,inv(3)(p13q21)")
(parse-karyotype-string "46,XX,qdp(1)(q23q32)")
(parse-karyotype-string "46,XX,trp(1)(q21q32)")
;; (parse-karyotype-string "46,XX,inv trp(1)(q32q21)")


(defn- create-karyotype-string [name]
  ;; (ensure-class (make-safe name))
  (println (str "TODO: " class)))

;; (create-karyotype-string "45,X,-X")