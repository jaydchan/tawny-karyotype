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
            [ncl.karyotype [features :as f]]
            [ncl.karyotype [named :as n]]
            [ncl.karyotype [iscnexamples :as i]]))

(defontology parsekaryotype
  :iri "http://ncl.ac.uk/karyotype/parsekaryotype"
  :prefix "pkr:")

;; import all ncl.karyotype axioms
(owl-import k/karyotype)
(owl-import h/human)
(owl-import e/events)
(owl-import f/features)
(owl-import n/named)

;; FUNCTIONS
(defn- make-safe
  "Returns a 'safe' string name for the OWL class.
karyotype is of type String."
  [karyotype]
  (str "k"
       (clojure.string/replace
        (clojure.string/replace
         karyotype
         #"[;,\+ \?]" "_")
        (re-pattern "[\\(\\)]") "_")))

(defn- get-no-of-Y
  "Determines how many Y chromosomes exist in karyotype.
s id of type String."
  [s]
  (- (count (re-seq #"Y" s))
     (count (re-seq #"\+[a-zA-Z0-Z(;]*Y" s))))

;; assume that you are unable to have +Y when youre a female
(defn- get-derived-from
  "Obtains the derivedFrom subclass"
  [karyotype]
  (let [value (read-string (first (clojure.string/split karyotype #",")))]
    (cond
     ;; If karyotype is near-haploid
     (<= value 34)
     (if (re-find #"Y" karyotype)
       n/k23_Y
       n/k23_X)
     ;; If karyotype is near-diploid
     (and (>= value 35) (<= value 57))
     (if (re-find #"Y" karyotype)
       n/k46_XY
       n/k46_XX)
     ;; If karyotype is near-triploid
     (and (>= value 58) (<= value 80))
     (let [Yno (get-no-of-Y karyotype)]
       (cond
        (= Yno 0)
        n/k69_XXX
        (= Yno 1)
        n/k69_XXY
        (> Yno 1)
        n/k69_XYY))
     ;; If karyotype is near-tetraploid
     (and (>= value 81) (<= value 103))
     (let [Yno (get-no-of-Y karyotype)]
       (cond
        (= Yno 0)
        n/k92_XXXX
        (= Yno 1)
        n/k92_XXXY
        (= Yno 2)
        n/k92_XXYY
        (> Yno 2)
        n/k92_XYYY))
     :default
     (throw (IllegalArgumentException.
             (str "Karyotype syntax not recognised: " karyotype))))))

(defn- split-bands
  "Splits bandinfo from one string to a vector of bands"
  [bandinfo]
  (into [] (re-seq #"[pq][\d\.]+|[pq]\?|\?" bandinfo)))

(defn- get-bands
  "Get the band entities inferred in bandinfo"
  [chrominfo bandinfo]
  (for [band (split-bands bandinfo)]
    (owl-class
     ncl.karyotype.human/human
     (cond
      (and (= "?" band) (= "?" chrominfo))
      "HumanChromosomeBand"
      (and (= "?" band) (re-find #"\d+" chrominfo))
      (str "HumanChromosome" chrominfo "Band")
      (= "p?" band)
      (str "HumanChromosome" chrominfo "Bandp")
      (= "q?" band)
      (str "HumanChromosome" chrominfo "Bandq")
      (re-find #"[pq][\d\.]+" band)
      (str "HumanChromosome" chrominfo "Band" band)))))

(defn- get-direction
  "Determines the direction of the band range as either direct or inverse"
  [bandinfo]
  (let [band1 (get (split-bands bandinfo) 0)
        band2 (get (split-bands bandinfo) 1)]
    (cond
     (or (= "?" band1) (= "?" band2))
     "Unknown"
     (and (h/pband? band1) (h/qband? band2))
     "Direct"
     (and (h/qband? band1) (h/pband? band2))
     "Inverse"
     (or
      (and (h/pband? band1) (h/pband? band2))
      (and (h/qband? band1) (h/qband? band2)))
     (let [digit1 (re-find #"[\d\.]+" band1)
           digit2 (if (nil? band2) nil (re-find #"[\d\.]+" band2))]
       (cond
        (nil? (or digit1 digit2))
        "Unknown"
        (<= (read-string digit1) (read-string digit2))
        "Direct"
        (> (read-string digit1) (read-string digit2))
        "Inverse")))))

(defn- get-insertion-function
  "TODO"
  [bandinfo]
  (cond
   (= (get-direction bandinfo) "Unknown")
   e/insertion
   (= (get-direction bandinfo) "Direct")
   e/direct-insertion
   (= (get-direction bandinfo) "Inverse")
   e/inverse-insertion))

(defn- define-event
  "TODO"
  [event]
  (with-ontology
    ncl.karyotype.human/human
     (let [info (clojure.string/split event (re-pattern "[\\(\\)]"))]
       (cond
        ;; If event is a band addition event
        (re-find (re-pattern "add\\(") event)
        (apply e/addition 1 (get-bands (get info 1) (get info 3)))
        ;; If event is a band deletion event
        (re-find (re-pattern "del\\(") event)
        (apply e/deletion 1 (get-bands (get info 1) (get info 3)))
        ;; If event is a duplication event
        (re-find (re-pattern "dup\\(") event)
        (cond
         (= (get-direction (get info 3)) "Unknown")
         (apply e/duplication 1 (get-bands (get info 1) (get info 3)))
         (= (get-direction (get info 3)) "Direct")
         (apply e/direct-duplication 1 (get-bands (get info 1) (get info 3)))
         (= (get-direction (get info 3)) "Inverse")
         (apply e/inverse-duplication 1 (get-bands (get info 1) (get info 3))))
        ;; If event is a fission event
        (re-find (re-pattern "fis\\(") event)
        (apply e/fission 1 (get-bands (get info 1) (get info 3)))
        ;; If event is an insertion event
        (re-find (re-pattern "ins\\(") event)
        (let [bands (split-bands (get info 3))
              band1 (get bands 0)
              band23 (str (get bands 1) (get bands 2))]
           (if (re-find #";" (get info 3))
             (apply (get-insertion-function band23)
                    1
                    (flatten
                     (conj (get-bands (re-find #"\d+$|\?$" (get info 1))
                                      (if (= "?" band23)
                                        (str band23 band23)
                                        band23))
                           (get-bands (re-find #"^\d+|^\?" (get info 1))
                                      band1))))
             (apply (get-insertion-function band23)
                    1 (get-bands (get info 1) (get info 3)))))
        ;; If event is an inversion event
        (re-find (re-pattern "inv\\(") event)
        (apply e/inversion 1 (get-bands (get info 1) (get info 3)))
        ;; If event is a quadruplication event
        (re-find (re-pattern "qdp\\(") event)
        (apply e/quadruplication 1 (get-bands (get info 1) (get info 3)))
        ;; If event is a translocation event
        (re-find (re-pattern "t\\(") event)
        (let [chrominfo (clojure.string/split (get info 1) #";")
              bandinfo (clojure.string/split (get info 3) #";")]
          (apply e/translocation
                 1
                 (for [i (range (count chrominfo))]
                   (into [] (get-bands (get chrominfo i) (get bandinfo i))))))
        ;; If event is a triplication event
        (re-find (re-pattern "trp\\(") event)
        (cond
         (= (get-direction (get info 3)) "Unknown")
         (apply e/triplication 1 (get-bands (get info 1) (get info 3)))
         (= (get-direction (get info 3)) "Direct")
         (apply e/direct-triplication 1 (get-bands (get info 1) (get info 3)))
         (= (get-direction (get info 3)) "Inverse")
         (apply e/inverse-triplication 1 (get-bands (get info 1) (get info 3))))
        ;; If event is a chromosomal addition event
        (re-find #"\+" event)
        (e/addition 1 (owl-class
                       (str "HumanChromosome"
                            (subs event 1))))
        ;; If event is a chromosomal deletion event
        (re-find #"\-" event)
        (e/deletion 1 (owl-class
                       (str "HumanChromosome"
                            (subs event 1))))
        :default
        (throw (IllegalArgumentException.
                (str "Event syntax not recognised: " event)))))))

(defn- get-subclasses
  "Obtains the other associated subclass"
  [class karyotype]
  (doseq [subclass (rest (rest (clojure.string/split karyotype #",")))]
    (add-subclass class (define-event subclass))))

(defn- parse-karyotype-string
  "Creates OWL entity equivalent of ISCN String"
  [karyotype]
  (let [name (make-safe karyotype)]
   (tawny.read/intern-entity
    (owl-class name
               :label (str "The " karyotype " karyotype")
               :subclass i/ISCNExampleKaryotype
               (if-not (re-find #"c" karyotype)
                 (owl-some n/derivedFrom (get-derived-from karyotype)))))
    (get-subclasses (owl-class name) karyotype)))

;; define karyotypes
(parse-karyotype-string "26,X,+4,+6,+21")
(parse-karyotype-string "71,XXX,+8,+10")
(parse-karyotype-string "89,XXYY,-1,-3,-5,+8,-21")
(parse-karyotype-string "47,XX,+X")
(parse-karyotype-string "45,X,-X")
(parse-karyotype-string "45,X,-Y")
(parse-karyotype-string "45,Y,-X")
(parse-karyotype-string "69,XXX,del(7)(p11.2)")
(parse-karyotype-string "46,XX,ins(1;?)(p22;?)")
(parse-karyotype-string "46,XX,add(19)(p13.3)")
(parse-karyotype-string "46,XY,add(12)(q13)")
(parse-karyotype-string "46,XX,ins(5;?)(q13;?)")
(parse-karyotype-string "46,XX,del(5)(q13)")
(parse-karyotype-string "46,XX,del(5)(q13q33)")
(parse-karyotype-string "46,XX,del(5)(q13q13)")
(parse-karyotype-string "46,XX,del(5)(q?)")
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
(parse-karyotype-string "46,XY,t(2;5)(q21;q31)")
(parse-karyotype-string "46,XY,t(2;5)(p12;q31)")
(parse-karyotype-string "46,X,t(X;13)(q27;q12)")
(parse-karyotype-string "46,XX,t(2;7;5)(p21;q22;q23)")
(parse-karyotype-string "46,X,t(X;22;1)(q24;q11.2;p33)")
(parse-karyotype-string "46,XX,t(3;9;22;21)(p13;q34;q11.2;q21)")
(parse-karyotype-string "46,XY,t(5;6)(q13q23;q15q23)")
(parse-karyotype-string "46,XX,t(5;14;9)(q13q23;q24q21;p12p23)")
(parse-karyotype-string "46,XY,t(1;3)(p10;q10)")
(parse-karyotype-string "46,XY,t(1;3)(p10;p10)")
(parse-karyotype-string "46,XX,trp(1)(q21q32)")
(parse-karyotype-string "46,XX,inv trp(1)(q32q21)")


;; CREATE KARYOTYPE STRING FUNCTIONS
;; TOFIX - Not true!
(defn- get-start
  "Returns the prefix of the karyotype.
axiom is of type derivedFrom."
  [axiom]
  (let [base (re-find #"k[\d_XY]+" axiom)]
    (clojure.string/replace (subs base 1) #"_" ",")))

(defn- sort-chromosomes
  "Returns a sorted vector of restrictions - i.e. hasDerived
  restriction, and other restrictions which are sorted by chromosome
  value (X,Y,1-22).
class is of type ISCNExampleKaryotype."
  [class]
  (into [] (superclasses class)))

(defn- get-event-string
  "Returns the String representation of the event restriction.
axiom is of type hasEvent."
  [axiom]
  (cond
   ;; "If axiom is an addition event"
   (re-find #"Addition" axiom)
   (let [chrom-band (re-find #"HumanChromosome[\dXYBandpq\.]+" axiom)]
     (if (re-find #"Band" chrom-band)
       (str ",add(" (re-find #"[XY]|\d+" chrom-band)
            ")(" (subs (re-find #"Band[pq\d\.]+" chrom-band) 4) ")")
       (str ",+" (re-find #"[XY]|\d+" chrom-band))))
   ;; "If axiom is a deletion event"
   ;; TOHERE - doseq does not save the string
   (re-find #"Deletion" axiom)
   (let [chrom-band (re-seq #"HumanChromosome[\dXYBandpq\.]+" axiom)]
     (if (re-find #"Band" (first chrom-band))
       (str ",del(" (re-find #"[XY]|\d+" (first chrom-band)) ")("
            (doseq [band chrom-band]
              (subs (re-find #"Band[pq\d\.]+" band) 4)) ")")
       (str ",-" (re-find #"[XY]|\d+" (first chrom-band)))))
   ;; "If axiom is a duplication event"
   (re-find #"Duplication" axiom)
   ",dup()()"
   ;; (str "dup(" (get-chromosome axiom) ")(" (get-band axiom) ")"
   ;; "If axiom is a fission event"
   (re-find #"Fission" axiom)
   ",fis()()"
   ;; (str "fis(" (get-chromosome axiom) ")(" (get-band axiom) ")"
   ;; "If axiom is an insertion event"
   (re-find #"Insertion" axiom)
   ",ins()()"
   ;; (str "ins(" (get-chromosome axiom) ")(" (get-band axiom) ")"
   ;; "If axiom is an inversion  event"
   (re-find #"Inversion" axiom)
   ",inv()()"
   ;; (str "inv(" (get-chromosome axiom) ")(" (get-band axiom) ")"
   ;; "If axiom is a quadruplication event"
   (re-find #"Quadruplication" axiom)
   ",qdp()()"
   ;; (str "qdp(" (get-chromosome axiom) ")(" (get-band axiom) ")"
   ;; "If axiom is a translocation event"
   (re-find #"Translocation" axiom)
   ",t()()"
   ;; (str "t(" (get-chromosome axiom) ")(" (get-band axiom) ")"
   ;; "If axiom is a triplication event"
   (re-find #"Triplication" axiom)
   ",trp()()"
   ;; (str "trp(" (get-chromosome axiom) ")(" (get-band axiom) ")"
))

(defn- get-axiom-string
  "Returns the string representation of the restriction.
axiom is of type OWL Object Property."
  [axiom]
  (cond
   (re-find #"derivedFrom" axiom)
   (get-start axiom)
   (re-find #"hasEvent" axiom)
   (get-event-string axiom)))

(defn- parse-karyotype-class
  "Returns the ISCN String of an OWL Karyotype Class.
Class is of type ISCNExampleKaryotype."
  [class]
  (let [s (apply str (for [superclass (sort-chromosomes class)
                           :let [axiom (get-axiom-string (str superclass))]
                           :when (complement (nil? axiom))]
                       axiom))]
    s))

(defn- create-karyotype-string0
  "Prints details of the string input - used for testing purposes.
detail is of type boolean.
name is of type String."
  [detail name]
  (let [class (owl-class (make-safe name))]
    ;; If true, print the name, owl class, and resulting string.
    (if (true? detail)
      [(println (str "NAME: " name))
      (println (str "CLASS: " class))
      (println (str "STRING: " (parse-karyotype-class class)))])))

;; get ISCN String
(def create-karyotype-string (partial create-karyotype-string0 false))
(create-karyotype-string "26,X,+4,+6,+21")
(create-karyotype-string "71,XXX,+8,+10")
(create-karyotype-string "89,XXYY,-1,-3,-5,+8,-21")
(create-karyotype-string "47,XX,+X")
(create-karyotype-string "45,X,-X")
(create-karyotype-string "45,X,-Y")
(create-karyotype-string "45,Y,-X")
(create-karyotype-string "46,XX,add(19)(p13.3)")
(create-karyotype-string "46,XX,del(5)(q13)")
(create-karyotype-string "46,XX,del(5)(q13q33)")
(create-karyotype-string "46,XX,del(5)(q13q13)")

(def create-karyotype-string (partial create-karyotype-string0 true))
(create-karyotype-string "26,X,+4,+6,+21")
