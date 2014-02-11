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

(ns ^{:doc "Translation of OWL karyotype classes to string and vice
versa. Limitation - only available for addition and deletion events."
      :author "Jennifer Warrender"}
  ncl.karyotype.parse
  (:use [tawny.owl])
  (:require [ncl.karyotype [karyotype :as k]]
            [ncl.karyotype [human :as h]]
            [ncl.karyotype [base :as b]]
            [ncl.karyotype [events :as e]]
            [ncl.karyotype [features :as f]]
            [ncl.karyotype [iscnexamples :as i]]
            [tawny [render :as r]]))

(defontology parse
  :iri "http://ncl.ac.uk/karyotype/parse"
  :prefix "par:"
  :comment "'Parse' ontology for Human Karyotype Ontology, written
  using the tawny-owl library")

;; FUNCTIONS
(defn- make-safe [karyotype]
  "Returns a 'safe' string name for the OWL class.
karyotype is of type String."
  (str "k"
       (clojure.string/replace
        (clojure.string/replace
         karyotype
         #"[;,\+ \?]" "_")
        (re-pattern "[\\(\\)]") "_")))

(defn- get-no-of-Y [s]
  "Determines how many Y chromosomes exist in karyotype.
s id of type String."
  (- (count (re-seq #"Y" s))
     (count (re-seq #"\+[a-zA-Z0-Z(;]*Y" s))))

;; assume that you are unable to have +Y when youre a female
(defn- get-derived-from [karyotype]
  "Obtains the derivedFrom subclass"
  (let [value (read-string (first (clojure.string/split karyotype #",")))]
    (cond
     ;; If karyotype is near-haploid
     (<= value 34)
     (if (re-find #"Y" karyotype)
       b/k23_Y
       b/k23_X)
     ;; If karyotype is near-diploid
     (and (>= value 35) (<= value 57))
     (if (re-find #"Y" karyotype)
       b/k46_XY
       b/k46_XX)
     ;; If karyotype is near-triploid
     (and (>= value 58) (<= value 80))
     (let [Yno (get-no-of-Y karyotype)]
       (cond
        (= Yno 0)
        b/k69_XXX
        (= Yno 1)
        b/k69_XXY
        (> Yno 1)
        b/k69_XYY))
     ;; If karyotype is near-tetraploid
     (and (>= value 81) (<= value 103))
     (let [Yno (get-no-of-Y karyotype)]
       (cond
        (= Yno 0)
        b/k92_XXXX
        (= Yno 1)
        b/k92_XXXY
        (= Yno 2)
        b/k92_XXYY
        (> Yno 2)
        b/k92_XYYY))
     :default
     (throw (IllegalArgumentException.
             (str "Karyotype syntax not recognised: " karyotype))))))

(defn- split-bands [bandinfo]
  "Splits bandinfo from one string to a vector of bands"
  (into [] (re-seq #"[pq][\d\.]+|[pq]\?|\?" bandinfo)))

(defn- get-bands [chrominfo bandinfo]
  "Get the band entities inferred in bandinfo"
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

(defn- get-direction [bandinfo]
  "Determines the direction of the band range as either direct or inverse"
  (let [band1 (get (split-bands bandinfo) 0)
        band2 (get (split-bands bandinfo) 1)]
    (cond
     (or (= "?" band1) (= "?" band2))
     "Unknown"
     (and (h/str-pband? band1) (h/str-qband? band2))
     "Direct"
     (and (h/str-qband? band1) (h/str-pband? band2))
     "Inverse"
     (or
      (and (h/str-pband? band1) (h/str-pband? band2))
      (and (h/str-qband? band1) (h/str-qband? band2)))
     (let [digit1 (re-find #"[\d\.]+" band1)
           digit2 (if (nil? band2) nil (re-find #"[\d\.]+" band2))]
       (cond
        (nil? (or digit1 digit2))
        "Unknown"
        (<= (read-string digit1) (read-string digit2))
        "Direct"
        (> (read-string digit1) (read-string digit2))
        "Inverse")))))

(defn- get-insertion-function [bandinfo]
  "Returns "
  (cond
   (= (get-direction bandinfo) "Unknown")
   e/insertion
   (= (get-direction bandinfo) "Direct")
   e/direct-insertion
   (= (get-direction bandinfo) "Inverse")
   e/inverse-insertion))

(defn- define-event [event]
  "Returns "
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

(defn- get-subclasses [class karyotype]
  "Obtains the other associated subclass"
  (doseq [subclass (rest (rest (clojure.string/split karyotype #",")))]
    (add-subclass class (define-event subclass))))

(defn- parse-karyotype-string [karyotype]
  "Creates OWL entity equivalent of ISCN String"
  (let [name (make-safe karyotype)]
    (tawny.read/intern-entity
     (owl-class name
                :label (str "The " karyotype " karyotype")
                :subclass i/ISCNExampleKaryotype
                (if-not (re-find #"c" karyotype)
                  (owl-some b/derivedFrom (get-derived-from karyotype)))))
    (get-subclasses (owl-class name) karyotype)))

;; TESTING
;; import all ncl.karyotype axioms
(owl-import k/karyotype)
(owl-import h/human)
(owl-import e/events)
(owl-import f/features)
(owl-import b/base)

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
;; Auxiliary functions
(defn- clean-up [clazz]
  (let [s (second (clojure.string/split (str (r/form clazz)) #"/"))]
    (cond
     (subclass? b/base b/BaseKaryotype clazz)
     (clojure.string/replace s #"k" "")
     (h/chromosome? clazz)
     (clojure.string/replace s #"HumanChromosome" "")
     (h/band? clazz)
     (clojure.string/replace s #"HumanChromosome[XY\d]+Band" "")
     :default
     (throw
      (IllegalArgumentException.
       (str "Clean-up expects a BaseKaryotype, Chromosome or Band
       Class. Got:" clazz))))))

(defn- get-base [axiom]
  "Returns the base karyotype. AXIOM is of type derivedFrom."
  (clojure.string/replace (clean-up (.getFiller axiom)) #"_" ","))

(defn- addition-chromosome [chromosome]
  {:pre (true? (h/chromosome? chromosome))}
  "Returns a vector [chromosome and chromosome addition string] based
  on given CHROMOSOME."
  [(clean-up chromosome) (str "+" (clean-up chromosome))])

(defn- addition-band [band]
  {:pre (true? (h/band? band))}
  "Returns a vector [chromosome and band addition string] based on
given BAND."
  (let [chromosome (clean-up (e/get-chromosome band))]
    [chromosome (str "add(" chromosome ")(" (clean-up band) ")")]))

(defn- chrom-filter [axiom]
  "Returns HumanChromosome(s)S present in AXIOM."
  (filter #(h/chromosome? %) (.asConjunctSet axiom)))

(defn- band-filter [axiom]
  "Returns HumanChromosomeBand(s) present in AXIOM."
  (let [coll (.asConjunctSet axiom)
        axioms (filter #(instance?
               uk.ac.manchester.cs.owl.owlapi.OWLObjectSomeValuesFromImpl
               %) coll)]
    (for [a axioms
      :let [clazz (.getFiller a)]
      :when (h/band? clazz)]
      clazz)))

;; PUBLIC - used by affects1
(defn human-filter [axiom]
  "Returns HumanChromosome(s) and HumanChromosomeBand(s) present in
AXIOM."
  (flatten (merge '() (chrom-filter axiom) (band-filter axiom))))

(defn- addition [axiom]
  "Returns a vector [chromosome and addition string] based on given
AXIOM."
  (let [chrom_band (first (human-filter axiom))]
    (cond
     (h/chromosome? chrom_band)
     (addition-chromosome chrom_band)
     (h/band? chrom_band)
     (addition-band chrom_band)
     :default
     (throw
      (IllegalArgumentException.
       (str "Addition expects a Chromosome or Band
       Class. Got:" axiom))))))

(defn- deletion-chromosome [chromosome]
  {:pre (true? (h/chromosome? chromosome))}
  "Returns a vector [chromosome and chromosome deletion string] based
  on given AXIOM."
  [(clean-up chromosome) (str "-" (clean-up chromosome))])

(defn- deletion-band [chromosome bands]
  "Returns a vector [chromosome and band deletion string] based on
  given AXIOM."
  [chromosome (str "del(" chromosome ")(" bands ")")])

(defn- deletion-band-driver [bands]
  {:pre (true? (every? h/band? bands))}
  (let [adjusted (if (= (count bands) 1)
                   (repeat 2 (first bands)) bands)
        band-info (for [band adjusted
                        :let [string (clean-up band)]
                        :when (not (h/ter? band))]
                    string)]
    (deletion-band (clean-up (e/get-chromosome (first bands)))
                   (apply str band-info))))

(defn- deletion [axiom]
  "Returns a vector [chromosome and deletion string] based on given
AXIOM."
  (let [chrom_band (human-filter axiom)]
    (cond
     (h/chromosome? (first chrom_band))
     (deletion-chromosome (first chrom_band))
     (h/band? (first chrom_band))
     (deletion-band-driver chrom_band)
     :default
     (throw
      (IllegalArgumentException.
       (str "Deletion expects a Chromosome or Band
       Class. Got:" axiom))))))

(defn- get-event-string [event]
  "Returns the String representation of the event restriction.
EVENT is of type hasEvent."
  (let [axiom (.getFiller event)]
    (cond
     ;; "If axiom is an addition event"
     (.containsConjunct axiom e/Addition)
     (addition axiom)
     ;; "If axiom is a deletion event"
     (.containsConjunct axiom e/Deletion)
     (deletion axiom)
     :default
     (throw
      (IllegalArgumentException.
       (str "Get-event-string expects a valid event restriction. Got:"
       event))))))

(defn- get-axiom-string [entity]
  "Returns the string representation of the given ENTITY."
  (cond
   (instance?
    org.semanticweb.owlapi.model.OWLRestriction
    entity)
   (cond
    (= (.getProperty entity) b/derivedFrom)
    (get-base entity)
    (= (.getProperty entity) e/hasDirectEvent)
    (get-event-string entity)
    :default
    (throw
     (IllegalArgumentException.
      (str "Get-axiom-string expects a derivedFrom or hasDirectEvent
        restriction. Got:" (.getProperty entity)))))
   (instance?
    org.semanticweb.owlapi.model.OWLClassExpression
    entity)
   "IGNORE-ME"
   :default
   (throw
    (IllegalArgumentException.
     (str "Get-axiom-string expects an OWLClass or
            OWLRestriction. Got:" entity)))))

(defn chrom-sort [x y]
  "Comprator used to sort chromosomes in ISCN order - i.e. X,Y then
1-22."
  (cond
   (re-find #"X|Y" x)
   +1
   (re-find #"X|Y" y)
   -1
   (= (type x) (type y))
   (compare (read-string x) (read-string y))
   :default
   (throw
    (IllegalArgumentException.
     (str "Unknown input for chrom-sort. Got: " x " and " y)))))

(defn parse-karyotype-class [o clazz]
  "Returns the ISCN String of an OWL Karyotype Class.
CLAZZ is of type ISCNExampleKaryotype."
  (let [parents (direct-superclasses o clazz)
        strings (remove #{"IGNORE-ME"} (map get-axiom-string parents))
        sorted (sort-by first chrom-sort (rest strings))
        add (count (filter #(re-find #"\+" %) (map second sorted)))
        del (count (filter #(re-find #"\-" %) (map second sorted)))
        base-string (first (filter #(re-find #"\d+,\w" %) strings))
        incase (if (nil? base-string) "46,XX" base-string)
        base (clojure.string/split incase #",")
        total (- (+ (read-string (get base 0)) add) del)
        all (flatten (merge '() (map second sorted) (get base 1) total))
        ]
    (if (nil? base-string) "ERROR" (clojure.string/join "," all))))

(defn- create-karyotype-string0 [o detail name]
  "Prints details of the string input - used for testing purposes.
detail is of type boolean. NAME is of type String."
  (let [clazz (owl-class (make-safe name))]
    ;; If true, print the name, owl class, and resulting string.
    (if (true? detail)
      [(println (str "NAME: " name))
       (println (str "CLASS: " class))
       (println (str "STRING: " (parse-karyotype-class o clazz)))])))

;; TESTING
;; get ISCN String
(def ^{:doc "Partial function for creating a karyotype string. FALSE
  mean that the output is elided."} create-karyotype-string
  (partial create-karyotype-string0 parse false))

(create-karyotype-string "47,XX,+X")
(create-karyotype-string "45,X,-X")
(create-karyotype-string "45,X,-Y")
(create-karyotype-string "45,Y,-X")
(create-karyotype-string "26,X,+4,+6,+21")
(create-karyotype-string "71,XXX,+8,+10")
(create-karyotype-string "89,XXYY,-1,-3,-5,+8,-21")

(create-karyotype-string "46,XX,add(19)(p13.3)")
(create-karyotype-string "46,XY,add(12)(q13)")
(create-karyotype-string "46,XX,del(5)(q13)")
(create-karyotype-string "46,XX,del(5)(q13q13)")
(create-karyotype-string "69,XXX,del(7)(p11.2)")

(def ^{:doc "Partial function for creating a karyotype string. TRUE
  mean that the output is shown."} create-karyotype-string
  (partial create-karyotype-string0 parse true))

;; TODO
;; 1. Order of bands
;; (create-karyotype-string "46,XX,del(5)(q13q33)")

;; 2. Starting sex chromosome description i.e 45,X,-X not 45,XX,-X
;; (create-karyotype-string "46,Y,del(X)(p21p21)")

;; 3. Unknown chromosomes or chromosomes
;; (create-karyotype-string "46,XX,del(5)(q?)")

;; 4. Other event types