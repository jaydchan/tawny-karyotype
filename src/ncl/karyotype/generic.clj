;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2014, Newcastle University

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

(ns ^{:doc "TODO"
      :author "Jennifer Warrender"}
  ncl.karyotype.generic
  (:use [tawny.owl :exclude [save-ontology]]
        [clojure.java.shell :only [sh]]
        [clojure.java.io :only [as-file reader]]))

(defonce output-file-path "./output/")

(if (not (.exists (clojure.java.io/as-file output-file-path)))
  (sh "mkdir" "-p" output-file-path))

(defn save-ontology
  "'Overloads' save-ontology function."
  [o name type]
  (if (not (.exists (clojure.java.io/as-file output-file-path)))
    (sh "mkdir" "-p" output-file-path))
  (tawny.owl/save-ontology o (str output-file-path name) type))

(defn output
  "APPENDs STRING to OUTPUT-FILE unless there is an ERROR"
  [output-file string append error]
  (try
    (spit output-file
          string
          :append append)
    (catch
        Exception exp (println error exp))))

(defn- get-lines
  "Reads in FILE line by line. Returns a java.long.Cons"
  [file]
  (with-open [r (reader file)]
    (doall (line-seq r))))

(defn read-file
  "Reads in file line by line as Clojure atoms. Returns a LazySeq."
  [file]
  (for [r (get-lines file)] (read-string r)))

(defn shorten
  "Removes the prefix of STRING"
  [string]
  (last (clojure.string/split string #"#")))

(defn get-entity-short-string
  [entity]
  (shorten (str (.getIRI entity))))