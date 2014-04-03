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

(ns ncl.karyotype.core_test
  (:use [clojure.test]
        [clojure.java.shell :only [sh]]
        [clojure.string :only [trim-newline]])
  (:require
   [ncl.karyotype.core :as c]))

(deftest Basic
  ;; LEIN VERSION
  (is (= "Leiningen 2.3.4 on Java 1.7.0_51 OpenJDK Client VM"
         (trim-newline (:out (sh "lein" "version")))))
  (println (str "Lein version EXPECTED 2.3.4 ACTUAL"
                (trim-newline (:out (sh "lein" "version")))))

  ;; CLOJURE VERSION
  (is (= "1.6.0" (clojure-version)))
  (println (str "Clojure version: EXPECTED 1.6.0 ACTUAL " (clojure-version)))

  ;; http://stackoverflow.com/questions/5103121/how-to-find-the-jvm-version-from-a-java-program
  ;; http://docs.oracle.com/javase/6/docs/api/java/lang/System.html#getProperties%28%29
  ;; JAVA VERSION
  (is (= "1.7" (System/getProperty "java.specification.version")))
  (println (str "Java specification version: EXPECTED 1.7 ACTUAL "
                (System/getProperty "java.specification.version")))
  (println (str "Java version " (System/getProperty "java.version")))
  (println (str "Java VM version " (System/getProperty "java.vm.version")))
  (println (str "Java runtime version "
                (System/getProperty "java.runtime.version")))
)
