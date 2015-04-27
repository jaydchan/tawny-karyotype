;; %% The contents of this file are subject to the LGPL License, Version 3.0.

;; %% Copyright (C) 2012-2015, Newcastle University

;; %% This program is free software: you can redistribute it and/or modify
;; %% it under the terms of the GNU General Public License as published by
;; %% the Free Software Foundation, either version 3 of the License, or
;; %% (at your option) any later version.

;; %% This program is distributed in the hope that it will be useful,
;; %% but WITHOUT ANY WARRANTY; without even the implied warranty of
;; %% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; %% GNU General Public License for more details.

;; %% You should have received a copy of the GNU General Public License
;; %% along with this program. If not, see http://www.gnu.org/licenses/.

;; \documentclass{article}

;; %% Syntax highlighting for \cloj, \tawny & tawny-karyotype
;; \usepackage{style/clojure}
;; \usepackage{style/tawny}
;; \usepackage{style/karyotype}

;; %% coding environment for tawny code
;; \usepackage{listings}
;; \lstMakeShortInline[style=kstyle]|
;; \lstnewenvironment{code}[1][]%
;; {\lstset{style=kstyle,#1}}{}
;; \lstnewenvironment{kcode}[1][]%
;; {\lstset{style=kstyle,#1}}{}

;; %% Used for spacing in macros.
;; \usepackage{xspace}

;; %% For references
;; \usepackage[numbers,sort&compress]{natbib}

;; \begin{document}

;; \newcommand{\iscn}{ISCN\xspace}
;; \newcommand{\istring}{ISCN String\xspace}
;; \newcommand{\istrings}{ISCN Strings\xspace}
;; \newcommand{\ko}{The Karyotype Ontology\xspace}
;; \newcommand{\turners}{Tuners Syndrome\xspace}
;; \newcommand{\tawny}{Tawny-OWL\xspace}

;; \newcommand{\function}[1]{\color{blue}\texttt{#1}}
;; \newcommand{\todo}[1]{\textbf{TODO: #1}}

;; \section{What is an \istring?}
;; \label{sec:istring_subset}

;; This section provides a lenticular review of how \istrings
;; are defined by the specification and are modelled using the
;; \ko, by focusing on a subset of exemplars defined in the
;; \iscn.

;; \begin{code}
;; Define namespace
(ns ^{:doc "Defining example karyotypes from the ISCN2013."
      :author "Jennifer Warrender"}
  ncl.karyotype.iscnexamples_subset
  (:use [tawny.owl])
  (:require [ncl.karyotype
             [karyotype :as k]
             [human :as h]
             [events :as e]
             [base :as b]]))

;; Define ontology
(defontology iscnexamples_subset
  :iri
  "http://www.purl.org/captau/karyotype/iscnexamples_subset"
  :prefix "isubset:"
  :comment "Subset of the ISCN Example Karyotypes ontology
  for Human Karyotype Ontology, written using the Tanwy-OWL
  library.")

;; Import all karyotype axioms
(owl-import k/karyotype)

;; Create a new subclass of Karyotype
(defclass ISCNExampleKaryotype_subset
  :super k/Karyotype)
;; \end{code}

;; In \ko ``normal'' karyotypes for each ploidy level are
;; modelled in the |base| ontology; thus we import all
;; associated axioms into the current ontology.

;; \begin{code}
(owl-import b/base)
;; \end{code}

;; However, not all karyotypes are normal; they can include a
;; variety of abnormalities. There are two types of
;; abnormality. \emph{Numerical abnormalities} are
;; abnormalities that affect the number of chromosomes present
;; in the karyotype, either by gaining or losing whole
;; chromosomes. \emph{Structural abnormalities} are
;; abnormalities that involve only parts of the
;; chromosomes\footnote{For simplicity, structural
;;   abnormalities will not be discussed at this time.}.

;; In order to model karyotypes, we need concepts in the
;; ontology that model the human chromosomes and the numerical
;; abnormality events. These are modelled in the |human| and
;; |events| ontologies respectively; thus we import all axioms
;; from both.

;; \begin{code}
(owl-import e/events)
(owl-import h/human)
;; \end{code}

;; In the \iscn, numerical abnormalities are represented in the
;; \istring using symbols and abbreviated terms. For numerical
;; abnormalities, the symbol \textbf{-} is used to represent
;; the loss of chromosomes while \textbf{+} represents the gain
;; of chromosomes.

;; For example, the karyotype of a female individual that has
;; lost one chromosome |22| (and no other abnormalities) is
;; represented as |k45,XX,-22|~\cite[p.~57]{iscn12}; this
;; results in 45 chromosomes and monosomy (one copy of)
;; chromosome |22|.

;; In \ko, each karyotype is modelled by explicitly stating the
;; base karyotype and any abnormality events, using the
;; |b/derivedFrom| and |e/hasDirectEvent| relations
;; respectively. For this exemplar, the base karyotype is
;; |k/46,XX|, as the tumour originated from a female. In
;; addition, we model the |1| deletion abnormality using a
;; cardinality restriction and the |e/Deletion| and
;; |h/HumanChromosome22| classes.

;; \begin{code}
(defclass k45_XX_-22
  :label "The 45,XX,-22 karyotype"
  :comment "A karyotype with monosomy 22."
  :super ISCNExampleKaryotype_subset
  (owl-some b/derivedFrom b/k46_XX)
  (exactly 1 e/hasDirectEvent
             (owl-and e/Deletion
                      h/HumanChromosome22)))
;; \end{code}

;; Due to the programmatic nature of \tawny, we can implement
;; parameterised patterns~\cite{warrender-pattern}, thus
;; simplifying the deletion abnormality definition to one line
;; of code, using the |e/deletion| pattern.

;; \begin{code}
(defclass k45_XX_-22
  :label "The 45,XX,-22 karyotype"
  :comment "A karyotype with monosomy 22."
  :super ISCNExampleKaryotype_subset
  (owl-some b/derivedFrom b/k46_XX)
  (e/deletion 1 h/HumanChromosome22))
;; \end{code}

;; Similarly, the karyotype of a tumour from a female
;; individual that has lost one chromosome |X| (and no other
;; abnormalities) is represented as
;; |k45,X,-X|~\cite[p.~56]{iscn12}. In \ko, this karyotype is
;; modelled with the base karyotype |b/46,XX| and |1| deletion
;; event that involves |h/HumanChromosomeX|.

;; \begin{code}
(defclass k45_X_-X
  :label "The 45,X,-X karyotype"
  :comment "A tumor karyotype in a female with loss of one X
  chromosome."
  :super ISCNExampleKaryotype_subset
  (owl-some b/derivedFrom b/k46_XX)
  (e/deletion 1 h/HumanChromosomeX))
;; \end{code}

;; However, the classification of abnormalities is not so
;; simple; an abnormality can be also classified as either a
;; \textit{constitutional} or \textit{acquired}
;; abnormality\footnote{All previous exemplars define acquired
;;   abnormalities.}. A constitutional abnormality, also known
;; as an in-born abnormality, is an abnormality that is present
;; in (almost) all cells of an individual and exists at the
;; earliest stages of embryogenesis, while an acquired
;; abnormality is an abnormality that develops in somatic
;; cells~\cite{constitutional}.

;; Generally, constitutional abnormalities are indicated using
;; the suffix \textbf{c}. For example the \istring
;; |46,XY,+21c,-21|~\cite[p.~58]{iscn12} represents the
;; karyotype of tumour cells taken from a male individual, that
;; had a constitutional trisomy |21| and has acquired disomy
;; |21|. Using this representation we see that karyotypes with
;; constitutional abnormalities explicitly define two types of
;; canonicalisation; one of the individual and the other for
;; the cell line they have given rise to.

;; In \ko, constitutional abnormalities are also modelled
;; explicitly using the |e/hasDirectEvent| relation. However
;; unlike acquired abnormalities, constitutional abnormalities
;; are modelled as a nested restriction in conjunction with the
;; base karyotype. In this exemplar:
;; \begin{itemize}
;; \item the base karyotype is |b/46,XY| (as the karyotype
;;   originates from a male individual).
;; \item the |1| constitutional abnormality is a gain of one
;;   chromosome |21|. The associated parameterised pattern for
;;   gain is |e/addition|.
;; \item the |1| acquired abnormality is a loss of one
;;   chromosome |21|.
;; \end{itemize}

;; \begin{code}
(defclass k46_XY_+21c_-21
  :label "The 46,XY,+21c,-21 karyotype"
  :comment "Acquired loss of one chromosome 21 in a patient
  with Down syndrome."
  :super ISCNExampleKaryotype_subset
  ;;aka 47,XY,+21
  (owl-some b/derivedFrom
           (owl-and
            (owl-some b/derivedFrom b/k46_XY)
            (e/addition 1 h/HumanChromosome21)))
  (e/deletion 1 h/HumanChromosome21))
;; \end{code}

;; However, constitutional sex chromosome numerical
;; abnormalities are more complex still. Instead of using the
;; \textbf{+} and \textbf{-} symbols to indicate numerical
;; abnormalities, these constitutional sex chromosome
;; abnormalities are included in the initial \istring sex
;; description. For example, the karyotype for an individual
;; born with \turners (and no other abnormalities) is
;; represented as |45,X|~\cite[p.~56]{iscn12}: a female
;; individual that has 45 chromosomes and monosomy X (only one
;; X chromosome)\footnote{Note that the definition is very
;;   similar to \texttt{45,X,-X}.}.

;; \vfill\eject %% pushes code onto next page

;; \begin{code}
 (defclass k45_X
   :label "The 45,X karyotype"
   :comment "A karyotype with one X chromosome (Turner
   syndrome)."
   :super ISCNExampleKaryotype_subset
   (owl-some b/derivedFrom
             (owl-and
              (owl-some b/derivedFrom b/k46_XN)
              (e/deletion 1 h/HumanSexChromosome))))
;; \end{code}

;; With the \textbf{c} suffix, acquired chromosome
;; abnormalities in individuals with a constitutional sex
;; chromosome abnormality can easily be distinguished. For
;; example the \istring |46,Xc,+21|~\cite[p.~57]{iscn12}
;; represents tumour cells taken from a female individual with
;; \turners; a constitutional monosomy X and an acquired
;; trisomy 21.

;; \begin{code}
(defclass k46_Xc_+21
  :label "The 46,Xc,+21 karyotype"
  :comment "Tumor cells with an acquired extra chromosome 21
  in a patient with Turner syndrome."
  :super ISCNExampleKaryotype_subset
  ;;aka 45,X
  (owl-some b/derivedFrom
            (owl-and
             (owl-some b/derivedFrom b/k46_XN)
             (e/deletion 1 h/HumanSexChromosome)))
  (e/addition 1 h/HumanChromosome21))
;; \end{code}

;; \begin{code}
(as-disjoint k45_XX_-22 k45_X_-X  k46_XY_+21c_-21 k45_X k46_Xc_+21)
;; \end{code}

;; Now that we defined a few exemplar karyotypes, we discuss
;; the definition of sex.

;; \subsection{Defining Sex}
;; \label{sec:sex}

;; While building this ontology, we found that sex is not as
;; intuitive as it seems. The obvious definition for sex was
;; that a ``male'' karyotype should be defined as a karyotype
;; with a |Y| chromosome, while a ``female'' karyotype as one
;; without. However further investigation showed that these
;; definitions are, in fact, too simplistic as the karyotype
;; |45,X,-Y|\footnote{A male-derived cell line which has lost
;;   its \texttt{Y} chromosome.}, has no |Y| chromosome, yet
;; would generally be considered to be a ``male'' karyotype.

;; Therefore, the finalised definition for sex, as shown below
;; considers the history of the karyotype by asserting a
;; |derivedFrom| relation\footnote{Due to the transitive
;;   property of \texttt{b/derivedFrom}, we can also determine
;;   the sex of karyotypes that contain constitutional
;;   abnormalities.}. Using these definitions, the |45,X,-Y|
;; karyotype can be correctly stated as being a ``male''
;; karyotype.

;; \vfill\eject

;; \begin{code}
(defclass MaleKaryotype
  :equivalent
  (owl-or
   b/k46_XY
   (owl-some b/derivedFrom b/k46_XY)))
;; \end{code}

;; \begin{code}
(defclass FemaleKaryotype
  :equivalent
  (owl-or
   b/k46_XX
   (owl-some b/derivedFrom b/k46_XX)))
;; \end{code}

;; However these definitions are unable to ontologically
;; categorise the |45,X| karyotype as either female or male
;; though it would generally be considered a ``female''
;; karyotype. There is no correct answer to this problem. We
;; could either redefine our female karyotype to include the
;; |45,X| karyotype or add phenotypic sex. This decision needs
;; to be taken by the domain experts themselves.

;; %% --------------------------------------------------------------------------
;; %% Bibliography

;; %% Can also change name bibliography of section
;; \renewcommand{\bibname}{References}

;; \bibliographystyle{newPlain} %other options: agsm, ieeetr
;; %% \begin{singlespace}
;; \pagestyle{plain}
;; \bibliography{bibliography}
;; %% \end{singlespace}

;; \end{document}
