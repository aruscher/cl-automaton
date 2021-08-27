(in-package #:cl-automaton)

(defclass automaton ()
  ())

(defgeneric states (automaton))
(defgeneric add-state (automaton state))
(defgeneric next-states (automaton state))
(defgeneric transitions (automaton))
(defgeneric add-transition (automaton transition))

(defgeneric to-dot (automaton &optional stream))

#|
transducer = eingabe -> augabe
detectors = eingabe -> yes/no
|#
