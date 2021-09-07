(in-package #:cl-automaton)

(defclass automaton ()())

(defgeneric states (automaton)
  (:documentation "Returns a list of all states of the AUTOMATON."))

(defgeneric add-state (automaton state &optional is-final-p))
(defgeneric has-state-p (automaton state))
(defgeneric advance-state (automaton event))
(defgeneric final-states (automaton))
(defgeneric mark-state-final (automaton state))
(defgeneric final-state-p (automaton state))
(defgeneric transitions (automaton))
(defgeneric add-transition (automaton from-state input to-state))

; --------------------??????

(defgeneric next-states (automaton state &optional input))





