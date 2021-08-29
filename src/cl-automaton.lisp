;;;; cl-automaton.lisp

(in-package #:cl-automaton)


(let ((automaton (make-instance 'automaton)))
  (add-state automaton 'foo)
  (describe automaton))
