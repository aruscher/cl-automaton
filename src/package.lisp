;;;; package.lisp

(defpackage #:cl-automaton
  (:use #:cl)
  (:export
   ;; protocol.lisp
   #:automaton
   #:current-state
   #:init-state
   #:states
   #:add-state
   #:has-state-p
   #:final-states
   #:mark-state-final
   #:final-state-p
   #:transitions
   #:add-transition
   ;; finite-state-machine.lisp
   #:finite-state-machine))
