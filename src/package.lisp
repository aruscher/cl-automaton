;;;; package.lisp

(defpackage #:cl-automaton
  (:use #:cl)
  (:export
   ;; protocol.lisp
   #:automaton
   #:states
   #:add-state
   #:has-state-p
   #:final-states
   #:mark-state-final
   #:final-state-p
   #:transitions
   #:add-transition))
