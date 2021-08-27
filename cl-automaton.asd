;;;; cl-automaton.asd

(asdf:defsystem #:cl-automaton
  :description "Describe cl-automaton here"
  :author "Andreas Ruscheinski <andreas.ruscheinski@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "cl-automaton")))
