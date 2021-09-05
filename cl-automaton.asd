;;;; cl-automaton.asd

(asdf:defsystem #:cl-automaton
  :description "Describe cl-automaton here"
  :author "Andreas Ruscheinski <andreas.ruscheinski@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src/"
  :depends-on ("alexandria")
  :components ((:file "package")
	       (:file "protocol")
	       (:file "export")
               (:file "cl-automaton")))


