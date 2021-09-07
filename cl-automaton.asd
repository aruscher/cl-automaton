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
	       (:file "finite-state-machine")
	       (:file "export")
               (:file "cl-automaton"))
  :in-order-to ((test-op (test-op "cl-automaton/tests"))))

(asdf:defsystem #:cl-automaton/tests
  :serial t
  :pathname "t/"
  :depends-on ("fiveam" "cl-automaton")
  :components ((:file "package")
	       (:file "main-suite")
	       (:file "protocol-tests"))
  :perform (test-op (op c)
		   (uiop:symbol-call :cl-automaton/tests :run-tests)))

